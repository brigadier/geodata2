-module(geodata2_format).

-include("geodata2.hrl").

-define(GEO_EXTENDED, 0).
-define(GEO_POINTER, 1).
-define(GEO_UTF, 2).
-define(GEO_DOUBLE, 3).
-define(GEO_BYTES, 4).
-define(GEO_MAP, 7).
-define(GEO_UINT16, 5).
-define(GEO_UINT32, 6).
-define(GEO_SIGNEDINT32, 8).
-define(GEO_UINT64, 9).
-define(GEO_UINT128, 10).
-define(EX_GEO_ARRAY, 11).
-define(EX_GEO_DATACACHE, 12).
-define(EX_GEO_END, 13).
-define(EX_GEO_BOOL, 14).
-define(EX_GEO_FLOAT, 15).

%% API
-export([parse/2, unpack/2, meta/1, lookup/4]).

meta(Data) ->
	Size = byte_size(Data),
	{ok, RawMeta} = read_meta(Data, Size - 14),
	{ok, Meta} = geodata2_format:unpack(undefined, RawMeta), %%'undefined' is safe as there are no pointers in meta
	make_meta(Data, Meta).

make_meta(Data, Meta) ->
	IPVersion = proplists:get_value(<<"ip_version">>, Meta),
	RecordSize = proplists:get_value(<<"record_size">>, Meta),
	NodeCount = proplists:get_value(<<"node_count">>, Meta),
	Descr = case proplists:get_value(<<"description">>, Meta, []) of
				[] -> <<"Unknown">>;
				DescrMap ->
					case proplists:get_value(<<"en">>, DescrMap) of
						undefined ->
							[{_, Val} | _Rest] = DescrMap,
							Val;
						Val -> Val
					end
			end,
	Languages = proplists:get_value(<<"languages">>, Meta, []),
	DBType = proplists:get_value(<<"database_type">>, Meta),
	Timestamp = calendar:gregorian_seconds_to_datetime(62167219200 + proplists:get_value(<<"build_epoch">>, Meta)), %%{{1970,1,1},{0,0,0}} + epoch,
	Vsn = {
		proplists:get_value(<<"binary_format_major_version">>, Meta),
		proplists:get_value(<<"binary_format_minor_version">>, Meta)
	},
	true = (IPVersion == ?IPV6) orelse (IPVersion == ?IPV4),
	true = RecordSize =/= undefined,
	true = NodeCount =/= undefined,
	true = DBType =/= undefined,

	TreeSize = ((RecordSize * 2) div 8) * NodeCount,
	MetaRec = #meta{
		ip_version = IPVersion, %%v4 or v6
		record_size = RecordSize, %%bits
		node_count = NodeCount, %%number
		descr = Descr,
		languages = Languages,
		database_type = DBType,
		timestamp = Timestamp,
		vsn = Vsn,
		whole = RecordSize div 8 * 8, %%bits
		remdr = RecordSize rem 8, %% bits
		tree_size = TreeSize, %%bytes
		data_start = TreeSize + 16 %%bytes
	},
	V4Start = v4_tree_start(Data, MetaRec), %%optimization for ipv4 lookups in ipv6 database - begin search for IPV4s from the position of ::ffff:0:0/96 in the tree, saves 96 movements on each lookup
	{ok, MetaRec#meta{v4_start = V4Start}}.


read_meta(_Data, Pos) when Pos =< 0 ->
	error(<<"Meta not found.">>);

read_meta(Data, Pos) ->
	case Data of
		<<_:Pos/binary, 16#ab, 16#cd, 16#ef, "MaxMind.com", RawMeta/binary>> -> {ok, RawMeta};
		_ -> read_meta(Data, Pos - 1)
	end.

%%optimization for ipv4 lookups in ipv6 database - begin search for IPV4s from the position of ::ffff:0:0/96 in the tree, saves 96 movements on each lookup
v4_tree_start(_Data, #meta{ip_version = ?IPV4}) ->
	0;
v4_tree_start(Data, #meta{ip_version = ?IPV6} = Meta) ->
	Bits = <<0:80, 16#FFFF:16>>,
	{error, {partial, Pos}} = lookup(Meta, Data, Bits, ?IPV6),
	Pos.

lookup(#meta{ip_version = V} = Meta, Data, Bits, V) -> %%same version of IPs
	lookup_pos(Meta, Data, Bits, 0);
lookup(#meta{ip_version = ?IPV6, v4_start = V4Start} = Meta, Data, Bits, ?IPV4) -> %%lookup v4 in v6 db
	lookup_pos(Meta, Data, Bits, V4Start);
lookup(_, _, _, _) -> %%lookup v6 in v4 db
	{error, v4db}.

lookup_pos(#meta{node_count = NodeCount}, _, <<>>, Pos) when Pos < NodeCount -> %% can't happen in the real db, gets used for lookup of v4 tree start
	{error, {partial, Pos}};


lookup_pos(#meta{node_count = NodeCount,
				 record_size = RecordSize,
				 remdr = Remdr,
				 whole = Whole} = Meta, Data, <<LR:1, Bits/bits>>, Pos) when Pos < NodeCount ->
	Offset = Pos * RecordSize * 2,
	lookup_pos(Meta, Data, Bits, lr(LR, Offset, Whole, Remdr, RecordSize, Data));

lookup_pos(#meta{node_count = NodeCount}, _, _Bits, NodeCount) ->
	not_found;

lookup_pos(#meta{node_count = NodeCount, tree_size = TreeSize} = Meta, Data, _, Pointer) when Pointer > NodeCount ->
	Offset = Pointer + TreeSize - NodeCount,
	{ok, parse_ptr({Meta, Data}, Offset)}.

lr(0, Offset, Whole, Remdr, RecordSize, Data) ->
	<<_:Offset, LeftL:Whole/integer, LeftH:Remdr/integer, _Right:RecordSize, _/binary>> = Data,
	LeftL bor (LeftH bsl Whole);
lr(1, Offset, _Whole, _Remdr, RecordSize, Data) ->
	<<_:Offset, _Left:RecordSize, Right:RecordSize/integer, _/binary>> = Data,
	Right.

unpack(MD, Segment) ->
	{Result, _} = parse(MD, Segment),
	{ok, Result}.

parse(MD, <<?GEO_EXTENDED:3, Sz:5, Type:8, Segment/binary>>) when Sz < 29 ->
	parse_sz(MD, Type + 7, Sz, Segment);

parse(MD, <<?GEO_EXTENDED:3, 29:5, Type:8, Sz1:8/unsigned-integer, Segment/binary>>) ->
	parse_sz(MD, Type + 7, Sz1 + 29, Segment);

parse(MD, <<?GEO_EXTENDED:3, 30:5, Type:8, Sz1:16/unsigned-integer, Segment/binary>>) ->
	parse_sz(MD, Type + 7, Sz1 + 285, Segment);

parse(MD, <<?GEO_EXTENDED:3, 31:5, Type:8, Sz1:24/unsigned-integer, Segment/binary>>) ->
	parse_sz(MD, Type + 7, Sz1 + 65821, Segment);


parse(_MD, <<?GEO_DOUBLE:3, 8:5, F:64/float, Rest/binary>>) ->
	{F, Rest};


parse({#meta{data_start = DS}, _} = MD, <<?GEO_POINTER:3, 0:2, P:11/unsigned-integer, Rest/binary>>) ->
	{parse_ptr(MD, P + DS), Rest};

parse({#meta{data_start = DS}, _} = MD, <<?GEO_POINTER:3, 1:2, P:19/unsigned-integer, Rest/binary>>) ->
	{parse_ptr(MD, P + 2048 + DS), Rest};

parse({#meta{data_start = DS}, _} = MD, <<?GEO_POINTER:3, 2:2, P:27/unsigned-integer, Rest/binary>>) ->
	{parse_ptr(MD, P + 526336 + DS), Rest};

parse({#meta{data_start = DS}, _} = MD, <<?GEO_POINTER:3, 3:2, _VVV:3, P:32, Rest/binary>>) ->
	{parse_ptr(MD, P + DS), Rest};



parse(MD, <<Type:3, Sz:5, Segment/binary>>) when Sz < 29 ->
	parse_sz(MD, Type, Sz, Segment);

parse(MD, <<Type:3, 29:5, Sz1:8, Segment/binary>>) ->
	parse_sz(MD, Type, Sz1 + 29, Segment);

parse(MD, <<Type:3, 30:5, Sz1:16, Segment/binary>>) ->
	parse_sz(MD, Type, Sz1 + 285, Segment);

parse(MD, <<Type:3, 31:5, Sz1:24, Segment/binary>>) ->
	parse_sz(MD, Type, Sz1 + 65821, Segment).

parse_ptr({_, Data} = MD, Offset) ->
	<<_:Offset/binary, Segment/binary>> = Data,
	{Result, _} = parse(MD, Segment),
	Result.

parse_sz(_MD, ?GEO_UTF, Size, Segment) ->
	<<Str:Size/binary, Rest/binary>> = Segment,
	{?BINARY_COPY(Str), Rest};

parse_sz(_MD, ?GEO_BYTES, Size, Segment) ->
	<<Bytes:Size/binary, Rest/binary>> = Segment,
	{?BINARY_COPY(Bytes), Rest};

parse_sz(_MD, Type, Bytes, Segment) when Type == ?GEO_UINT16; Type == ?GEO_UINT32; Type == ?GEO_UINT64; Type == ?GEO_UINT128 ->
	<<Val:Bytes/unsigned-integer-unit:8, Rest/binary>> = Segment,
	{Val, Rest};

parse_sz(_MD, ?GEO_SIGNEDINT32, Bytes, Segment) ->
	<<Val:Bytes/signed-integer-unit:8, Rest/binary>> = Segment,
	{Val, Rest};

parse_sz(MD, ?GEO_MAP, Size, Segment) ->
	parse_map(MD, Segment, Size, []);

parse_sz(MD, ?EX_GEO_ARRAY, Size, Segment) ->
	parse_array(MD, Segment, Size, []);

parse_sz(_MD, ?EX_GEO_DATACACHE, Size, Segment) ->
	<<Cache:Size/binary, Rest/binary>> = Segment,
	{?BINARY_COPY(Cache), Rest};

parse_sz(_MD, ?EX_GEO_FLOAT, 4, Segment) ->
	<<Val:32/float, Rest/binary>> = Segment,
	{Val, Rest};

parse_sz(_MD, ?EX_GEO_END, 0, Segment) ->
	{eos, Segment};

parse_sz(_MD, ?EX_GEO_BOOL, 1, Segment) ->
	{true, Segment};

parse_sz(_MD, ?EX_GEO_BOOL, 0, Segment) ->
	{false, Segment}.


parse_array(_MD, Segment, 0, Acc) ->
	{lists:reverse(Acc), Segment};

parse_array(MD, Segment, N, Acc) ->
	{Elem, Rest} = parse(MD, Segment),
	parse_array(MD, Rest, N - 1, [Elem | Acc]).



parse_map(_MD, Segment, 0, Acc) ->
	{Acc, Segment};

parse_map(MD, Segment, N, Acc) ->
	{Key, Rest1} = parse(MD, Segment),
	{Val, Rest2} = parse(MD, Rest1),
	parse_map(MD, Rest2, N - 1, [{Key, Val} | Acc]).