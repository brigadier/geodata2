-module(geodata2_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("geodata2.hrl").

%% API
-compile(export_all).

groups() -> [{main, [], [
	testgeo
	, loadtest1M
]}].

all() -> [{group, main}].

loadtest1M(_Config) -> %%1M read ops, 1000 processes, 8 workers
	erlang:process_flag(trap_exit, true),
	rand:seed(exs64),
	L = lists:seq(1, 1000),
	?debugVal(calendar:local_time()),
	lists:foreach(
		fun(_) ->
			spawn_link(
				fun() ->
					lists:foreach(
						fun(_) ->
							IP = {rand:uniform(255), rand:uniform(255), rand:uniform(255), rand:uniform(255)},
							R = geodata2:lookup(countrypool, IP),
							true = is_map(R) orelse R == not_found
						end,
						L
					)
				end
			)
		end,
		L
	),


	lists:foreach(
		fun(_) ->
			receive
				{'EXIT', _, normal} -> ok;
				X -> exit(X)
			end
		end,
		L

	),
	?debugVal(calendar:local_time()),
	ok.

testgeo(Config) ->
	not_found = geodata2:lookup(isppool, {192, 168, 1, 1}),
	{ok, #{<<"autonomous_system_number">> := 1221,
		<<"autonomous_system_organization">> := <<"Telstra Pty Ltd">>,
		<<"isp">> := <<"Telstra Internet">>,
		<<"organization">> := <<"Telstra Internet">>}} = geodata2:lookup(isppool, {1, 128, 1, 10}, map),

	{ok, #{asn := 1221}} = geodata2:lookup(isppool, {1, 128, 1, 10},
		[?GPATH(asn, <<"autonomous_system_number">>, undefined)]),
	{ok, #{asn := 1221}} = geodata2:lookup(isppool, {1, 128, 1, 10},
		[?GPATH(asn, [<<"autonomous_system_number">>], undefined)]),
	{ok, #{abc := nope}} = geodata2:lookup(isppool, {1, 128, 1, 10},
		[?GPATH(abc, [<<"abc">>], nope)]),

	{ok, [{<<"connection_type">> , <<"Dialup">>}]} = geodata2:lookup(ctpool, {1, 0, 128, 1}),
	{ok, #{country := <<"Philippines">>,
		iso_code := <<"PH">>,
		lat := 13.0,
		long := 122.0,
		postal := <<"34021">>,
		represented_country_iso := <<"US">>,
		time_zone := <<"Asia/Manila">>}} = geodata2:lookup(countrypool, {202, 196, 224, 1}),


	{ok, #{anonproxy := true, iso_code := <<"BT">>, sat := false}} = geodata2:lookup(countrypool, {67, 43, 156, 0},
		[
			?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
			?GPATH(anonproxy, [<<"traits">>, <<"is_anonymous_proxy">>], false),
			?GPATH(sat, [<<"traits">>, <<"is_satellite_provider">>], false)
		]),

	DataDir = ?config(data_dir, Config),
	CityFile = filename:join(DataDir, "GeoIP2-City-Test.mmdb"),

	ok = geodata2:start_pool(dynpool, [
		{size, 2},
		{sup_flags, {one_for_all, 1, 5}}
	], CityFile, [
		?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
		?GPATH(city, [<<"city">>, <<"names">>, <<"en">>], <<>>),
		?GPATH(anonproxy, [<<"traits">>, <<"is_anonymous_proxy">>], false),
		?GPATH(postal, [<<"postal">>, <<"code">>], <<>>),
		?GPATH(country, [<<"country">>, <<"names">>, <<"ru">>], <<>>)


	]),


	{ok, CityFile, #mmdbmeta{}} = geodata2:state(dynpool),
	R1 = #{city => <<"Boxford">>,
		iso_code  => <<"GB">>,
		anonproxy  => false,
		postal  => <<"OX1">>,
		country  => <<"Великобритания"/utf8>>
	},
	{ok, R1} = geodata2:lookup(dynpool, {2, 125, 160, 216}),

	undefined = geodata2:reload_base(dynpool, undefined),
	not_loaded = geodata2:lookup(dynpool, {2, 125, 160, 216}),
	error = geodata2:reload_base(dynpool, "no_exists"),
	not_loaded = geodata2:lookup(dynpool, {2, 125, 160, 216}),

	ok = geodata2:reload_base(dynpool, CityFile),
	{ok, R1} = geodata2:lookup(dynpool, {2, 125, 160, 216}),
	ok = geodata2:reload_base(dynpool),
	{ok, R1} = geodata2:lookup(dynpool, {2, 125, 160, 216}),
	case catch geodata2:lookup(dynpool, {2, 125, 160, 216}, [{bad, schema}]) of
		{'EXIT', _} -> timer:sleep(100), ok
	end,
	ok = geodata2:start_pool(dynpool2, [
		{size, 2},
		{sup_flags, {one_for_all, 1, 5}}
	], undefined, undefined),
	{ok, undefined, undefined} = geodata2:state(dynpool2),

	ok = geodata2:start_pool(dynpool3, [
		{size, 2},
		{sup_flags, {one_for_all, 1, 5}}
	], "not exists.dat", map),
	{error, "not exists.dat"} = geodata2:state(dynpool3),

	ok.


init_per_suite(Config) ->
	DataDir = ?config(data_dir, Config),
	CTFile = filename:join(DataDir, "GeoIP2-Connection-Type-Test.mmdb"),
	ISPFile = filename:join(DataDir, "GeoIP2-ISP-Test.mmdb"),
	CityFile = filename:join(DataDir, "GeoIP2-City-Test.mmdb"),


	application:load(geodata2),
	application:set_env(
		geodata2,
		pools,
		[
			{
				ctpool, [
				[
					{size, 2},
					{sup_flags, {one_for_all, 1, 5}}
				],
				[
					{file, CTFile}

				]]
			},
			{
				isppool, [
				[
					{size, 2},
					{sup_flags, {one_for_all, 1, 5}}
				],
				[
					{file, ISPFile}
				]]
			},
			{
				countrypool, [
				[
					{size, 8},
					{sup_flags, {one_for_all, 1, 5}}
				],
				[
					{file, CityFile},
					{schema, [
						?GPATH(long, [<<"location">>, <<"longitude">>], undefined),
						?GPATH(lat, [<<"location">>, <<"latitude">>], undefined),
						?GPATH(time_zone, [<<"location">>, <<"time_zone">>], undefined),
						?GPATH(country, [<<"country">>, <<"names">>, <<"en">>], <<>>),
						?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
						?GPATH(postal, [<<"postal">>, <<"code">>], <<>>),
						?GPATH(represented_country_iso, [<<"represented_country">>, <<"iso_code">>], undefined)
					]}
				]]
			}
		]
	),

	geodata2:start(),
	Config.

end_per_suite(_Config) ->
	application:stop(geodata2),
	application:stop(simplepool),
	ok.



init_per_group(_GroupName, Config) ->
	Config.


end_per_group(_GroupName, _Config) ->
	ok.


init_per_testcase(_TestCase, Config) ->
	Config.

end_per_testcase(_TestCase, _Config) ->
	ok.
