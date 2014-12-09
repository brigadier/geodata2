-module(geodata2_ip).

-include("geodata2.hrl").
%% API
-export([make_ip/1]).

make_ip({B3, B2, B1, B0}) when is_integer(B0), is_integer(B1), is_integer(B2), is_integer(B3) ->
	{ok, <<B3:8, B2:8, B1:8, B0:8>>, ?IPV4};

make_ip(IP) when is_integer(IP), IP =< 16#FFFFFFFF ->
	{ok, <<IP:32>>, ?IPV4};

make_ip({W7, W6, W5, W4, W3, W2, W1, W0}) when is_integer(W0), is_integer(W1), is_integer(W2), is_integer(W3), is_integer(W4), is_integer(W5), is_integer(W6), is_integer(W7) ->
	{ok, <<W7:16, W6:16, W5:16, W4:16, W3:16, W2:16, W1:16, W0:16>>, ?IPV6};

make_ip(IP) when is_integer(IP) ->
	{ok, <<IP:128/integer>>, ?IPV6};

make_ip(_) ->
	{error, format}.