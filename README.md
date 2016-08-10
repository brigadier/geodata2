## Erlang OTP application for handling MaxMind(tm) Geoip2 (.mmdb) files.

#### Features:
* Supports all types (hopefully) of bases in .mmdb format, both IPv4 and IPv6 ones;
* Multiple pools, each with its' own file and settings;
* Start/stop pools dynamically or from the application `env`;
* Access to each opened file by atom;
* Allows reloading files and replacing files in each pool without inerrupting or slowing down requests stream;
* Safe from binary reference leakage, binary parts are getting copied.
* Optional Schema for each pool for transforming deep lists into simple map



#### Note:
* Uses [Simplepool](https://github.com/brigadier/simplepool) pools. You might not like it as
`simplepool` uses quite unconventional thing - it compiles pool proc names and other data in a RAM beam module.
* v 1.0.0 is incompatible with earlier versions in every possible way.

Build
-----

    $ rebar3 compile


* The app accepts IPs in `{B3:8, B2:8, B1:8, B0:8}`, `{W7:16, W6:16, W5:16, W4:16, W3:16, W2:16, W1:16, W0:16}`
and big-endian dword formats.
* Schema parameter could be either `undefined` - IP info gets returned as deep proplist; `map` - the proplist
is converted to map (only topmost one); [{Name, Path, Default}, ...] where Name - name of the key
in the result map, Path - path to the value as binary or list of binaries, Default - default value
if there's no this fild for this IP. While it is possible to send schema with each lookup, it is better to specify
the schema at the time of the creation of a pool. Invalid schema would result in crash. See examples in the tests.


#### Example:
```erlang
CityFile = "/path/to/GeoIP2-City-Test.mmdb".
ok = geodata2:start_pool(dynpool, [
    {size, 2},
    {sup_flags, {one_for_all, 1, 5}}
], CityFile, [
    ?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
    ?GPATH(city, [<<"city">>, <<"names">>, <<"en">>], <<>>),
    ?GPATH(anonproxy, [<<"traits">>, <<"is_anonymous_proxy">>], false),
    ?GPATH(postal, [<<"postal">>, <<"code">>], <<>>),
    ?GPATH(country, [<<"country">>, <<"names">>, <<"en">>], <<>>)
]).
{ok, CityFile} = geodata2:state(dynpool).
{ok, #{city := <<"Boxford">>,
    iso_code := <<"GB">>,
    anonproxy := false,
    postal := <<"OX1">>,
    country := <<"United Kingdom">>
}} = geodata2:lookup(dynpool, {2, 125, 160, 216}).
{ok, #{iso_code := <<"GB">>,
    no_exists := noexists
}} = geodata2:lookup(dynpool, {2, 125, 160, 216}, [
    ?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
    ?GPATH(no_exists, [<<"no">>, <<"exists">>, <<"at all">>], noexists)
]).
ok = geodata2:reload_base(dynpool, undefined).
not_loaded = geodata2:lookup(dynpool, {2, 125, 160, 216}),
ok = geodata2:reload_base(dynpool, CityFile).
{ok, #{iso_code := <<"GB">>,
    no_exists := noexists
}} = geodata2:lookup(dynpool, {2, 125, 160, 216}, [
    ?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
    ?GPATH(no_exists, [<<"no">>, <<"exists">>, <<"at all">>], noexists)
]).
ok = geodata2:reload_base(dynpool).
{ok, #{iso_code := <<"GB">>,
    no_exists := noexists
}} = geodata2:lookup(dynpool, {2, 125, 160, 216}, [
    ?GPATH(iso_code, [<<"country">>, <<"iso_code">>], <<>>),
    ?GPATH(no_exists, [<<"no">>, <<"exists">>, <<"at all">>], noexists)
]).
```

See more examples in tests


Tests
-----

    $ rebar3 ct

Tests use some demo mmdb databases checked out from MaxMind github repo. The databases are licensed under
Creative Commons Attribution-ShareAlike 3.0 license and contain just a few IP ranges.


*A MaxMind repo with a number of test bases: [MaxMind-DB](https://github.com/maxmind/MaxMind-DB/)*


