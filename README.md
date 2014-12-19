## Erlang OTP application for handling MaxMind(tm) Geoip2 (.mmdb) files.

#### Features:
* Supports all types (hopefully) of bases in .mmdb format, both IPv4 and IPv6 ones;
* Allows opening multiple files;
* Access to each opened file by atom;
* Uses access pool for each of the file, by default 4 workers per file. At the moment this number can be changed in compile time only;
* Allows reloading files and replacing files in each pool without inerrupting or slowing down requests stream;
* Safe from binary reference leakage, binary parts are getting copied.
* R15 compatible


The app accepts IPs in `{B3:8, B2:8, B1:8, B0:8}`, `{W7:16, W6:16, W5:16, W4:16, W3:16, W2:16, W1:16, W0:16}` and big-endian dword formats.


#### Example:

Download and unzip GeoLite2-City.mmdb and GeoLite2-Country.mmdb from [MaxMind](http://dev.maxmind.com/geoip/geoip2/geolite2/) site and put the databases in the `priv/` dir.


`$make app shell`



```
1> geodata2:start().
ok
```

```
2> geodata2:open_base(b, "priv/GeoLite2-City.mmdb").
{ok,<0.39.0>}
``` 
```                                                                                                                                                                                                                                                       
3> geodata2:lookup(b, {94, 75, 242, 11}).
{ok,[{<<"registered_country">>,
      [{<<"names">>,
        [{<<"zh-CN">>,<<232,141,183,229,133,176>>},
         {<<"ru">>,
          <<208,157,208,184,208,180,208,181,209,128,208,187,208,
            176,208,189,208,...>>},
         {<<"pt-BR">>,<<"Holanda">>},
    ...
       {<<"geoname_id">>,6255148},
       {<<"code">>,<<"EU">>}]}]}
```
       
Shortcut if you don't want to copy too much data between processes, for GeoIP2 Country and GeoIP2 City only:

```
4> geodata2:lookup_geocity(b, {91,78,223,188}).
{ok,{geocity,<<"RU">>,<<"Moscow">>,524901,37.6156,55.7522}}
```

```
5> geodata2:reload_base(b).
ok
```
```
6> geodata2:reload_base(b, "priv/GeoLite2-Country.mmdb").
ok
```
```
7> geodata2:lookup_geocity(b, {91,78,223,188}).
{ok,{geocity,<<"RU">>,<<>>,undefined,undefined,undefined}}
```

```
8> geodata2:lookup(b, {127, 0, 0, 1}).
not_found
```

```
9> geodata2:lookup(b, {127, 0, 0, 1, 1}).
{error,format}
```


*A MaxMind repo with a number of test bases: [MaxMind-DB](https://github.com/maxmind/MaxMind-DB/)*

*This application uses some bits and pieces from [egeoip](http://github.com/mochi/egeoip)*
