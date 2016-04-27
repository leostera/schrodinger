# Schrodinger
> Because your refactor will both work and not work until it runs in production

```erlang
1> {ok, S} = schrodinger_server:start_link().
{ok,<0.74.0>}
2> schrodinger:sample(S, "Test Experiment", 7).
Experiment #0   1461755316993   1461755317885   892     true    control
true
3> schrodinger:report(S, "Test Experiment").
"Test Experiment" (7 experiments)
------------------------------------------------------------------------------------------
Name            Start           End             Time    Result  Observation
------------------------------------------------------------------------------------------
Experiment #6   1461755316993   1461755319662   2669    6       candidate
Experiment #5   1461755316993   1461755319218   2225    5       candidate
Experiment #4   1461755316993   1461755318775   1782    4       candidate
Experiment #3   1461755316993   1461755318332   1339    3       candidate
Experiment #2   1461755316993   1461755317885   892     2       candidate
Experiment #0   1461755316993   1461755317885   892     true    control
Experiment #1   1461755316993   1461755317445   452     1       candidate
[{observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "6"],
              1461755316993,1461755319662,2669,6,
              #Fun<schrodinger.1.86571397>,<0.82.0>,candidate},
 {observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "5"],
              1461755316993,1461755319218,2225,5,
              #Fun<schrodinger.1.86571397>,<0.81.0>,candidate},
 {observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "4"],
              1461755316993,1461755318775,1782,4,
              #Fun<schrodinger.1.86571397>,<0.80.0>,candidate},
 {observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "3"],
              1461755316993,1461755318332,1339,3,
              #Fun<schrodinger.1.86571397>,<0.79.0>,candidate},
 {observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "2"],
              1461755316993,1461755317885,892,2,
              #Fun<schrodinger.1.86571397>,<0.78.0>,candidate},
 {observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "0"],
              1461755316993,1461755317885,892,true,
              #Fun<schrodinger.1.86571397>,<0.76.0>,control},
 {observation,[69,120,112,101,114,105,109,101,110,116,32,35,
               "1"],
              1461755316993,1461755317445,452,1,
              #Fun<schrodinger.1.86571397>,<0.77.0>,candidate}]
```
