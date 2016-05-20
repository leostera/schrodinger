# Schrodinger 🐈
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

## Architecture (Explained Shell Session)

```erlang
% assuming the anchorman application is running
1> application:start(schrodinger).
% schrodinger_sup gets started
% a schrodinger_lab get started by schrodinger_sup
2> schrodinger:experiment(my_test_experiment, fun control_function/0, [
  { candidate_1, fun candidate_1/0 },
  { candidate_2, fun candidate_2/0 },
  { candidate_3, fun candidate_3/0 }
]).
% an anchorman subscription will be created for the control_function's return value
% an experiment record will be created and sent to the schrodinger_lab gen_server
% the schrodinger_lab will spawn a schrodinger_box to act as an experiment supervisor
% the schrodinger_box will spawn a schrodinger_experiment for each candidate and the control
% as experiments finish, they will publish back results to their corresponding box
% the box will then publish back the results to the lab
% once all experiments are finished, the box will send a summary to the lab
% the lab will forward all these messages to anchorman for publishing/consumption
% the anchorman subscription will receive the value
<control_fun_return_value>
```

This gives us the following architecture components:

* `schrodinger_app`, that starts the
* `schrodinger_sup`, that keeps alive the
* `schrodinger_lab`, that for each experiment will spawn a
* `schrodinger_box`, that will run, and summarize and bubble up results from a bunch of
* `schrodinger_experiment`, that will run the control/candidates and measure
* `schrodinger`, that will be the public API
