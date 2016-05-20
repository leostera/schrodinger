# Schrodinger 🐈 ![https://api.travis-ci.org/ostera/schrodinger.svg](https://api.travis-ci.org/ostera/schrodinger.svg)
> Because your refactor will both work and not work until it runs in production

## Tutorial 

```erlang
1> application:start(schrodinger).
% schrodinger_sup gets started
% a schrodinger_lab get started by schrodinger_sup
2> schrodinger:experiment(my_test_experiment, fun control_function/0, [
  { candidate_1, fun candidate_1/0 },
  { candidate_2, fun candidate_2/0 },
  { candidate_3, fun candidate_3/0 }
]).
% an experiment record will be created and sent to the schrodinger_lab gen_server
% the schrodinger_lab will spawn a schrodinger_box to act as an experiment supervisor
% the schrodinger_box will spawn a schrodinger_experiment for each candidate and the control
% as experiments finish, they will publish back results to their corresponding box
% the box will then publish back the results to the lab
% once all experiments are finished, the box will send a summary to the lab
% the lab will forward all these messages to the publishers (by default it's self())
<control_fun_return_value>
```

This gives us the following architecture components:

* `schrodinger_app`, that starts the
* `schrodinger_sup`, that keeps alive the
* `schrodinger_lab`, that for each experiment will spawn a
* `schrodinger_box`, that will run, and summarize and bubble up results from a bunch of
* `schrodinger_experiment`, that will run the control/candidates and measure
* `schrodinger`, that will be the public API
