# Schrodinger [![Travis-CI](https://api.travis-ci.org/ostera/schrodinger.svg)](https://travis-ci.org/ostera/schrodinger)
🔬 An Erlang library for carefully refactoring critical code paths.

Schrodinger helps you refactor a critical code path by setting up experiments.

These experiments consist on a `control` (your current code path) and a number of
`candidates` (paths of code to test).

For each of your experiments, the `control` will execute and return immediately,
with minimal overhead &mdash; preserving the original flow of your application.

Whereas the `candidates` will be ran safely and concurrently as isolated processes
and each of the results will be measured and published for later analysis.

## Installation

Simply include in your `rebar.config` as:

```
{deps, [
  % ...
  {schrodinger, {git, "https://github.com/ostera/schrodinger", {tag, "0.1.0"}}}
  % ...
]}.
```

## Usage

Include `schrodinger` in the list of apps that you start up in your `*.app.src` so
that your experiments will be run appropriately.

After that, just create new experiments with `schrodinger:experiment/3` as shown below:

#### Shell Session Tutorial

```erlang
1> application:start(schrodinger).
% schrodinger_sup gets started
% a schrodinger_lab get started by schrodinger_sup
2> Control = fun () -> ok end.
3> schrodinger:experiment(my_test_experiment, fun schrodinger_utils:timestamp/0, [
  { candidate_1, fun schrodinger_utils:timestamp/0 },
  { candidate_2, fun schrodinger_utils:timestamp/0 },
  { candidate_3, fun schrodinger_utils:timestamp/0 }
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

## Motivation

Inspired by [Github's Scientist](https://github.com/github/scientist) I decided to make
a quick proof of concept by running each of the candidates as different processes.

The original code was very naive, just wrapping the candidates in a simple fun that
would track the time they took and their result value in a record:

```erlang
run({Name, F}, Type) when is_function(F) ->
  Collector=self(),
  Observation = #observation{
                   name=Name,
                   predicate=F,
                   type=Type,
                   started_at=timestamp()
                  },
  Pid=erlang:spawn( fun () ->
                        Result = F(),
                        Done = timestamp(),
                        Collector ! Observation#observation {
                          result=Result,
                          finished_at=Done,
                          time_delta=Done-Observation#observation.started_at,
                          pid=self()
                         }
                    end ),
  Observation#observation{
     pid=Pid
  }.
```

The goal of Schrodinger is to provide a straightforward, clear tool to refactor big
chunks of a codebase without shrugging at the idea of doing it.

## Contributing

Fork, make a topic branch, and send a Pull Request. Travis will let you know if
it's good to go, and from the on we can review, retouch, and merge.

Included here is a `Makefile` with handy targets. Run `make` to execute the complete
battery of tests.

## Next Steps

See the [issues page](https://github.com/ostera/schrodinger/issues?q=is%3Aopen+is%3Aissue+label%3Aenhancement) for a list of planned enhancements and features.

## License

See [LICENSE](https://github.com/ostera/schrodinger/blob/master/LICENSE).
