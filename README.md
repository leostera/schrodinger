# Nobel
> A simple approach to Science in Erlang

```erlang
nobel:experiment(ExperimentName, Control, Candidate)

ExperimentName :: string()
Control        :: Hypothesis
Candidate      :: Hypothesis | [Hypothesis]
Hypothesis     :: { Name, Function }
```

```erlang
nobel:experiment("experiment.key_name",
                {"Old control function", ControlFoo},
                {"New hypothesis!", CandidateFoo}).

nobel:experiment("multiple_experiments",
                {"Old control function", ControlFoo},
                [
                  {"comprehension instead of map", Exp1},
                  {"hit cache first", Exp2},
                  {"completely ignore cache", Exp3},
                ]).
```
