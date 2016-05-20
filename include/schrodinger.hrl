
-type name() :: atom() | [char()] | <<>>.
-type timestamp() :: integer() | unset.

-type type() :: candidate | control.

-type predicate()  :: fun().
-type control()    :: predicate().
-type candidate()  :: { name(), predicate() }.
-type candidates() :: [ candidate() ].

-type publisher()  :: pid().
-type publishers() :: [ publisher() ].

-type option()  :: { name(), any() }.
-type options() :: [ option() ].

-record(observation, {
          duration    :: timestamp(),
          finished_at :: timestamp(),
          name        :: name(),
          pid         :: pid(),
          predicate   :: predicate(),
          result      :: any(),
          started_at  :: timestamp(),
          type        :: type()
         }).
