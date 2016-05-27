
-record(observation, {
          duration    :: schrodinger:timestamp(),
          finished_at :: schrodinger:timestamp(),
          name        :: schrodinger:name(),
          pid         :: schrodinger:pid(),
          predicate   :: schrodinger:predicate(),
          result      :: schrodinger:any(),
          started_at  :: schrodinger:timestamp(),
          type        :: schrodinger:type()
         }).

