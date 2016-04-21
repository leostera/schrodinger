-record(observation, {
          name,
          started_at=timestamp(),
          finished_at=not_finished,
          time_delta=not_finished,
          result=no_result,
          function=no_function,
          pid=no_pid,
          type=candidate
         }).
