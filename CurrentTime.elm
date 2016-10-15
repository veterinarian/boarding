module CurrentTime
    ( getCurrentTime
    ) where
{-| This module provides a task to get current time.  It is based on evancz/TaskTutorial

@docs getCurrentTime

-}

import Native.CurrentTime
import Task exposing (Task)
import Time exposing (Time)

{-| This task results in the current time. Whenever the task is performed, it
will look at the current time and give it to you.
-}
getCurrentTime : Task x Time
getCurrentTime =
  Native.CurrentTime.getCurrentTime
