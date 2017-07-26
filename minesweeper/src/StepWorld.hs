module StepWorld where
  import Board
  import Data.Time

  step :: Float -> Board -> IO (Board)
  step _ b@(Board st sz nm cells (False, _, _)) = return b
  step _ b@(Board st sz nm cells (True, start, _)) = do
    now <- getCurrentTime
    return (Board st sz nm cells (True, start, now))
