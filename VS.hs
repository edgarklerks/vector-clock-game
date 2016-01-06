module VS where

import qualified Data.IntMap as M
import Data.Monoid
import Data.List
import System.Random.MWC
import Control.Monad.ST
import Control.Applicative
import Control.Monad
import Data.Maybe

type ID = Int
type TimeDelta = Integer
newtype VectorClock = VC (ID, (M.IntMap Integer))

data PartialOrdering = NotOrderable | Orderable Ordering
     deriving Show

class PartialOrd a where
      (~~) :: a -> a -> PartialOrdering

instance PartialOrd VectorClock where
         (VC (pid1, vclock1)) ~~ (VC (pid2, vclock2)) = let lclock1 = (M.toList vclock1)
                                                            lclock2 = (M.toList vclock2)
                                                      {--
                    Cases:
                    forall i => x[i] > y[i] -> GT
                    forall i => x[i] = y[i] -> EQ
                    forall i => x[i] < y[i] -> LT
                    exists i,j => x[j] < y[j] && x[i] > y[i] -> Unorderable
           --}
                                                        in if (and ( zipWith (>=) lclock1 lclock2))
                                                                           then if or (zipWith (>) lclock1 lclock2)
                                                                                   then Orderable GT
                                                                                   else Orderable EQ
                                                                           else if and (zipWith (<) lclock1 lclock2)
                                                                                   then Orderable LT
                                                                                   else NotOrderable

instance Monoid VectorClock where
         mempty = zeroClock 0
         mappend vc1 vc2 = syncClock vc1 vc2 1

instance Show VectorClock where
         show (VC (id, map)) =  "VectorClock " <> show id <> " << " ++ intercalate " " ( M.foldWithKey step [] map) ++ " >>"
              where step k x z | k == id = ( "*" ++ show x ++ "*") : z
                               | otherwise = show x : z

updateID :: VectorClock -> ID -> VectorClock
updateID (VC (id, map)) pid =  VC (pid, map) <> zeroClock pid

updateClock :: VectorClock -> TimeDelta -> VectorClock
updateClock (VC (pid, clock)) td = VC (pid, M.adjust (+td) pid clock)


syncClock :: VectorClock -> VectorClock -> TimeDelta -> VectorClock
syncClock (VC (pid1, clock1)) (VC (_, clock2)) = updateClock ( VC (pid1, ( mergeClock clock1 clock2 )))
          where mergeClock :: M.IntMap Integer -> M.IntMap Integer -> M.IntMap Integer
                mergeClock = M.unionWith (\me other -> if other > me then other else me)


zeroClock :: ID -> VectorClock
zeroClock pid = VC (pid, M.fromList [(pid, 0)])


{--
        A test game, where every turn will create m events from n participants in one time slot,
the code will test if everything is still consistent
                                          --}

type Event = Int
data GameState s = GameState {
                 clocks :: M.IntMap VectorClock,
                 participants :: Int,
                 seed :: GenST s,
                 events :: [(TimeDelta, [(ID, Event, ID)])]
        }

getLastTime :: GameState s  -> TimeDelta
getLastTime gs | null ( events gs) = 0
getLastTime gs = fst $ head ( events gs)

instance Show ( GameState s) where
         show (GameState clocks participants _ events) = "clocks:\n" ++ intercalate "\n" ( M.foldWithKey (\k c z -> ("participant: " ++ show k ++ " -> " ++ show c ):z) [] clocks )<> "\nevents sequences:\n" <> intercalate "\n" ( foldr (\(td, ids) z -> ( (show td) ++ " -> " ++ (show ids)) :z ) [] events)

newGame :: Int -> ST s (GameState s)
newGame n = create >>= \seed -> return (GameState (M.fromList $ fmap (\pid -> (pid, zeroClock pid)) [1..n]) n seed [])

data GameOutput = GameOutput {
                vclocks :: M.IntMap VectorClock,
                vevents :: [(TimeDelta, [(ID, Event, ID)])]
     }

instance Show GameOutput where
         show (GameOutput c v) = show (GameState c undefined undefined v)

stepGame :: GameState s -> ST s (GameState s, GameOutput)
stepGame gs = do
         let s = seed gs
         r <- uniformR (1,participants gs)  s
         let lt = getLastTime gs
         -- Draw m players and send cards to m others
         ies <- replicateM r $ do
                           e <- uniformR (1,1000 :: Int) s
                           from <- uniformR (1, participants gs) s
                           to <- uniformR (1, participants gs) s
                           return (fromIntegral from,e,fromIntegral to)

         let vclocks' = foldr (replaceEvent
                                      ) (clocks gs) ies
         return (GameState vclocks' (participants gs) s ( (lt, ies): events gs), GameOutput vclocks' ( (lt, ies) : events gs))

replaceEvent :: (ID,Event,ID) -> M.IntMap VectorClock -> M.IntMap VectorClock
replaceEvent (from,ev,to) vclocks = let fclock = fromMaybe (zeroClock from ) $ M.lookup from vclocks
                                        tclock = fromMaybe (zeroClock to) $ M.lookup to vclocks
                                        tclock' = syncClock tclock fclock 1
                                        fclock' = updateClock fclock 1
                                    in M.insert to tclock' $ M.insert from fclock' vclocks


runGame :: Int -> GameState s -> ST s [GameOutput]
runGame 0 gs = return []
runGame steps gs =
  do (gs', go) <- stepGame gs
     (go:) <$> runGame (pred steps) gs'
