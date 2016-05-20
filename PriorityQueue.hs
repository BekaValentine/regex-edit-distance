module PriorityQueue where

newtype PriorityQueue p a = PriorityQueue [(p,a)]
  deriving (Show)


empty :: PriorityQueue p a
empty = PriorityQueue []


elements :: PriorityQueue p a -> [a]
elements (PriorityQueue elems) = map snd elems


lookupPriority :: Ord p => p -> PriorityQueue p a -> Maybe a
lookupPriority p (PriorityQueue elems) = lookup p elems


lowestPriority :: PriorityQueue p a -> Maybe (p, a, PriorityQueue p a)
lowestPriority (PriorityQueue []) =
  Nothing
lowestPriority (PriorityQueue ((p,a):queue)) =
  Just (p, a, PriorityQueue queue)


insert :: Ord p => p -> a -> PriorityQueue p a -> PriorityQueue p a
insert p a (PriorityQueue elems) =
  PriorityQueue (go elems)
  where
    go [] = [(p,a)]
    go queue@((p',a'):queue')
      | p < p'    = (p,a) : queue
      | p == p'   = (p,a) : queue'
      | otherwise = (p',a') : go queue'


insertWith
  :: Ord p
  => (a -> a -> a)
  -> p -> a -> PriorityQueue p a -> PriorityQueue p a
insertWith f p a (PriorityQueue elems) =
  PriorityQueue (go elems)
  where
    go [] = [(p,a)]
    go queue@((p',a'):queue')
      | p < p'    = (p,a) : queue
      | p == p'   = (p, f a a') : queue'
      | otherwise = (p',a') : go queue'