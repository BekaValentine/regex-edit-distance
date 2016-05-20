{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RegexEditDistance where

import Data.List
import qualified PriorityQueue as PQ
import RegularExpression





-- | The 'Transition' class captures the idea that a type 'a' can act as a
-- state in a state machine, transitioning to some new states by emitting
-- 'b's.

class Transition a b | a -> b where
  transition :: a -> [(a,b)]
  remainder :: a -> Int





-- | Given two states, we can determine if they constitute a final coupled
-- state if both states have a remainder of zero.

isFinalCoupledState :: (Transition a s, Transition b s')
                    => a -> b -> Bool
isFinalCoupledState a b = remainder a == 0 && remainder b == 0





-- | Given two states, we can calculate the lower bound on the distance to the
-- final coupled state by taking the absolute difference of their respective
-- remainders.

lowerBoundOnCoupledRemainder
  :: (Transition a s, Transition b s)
  => a -> b -> Int
lowerBoundOnCoupledRemainder a b =
  abs (remainder a - remainder b)





-- | Given two states, we can calculate the coupled transition by combining
-- the transitions where the left state transitions and the right remains the
-- same, the right transitions and the left remainders the same, and where
-- both transition simultaneously. We also calculate the cost of such coupled
-- transitions to be 0 when its simultaneous, and 1 otherwise.

coupledTransition :: (Eq s, Transition a s, Transition b s)
                  => a -> b -> [(a,b,s,Int)]
coupledTransition a b =
  leftTransitionsOnly ++ rightTransitionsOnly ++ simultaneousTransitions
  where
    
    transition_a = transition a
    transition_b = transition b
    
    leftTransitionsOnly =
      [ (a',b,s,1) | (a',s) <- transition_a ]
    
    rightTransitionsOnly =
      [ (a,b',s,1) | (b',s) <- transition_b ]
    
    simultaneousTransitions =
      [ (a',b',s,0)
      | (a',s) <- transition_a
      , (b',s') <- transition_b
      , s == s'
      ]





-- | A 'SearchState a b' is a coupled state together with the minimum cost
-- required to reach that state from the initial state.

data SearchState a b
  = SearchState
    { leftState :: a
    , rightState :: b
    , minimumCostFromInitialState :: Int
    }
  deriving (Show)





-- | A 'SearchSpace a b s' is a collection of coupled states that we've seen
-- already in the search process, together with some states waiting to be
-- inspected. The waiting states are arranged by the lower bound on the cost
-- of the state.

data SearchSpace a b s
  = SearchSpace
    { seen :: [(a,b)]
    , waiting :: PQ.PriorityQueue Int [SearchState a b]
    }
  deriving (Show)





-- | The initial search space for two states consists of a search space with
-- no states yet seen, together with a singleton waiting list.

initialSearchSpace :: (Transition a s, Transition b s)
                   => a -> b -> SearchSpace a b s
initialSearchSpace a b =
  SearchSpace
  { seen = []
  , waiting =
      PQ.insert (lowerBoundOnCoupledRemainder a b)
                [SearchState a b 0]
                PQ.empty
  }





-- | We can step a search space by taking the lowest priority in the waiting
-- list and for each state, transitioning from it to its next states. The
-- minimum cost from the initial state of each next state is computed by
-- adding the cost for the original state to the cost of the transition. The
-- states are then grouped by their calculated lower bound on the remainder,
-- and inserted into the rest of the waiting list, to form a new waiting list.

stepSearchSpace :: (Eq a, Eq b, Eq s, Transition a s, Transition b s)
                => SearchSpace a b s -> Maybe (SearchSpace a b s)
stepSearchSpace (SearchSpace sn wt) =
  case PQ.lowestPriority wt of
    Nothing -> Nothing
    Just (_, ss, wt') ->
      let sn' = [ (a,b) | SearchState a b _ <- ss ]
          ss' = [ ( cost + cost' + lowerBoundOnCoupledRemainder a' b'
                  , SearchState a' b' (cost + cost')
                  )
                | SearchState a b cost <- ss
                , not (elem (a,b) sn)
                , (a',b',_,cost') <- coupledTransition a b
                ]
          grouped = [ (c, map snd css)
                    | css@((c,_):_) <- groupBy
                                       (\(c,_) (c',_) -> c == c')
                                       ss'
                    ]
          wt'' = foldl' (\q (c,ss'') -> PQ.insertWith (++) c ss'' q)
                        wt'
                        grouped
      in Just (SearchSpace (sn' ++ sn) wt'')





-- We can search from an initial coupled state by repeatedly stepping until
-- no steps can be performed. Then, the final edit distance cost is the
-- minimum cost from the initial state for the final state.

searchEditDistance :: (Eq a, Eq b, Eq s, Transition a s, Transition b s)
                   => a -> b -> Int
searchEditDistance a b = go (initialSearchSpace a b)
  where
    go ss =
      let finals =
            filter (\(SearchState a' b' _) -> isFinalCoupledState a' b')
                   (concat (PQ.elements (waiting ss)))
      in case finals of
           (SearchState _ _ c:_) -> c
           _ -> case stepSearchSpace ss of
                  Nothing -> error "Could not step search space."
                  Just ss' -> go ss'





class EditDistanceState a s | a -> s where
  initialEditDistanceState :: a -> s


editDistance :: ( EditDistanceState a a', EditDistanceState b b'
                , Eq a', Eq b', Eq s, Transition a' s, Transition b' s
                )
             => a -> b -> Int
editDistance a b =
  searchEditDistance (initialEditDistanceState a)
                     (initialEditDistanceState b)









-- | 'String's can be states that emit 'Char's. Transitions just split the
-- next 'Char' off the list. The remainder is just the length.

instance Transition String Char where
  transition "" = []
  transition (c:cs) = [(cs,c)]
  remainder = length


-- | 'String's have an edit distance state of just 'String's.

instance EditDistanceState String String where
  initialEditDistanceState x = x





-- | 'RegexState's can also be states that emit 'Char's. Transitions are
-- emits from the state. The remainder is just the number of characters to the
-- end of the regular expression.

instance Transition RegexState Char where
  transition = emit
  remainder = charsToEnd


-- | 'Regex's have an edit distance state of 'RegexState's.

instance EditDistanceState Regex RegexState where
  initialEditDistanceState re = (Starting, re, [])





main :: IO ()
main = do print (editDistance "abc" "cxy")
          print (editDistance "abc" res)
  where
    -- res = /a(b|d+)c/
    res = Lit 'a' :>: (Lit 'b' :|: Rep (Lit 'd')) :>: Lit 'c'