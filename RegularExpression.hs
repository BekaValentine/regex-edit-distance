module RegularExpression where

import Data.List




data Regex
  = Lit Char
  | Regex :>: Regex
  | Regex :|: Regex
  | Opt Regex
  | Rep Regex
  deriving (Show,Eq)

data LocalRegexContext
  = InSeqL Regex
  | InSeqR Regex
  | InAltL Regex
  | InAltR Regex
  | InOpt
  | InRep
  deriving (Show,Eq)

type RegexContext = [LocalRegexContext]

data Direction = Starting | Finishing
  deriving (Show,Eq)

type RegexState = (Direction, Regex, RegexContext)




next :: RegexState -> [RegexState]

-- this group corresponds to the edges out of starting states
next (Starting, Lit x, c)   = [ (Finishing, Lit x, c) ]
next (Starting, e :>: f, c) = [ (Starting, e, InSeqL f : c) ]
next (Starting, e :|: f, c) = [ (Starting, e, InAltL f : c)
                              , (Starting, f, InAltR e : c)
                              ]
next (Starting, Opt e, c)   = [ (Starting, e, InOpt : c)
                              , (Finishing, Opt e, c)
                              ]
next (Starting, Rep e, c)   = [ (Starting, e, InRep : c) ]

-- this group corresponds to the edges out of finishing states

next (Finishing, e, InSeqL f : c) = [ (Starting, f, InSeqR e : c) ]
next (Finishing, f, InSeqR e : c) = [ (Finishing, e :>: f, c) ]
next (Finishing, e, InAltL f : c) = [ (Finishing, e :|: f, c) ]
next (Finishing, f, InAltR e : c) = [ (Finishing, e :|: f, c) ]
next (Finishing, e, InOpt : c)    = [ (Finishing, Opt e, c) ]
next (Finishing, e, InRep : c)    = [ (Finishing, Rep e, c)
                                    , (Starting, e, InRep : c)
                                    ]
next _ = []




emit :: RegexState -> [(RegexState,Char)]
emit s =
  do s' <- next s
     case s' of
       (Finishing, Lit x, _) -> [ (s',x) ]
       _ -> emit s'




charsToEnd :: RegexState -> Int

charsToEnd (Starting, Lit x, c) =
  1 + charsToEnd (Finishing, Lit x, c)
charsToEnd (Starting, e :>: f, c) =
  charsToEnd (Starting, e, InSeqL f : c)
charsToEnd (Starting, e :|: f, c) =
  min (charsToEnd (Starting, e, InAltL f : c))
      (charsToEnd (Starting, f, InAltR e : c))
charsToEnd (Starting, Opt e, c) =
  charsToEnd (Finishing, Opt e, c)
charsToEnd (Starting, Rep e, c) =
  charsToEnd (Starting, e, InRep : c)

charsToEnd (Finishing, e, []) =
  0
charsToEnd (Finishing, e, InSeqL f : c) =
  charsToEnd (Starting, f, InSeqR e : c)
charsToEnd (Finishing, f, InSeqR e : c) =
  charsToEnd (Finishing, e :>: f, c)
charsToEnd (Finishing, e, InAltL f : c) =
  charsToEnd (Finishing, e :|: f, c)
charsToEnd (Finishing, f, InAltR e : c) =
  charsToEnd (Finishing, e :|: f, c)
charsToEnd (Finishing, e, InOpt : c) =
  charsToEnd (Finishing, Opt e, c)
charsToEnd (Finishing, e, InRep : c) =
  charsToEnd (Finishing, Rep e, c)