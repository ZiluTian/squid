-- Generated Haskell code from Graph optimizer
-- Core obtained from: The Glorious Glasgow Haskell Compilation System, version 8.6.3
-- Optimized after GHC phase:
--   Simplifier: Max iterations = 4
--               SimplMode {Phase = 0 [Non-opt simplification],
--                          inline,
--                          no rules,
--                          eta-expand,
--                          case-of-case}
-- Total nodes: 138; Boxes: 15; Branches: 4
-- Apps: 45; Lams: 1; Unreduced Redexes: 0

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main (count,main,ds,rec,nats,n3) where

import GHC.Base
import GHC.Classes
import GHC.List
import GHC.Num
import GHC.Show
import GHC.TopHandler
import GHC.Types
import System.Exit
import System.IO

count = (case ds of {(,) arg0 arg1 -> arg0})

ds = (((,) (_0(# 3 #))) (_1(# (GHC.Types.I# 0#) #)))

_0(# state #) = (case (((GHC.Classes.>) state) 0) of {False -> (GHC.Types.I# 0#); True -> (((GHC.Num.+) (_0(# (((GHC.Num.-) state) 1) #))) (GHC.Types.I# 1#))})

_1(# state' #) = (((:) state') (_1(# (((GHC.Num.+) state') (GHC.Types.I# 1#)) #)))

main = (GHC.TopHandler.runMainIO (case (((GHC.Classes.&&) (((GHC.Classes.==) n3) (((:) (GHC.Types.I# 0#)) (((:) (GHC.Types.I# 1#)) (((:) (GHC.Types.I# 2#)) (((:) (GHC.Types.I# 3#)) (((:) (GHC.Types.I# 4#)) []))))))) (((GHC.Classes.==) count) (GHC.Types.I# 3#))) of {False -> (((GHC.Base.>>) (System.IO.print n3)) System.Exit.exitFailure); True -> System.Exit.exitSuccess}))

n3 = ((GHC.List.take (GHC.Types.I# 5#)) nats)

nats = (case ds of {(,) arg0' arg1' -> arg1'})

rec = (\state'2 -> (_1(# state'2 #)))
