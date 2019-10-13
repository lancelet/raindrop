{-|
Module      : AlphaStencilDiagrams.SamplePaths
Description : Sample paths for the alpha stencil diagrams.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencilDiagrams.SamplePaths
  ( Path(Path)
  , Loop(Loop)
  , hashSymbol
  , pathToSegs
  )
where

import           Data.Maybe                     ( catMaybes )

import           AlphaStencil                   ( Epsilon
                                                , P(P)
                                                , Seg
                                                , seg
                                                )

newtype Path a = Path [Loop a]
newtype Loop a = Loop [P a]

pathToSegs :: (Ord a, Fractional a) => Epsilon a -> Path a -> [Seg a]
pathToSegs eps (Path ls) = concat $ loopToSegs eps <$> ls

loopToSegs
  :: forall  a . (Ord a, Fractional a) => Epsilon a -> Loop a -> [Seg a]
loopToSegs _   (Loop []       ) = []
loopToSegs _   (Loop [_, _]   ) = []
loopToSegs eps (Loop (pf : ps)) = catMaybes (followTrail pf (pf : ps))
 where
  followTrail :: P a -> [P a] -> [Maybe (Seg a)]
  followTrail _       []           = []
  followTrail firstPt [p         ] = [seg eps p firstPt]
  followTrail firstPt (p : q : ss) = seg eps p q : followTrail firstPt (q : ss)

hashSymbol :: Path Float
hashSymbol = Path
  [ Loop
    [ P 3.70  1.00  -- bottom right corner; outer loop
    , P 5.00  1.00
    , P 5.78  4.90
    , P 8.78  4.90
    , P 8.00  1.00
    , P 9.30  1.00
    , P 10.08 4.90
    , P 13.00 4.90
    , P 13.00 6.30
    , P 10.36 6.30
    , P 11.02 9.60
    , P 13.00 9.60
    , P 13.00 11.00
    , P 11.30 11.00
    , P 12.10 15.00
    , P 10.80 15.00
    , P 10.00 11.00
    , P 7.00  11.00
    , P 7.80  15.00
    , P 6.50  15.00
    , P 5.70  11.00
    , P 3.00  11.00
    , P 3.00  9.60
    , P 5.42  9.60
    , P 4.76  6.30
    , P 3.00  6.30
    , P 3.00  4.90
    , P 4.48  4.90
    ]
  , Loop
    [ P 6.72 9.60 -- top left corner; inner loop
    , P 9.72 9.60
    , P 9.06 6.30
    , P 6.06 6.30
    ]
  ]
