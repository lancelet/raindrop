{-|
-}
module AlphaStencil
  ( -- * Types
    P(P)
  , Seg
  , Epsilon
    ( Epsilon
    -- * Functions
    )
  , seg
  , renderSegs
  , logRenderSegs
  )
where

import           AlphaStencil.Render            ( logRenderSegs
                                                , renderSegs
                                                )
import           AlphaStencil.Seg               ( Epsilon(Epsilon)
                                                , P(P)
                                                , Seg
                                                , seg
                                                )
