{-|
-}
module AlphaStencil
  ( P(P)
  , Seg
  , Epsilon(Epsilon)
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
