module Synth.Source.Polygon
    ( Polygon (resolution), Params (..)
    , fromBuffers, forCoords )
where

import Synth.Source.Buffer (Buffer)
import Synth.Source.Buffer qualified as Buffer

import Data.Vector.Storable (Vector, (!))

import Data.Word (Word32)

import Control.Monad (forM_)

-- | A 2D geometrical representation of the sound produced by a `Source`.
--
-- This is not a closed shape in the mathematical sense, but rather a set of
-- coordinates intended for graphing.
data Polygon = Polygon
    { resolution :: !Word32     -- ^ The number of coordinates.
    , xs, ys :: Vector Float }

-- | The processing parameters of a `Polygon`.
data Params = Params
    { origin :: !(Float, Float)
    , size :: !Float }

-- | Constructs a `Polygon` by reading from two `Buffer`s containing the x and y
-- coordinates respectively.
--
-- If the passed `Buffer`s are not of the same size, the excess data from the
-- bigger `Buffer` is not used.
fromBuffers :: (Buffer, Buffer) -> IO Polygon
fromBuffers (xBuf, yBuf) = do
    let resolution = min (Buffer.size xBuf) (Buffer.size yBuf)
    xs <- Buffer.read xBuf resolution
    ys <- Buffer.read yBuf resolution
    pure Polygon{..}

forCoords :: Monad m => Polygon -> Params -> ((Float, Float) -> m ()) -> m ()
forCoords Polygon{..} Params{..} op =
    forM_ [0 .. lastI] \i -> op
        ( ox + normalise (xs ! i)
        , oy + normalise (ys ! i) )
  where
    lastI = fromIntegral resolution - 1
    normalise s = size * s / 2
    (ox, oy) = origin
