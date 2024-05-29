{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Synth.Source
    ( Source
    , Params (..), ParamType (..)
    , Polygon
    , start, stop, withStarted
    , setParam, newPolygon
    )
where

import Synth.Source.Params (Params (..), ParamType (..), ParamVars)
import Synth.Source.Params qualified as Params

import Synth.Source.Buffer (Buffer)
import Synth.Source.Buffer qualified as Buffer

import Synth.Source.Polygon (Polygon)
import Synth.Source.Polygon qualified as Polygon

import Vivid

import Control.Exception (bracket, onException)

-- To prevent `Integer` defaults in `SynthDef` definitions.
default (Float)

-- | Represents an audio source for polygonal synthesis active on the
-- SuperCollider server.
--
-- Supports generating geometrical data for graphing.
data Source = Source
    { syn :: Synth (InnerVars StateVars)
    , xBuf :: Buffer
    , yBuf :: Buffer }

type StateVars =
    AddParams ParamVars (I "xBuf", I "yBuf")

-- | Constructs and begins playing a `Source` on the server.
--
-- The `Source` is initialised with the passed parameters.
start :: Params -> IO Source
start params = do
    xBuf <- Buffer.new polygonResolution
    yBuf <- Buffer.new polygonResolution
        `onException` Buffer.close xBuf

    let paramVars = Params.varsFrom params
        bufVars   = (Buffer.iFrom xBuf, Buffer.iFrom yBuf)
        stateVars = paramVars `AddParams` bufVars

    syn <- synth (figSynthDef stateVars) stateVars
        `onException` (Buffer.close xBuf >> Buffer.close yBuf)

    pure Source{..}
  where
    -- The numbers of coordinates to generate for the graphical representation.
    --
    -- This value is chosen to produce a smooth representation while preserving
    -- performance.
    polygonResolution = 8192


-- | Stops playing a `Source` and frees its resources on the server.
--
-- The stopped `Source` should not be used afterwards.
stop :: Source -> IO ()
stop Source{..} = do
    free syn
    Buffer.close xBuf
    Buffer.close yBuf

withStarted :: Params -> (Source -> IO a) -> IO a
withStarted params = bracket (start params) stop

-- | Sets a parameter of a `Source`.
--
-- This operation is somewhat expensive as it causes an interaction with the
-- server. Because of this, it can be a good idea to track the `Params`
-- separately and avoid updating them unnecessarily.
setParam :: Source -> ParamType -> Double -> IO ()
setParam Source{syn} ty v = case ty of
    Modulation -> set syn (toI v :: I "modulation")
    ModRatio   -> set syn (toI v :: I "modRatio")
    FoldRatio  -> set syn (toI v :: I "foldRatio")
    Order      -> set syn (toI v :: I "order")
    Roll       -> set syn (toI v :: I "roll")
    Teeth      -> set syn (toI v :: I "teeth")
    Frequency  -> set syn (toI v :: I "frequency")
    Amplitude  -> set syn (toI v :: I "amplitude")

-- | Constructs a `Polygon` for use in a graphical representation.
--
-- This is an expensive operation and should only be performed when necessary.
newPolygon :: Source -> IO Polygon
newPolygon Source{xBuf, yBuf} = Polygon.fromBuffers (xBuf, yBuf)

-- | Based on the equations in the white paper.
figSynthDef :: StateVars -> SynthDef (InnerVars StateVars)
figSynthDef vars = sd vars do
    let m = V :: V "modulation";
        mr = V :: V "modRatio"; fr = V :: V "foldRatio"
        o = V :: V "order"; r = V :: V "roll"; t = V :: V "teeth"
        f = V :: V "frequency"; v = V :: V "amplitude"
        xb = V :: V "xBuf"; yb = V :: V "yBuf"

    thetaZero <- pi ~/ o
    fm <- f ~+ m ~* f ~* sinOsc (freq_ $ mr ~* f)
    angle <- tau ~* phasorFreq fm

    rotate <- tau ~* phasorFreq r
    shape <- (pi ~* (o ~- 2)) ~/ (2 ~* o ~* t)

    theta <- 2 ~* thetaZero ~* ((angle ~* o ~/ tau) `mod_` 1)

    ampl <- cos_ (thetaZero ~+ shape) ~/ cos_ (theta ~- thetaZero ~+ shape)
    x <- cos_ (angle ~+ rotate) ~* ampl
    y <- sin_ (angle ~+ rotate) ~* ampl

    out0  <- v ~* fold_ ((fr ~+ 1) ~* x) 1
    out90 <- v ~* fold_ ((fr ~+ 1) ~* y) 1

    write out0  xb
    write out90 yb
    out 0 [out0, out90]
  where
    write v buf =
        let phase = phasor (rate_ $ bufRateScale buf, end_ $ bufFrames buf)
         in void $ bufWr (in_ v, buf_ buf , phase_ phase)

    phasorFreq f = phasor $ rate_ (f ~/ sampleRate)

    fold_ = binaryOp Fold2
    mod_  = binaryOp Mod
    sin_  = unaryOp Sin
    cos_  = unaryOp Cos

    tau = 2 * pi
