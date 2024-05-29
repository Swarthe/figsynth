{-# LANGUAGE DataKinds #-}

module Synth.Source.Params
    ( Params (..), ParamType (..), ParamVars
    , update, varsFrom
    )
where

import Vivid hiding (amplitude)

import Data.Default

-- | The parameters of a `Synth.Source.Source`.
--
-- Determines the sound that a `Synth.Source.Source` produces.
data Params = Params
    { modulation :: !Double     -- ^ 0 .. 100
    , modRatio   :: !Double     -- ^ 0 .. 10
    , foldRatio  :: !Double     -- ^ 0 .. 20
    , order      :: !Double     -- ^ 2.1 .. 50
    , roll       :: !Double     -- ^ -1000 .. 1000
    , teeth      :: !Double     -- ^ 0 .. 1
    , frequency  :: !Double     -- ^ 0 .. 44100
    , amplitude  :: !Double }   -- ^ 0 .. 1
  deriving Eq

-- | Describes one of the fields of `Params`.
--
-- Useful when handling a specific `Synth.Source.Source` parameter.
data ParamType
    = Modulation | ModRatio  | FoldRatio
    | Order      | Roll      | Teeth
    | Frequency  | Amplitude

type ParamVars =
    ( I "modulation", I "modRatio", I "foldRatio"
    , I "order",      I "roll",     I "teeth"
    , I "frequency",  I "amplitude" )

-- | Updates a field of `Params`.
update :: Params -> ParamType -> Double -> Params
update ps ty v = case ty of
    Modulation -> ps{modulation = v}
    ModRatio   -> ps{modRatio   = v}
    FoldRatio  -> ps{foldRatio  = v}
    Order      -> ps{order      = v}
    Roll       -> ps{roll       = v}
    Teeth      -> ps{teeth      = v}
    Frequency  -> ps{frequency  = v}
    Amplitude  -> ps{amplitude  = v}

-- | Converts `Params` to a `VarList` for use in a `SynthDef`.
varsFrom :: Params -> ParamVars
varsFrom Params{..} =
    ( toI modulation :: I "modulation"
    , toI modRatio   :: I "modRatio"
    , toI foldRatio  :: I "foldRatio"
    , toI order      :: I "order"
    , toI roll       :: I "roll"
    , toI teeth      :: I "teeth"
    , toI frequency  :: I "frequency"
    , toI amplitude  :: I "amplitude" )

-- | Silent preset. Produces a relatively pleasant sound with reasonable parameter
-- modifications.
instance Default Params where
    def = Params
        { modulation = 0
        , modRatio = 0
        , foldRatio = 0
        , order = 3
        , roll = 0
        , teeth = 0
        , frequency = 40
        , amplitude = 1 }
