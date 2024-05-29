module Synth.GUI.Input (make) where

import Synth.GUI.Common

import Synth.Source qualified as Source
import Synth.Source (ParamType (..))

import Data.Text (Text)
import Data.Text.Lazy.Builder.RealFloat (FPFormat (Fixed), formatRealFloat)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)

make :: Source.Params -> WidgetNode
make Source.Params{..} = sliderStack
    [ ("Modulation", modulation, SetParam Modulation, 0,    100 )
    , ("Mod ratio",  modRatio,   SetParam ModRatio,   0,    10  )
    , ("Fold ratio", foldRatio,  SetParam FoldRatio,  0,    20  )
    , ("Order",      order,      SetParam Order,      2.1,  50  )
    , ("Roll",       roll,       SetParam Roll,      -1000, 1000)
    , ("Teeth",      teeth,      SetParam Teeth,      0,    1   )
    , ("Frequency",  frequency,  SetParam Frequency,  0,    500 )
    , ("Amplitude",  amplitude,  SetParam Amplitude,  0,    1   ) ]

sliderStack :: [(Text, Double, Double -> Event, Double, Double)] -> WidgetNode
sliderStack configs = hstack
    [ vstack names   `styleBasic` [paddingR 5]
    , vstack sliders `styleBasic` [paddingR 5]
     -- Constant width so the sliders don't jitter.
    , vstack values  `styleBasic` [width   73] ]
        `styleBasic` [padding 10]
  where
    (names, sliders, values) = foldr
        (\(title, v, f, lo, hi) (names', sliders', values') ->
            ( name title : names'
            , slider v f lo hi : sliders'
            , value v : values' ))
        ([], [], [])
        configs

name :: Text -> WidgetNode
name text = label_ text [resizeFactor 0]
    `styleBasic` [textFont "Regular", textSize 20]

slider
    :: Double -> (Double -> Event)
    -> Double -> Double
    -> WidgetNode
slider v f lo hi =
    hsliderV_ v f lo hi [thumbVisible]
        `styleBasic`
            [ paddingV 5
            , hlColor azure     -- Thumb colour
            , fgColor skyBlue ]
  --where
  --  roundToPlace n x = fromIntegral (round $ x * n :: Int) / n

value :: Double -> WidgetNode
value v  = label_ s [resizeFactor 0]
    `styleBasic` [textFont "Bold", textSize 20, textColor gold]
  where
    -- Disable scientific notation for consistent representation.
    s = toStrict $ toLazyText $ formatRealFloat Fixed Nothing v


