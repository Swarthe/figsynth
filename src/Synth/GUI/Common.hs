module Synth.GUI.Common
    ( module Monomer
    , Event (..)
    , Response, Widget, WidgetEnv, WidgetNode
    )
where

import Synth.Source (Params, ParamType)

import Monomer hiding (Widget, WidgetEnv, WidgetNode)
import Monomer qualified

data Event = NoOp | SetParam !ParamType !Double

type Response = AppEventResponse Params Event

type Widget = Monomer.Widget Params Event

type WidgetEnv = Monomer.WidgetEnv Params Event

type WidgetNode = Monomer.WidgetNode Params Event

