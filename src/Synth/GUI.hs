module Synth.GUI
    ( start
    , Event (..), WidgetEnv, WidgetNode
    )
where

import Synth.GUI.Common
import Synth.GUI.Input qualified as Input
import Synth.GUI.Graph qualified as Graph

import Synth.Source (Source)
import Synth.Source qualified as Source
import Synth.Source.Params qualified as Params

start :: Source.Params -> Source -> IO ()
start state source = do
    graphState <- Graph.newState source

    startApp state
        (handleEvent source graphState)
        (buildUI graphState)
        config
  where
    config =
        [ appWindowTitle "Polygonal Synthesizer"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appFontDef "Medium"  "./assets/fonts/Roboto-Medium.ttf"
        , appFontDef "Bold"    "./assets/fonts/Roboto-Bold.ttf"
        , appFontDef "Italic"  "./assets/fonts/Roboto-Italic.ttf"
        , appInitEvent NoOp ]

handleEvent
    :: Source
    -> Graph.State
    -> WidgetEnv
    -> WidgetNode
    -> Source.Params
    -> Event
    -> [Response]
handleEvent source graphState _env _node ps = \case
    SetParam ty v ->
        [ Model $ Params.update ps ty v
        , Task $ NoOp <$ do
            Source.setParam source ty v
            Graph.updateState graphState source
        ]
    NoOp -> []

buildUI
    :: Graph.State
    -> WidgetEnv
    -> Source.Params
    -> WidgetNode
buildUI graphState _env state = vstack
    [ Input.make state
    , Graph.make graphState ]
