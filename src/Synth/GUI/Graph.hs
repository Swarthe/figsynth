module Synth.GUI.Graph (make, State, newState, updateState) where

import Synth.GUI.Common

import Synth.Source (Source)
import Synth.Source qualified as Source

import Synth.Source.Polygon qualified as Polygon

import Monomer.Widgets.Single hiding (Widget, WidgetNode, makeState)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Default (def)

import GHC.Float (float2Double, double2Float)

make :: State -> WidgetNode
make = defaultWidgetNode "Graph" . makeWidget

data State = State
    { polygonRef :: IORef Source.Polygon
    , params :: !Polygon.Params }

newState :: Source -> IO State
newState source = do
    polygonRef <- newIORef =<< Source.newPolygon source
    pure State{params = initParams, ..}
  where
    initParams = Polygon.Params { origin = (0, 0), size = 100 }


updateState :: State -> Source -> IO ()
updateState State{polygonRef} source =
    writeIORef polygonRef =<< Source.newPolygon source

makeWidget :: State -> Widget
makeWidget state = createSingle state def
    { singleUseScissor = False
    , singleMerge = merge
    , singleHandleEvent = handleEvent
    , singleGetSizeReq = getSizeReq
    , singleResize = resize
    , singleRender = render
    }
  where
    merge _env node _oldNode _oldState = resultNode
        node{_wnWidget = makeWidget state}

    -- TODO: Pseudo 3D graph:
    -- 3 copes of the graph stacked and angled to form 3d illusion
    -- implement mouse rotation or smth maybe with mousepos in `render`
    handleEvent _env _node _target = \case
        _ -> Nothing

    getSizeReq _env _node = (expandSize 100 1, expandSize 100 1)

    resize _env node view =
        let origin =
                ( double2Float $ _rX view + (_rW view / 2)
                , double2Float $ _rY view + (_rH view / 2) - offsetY)
            size = double2Float
                $ min (_rW view) (_rH view) - paddingD
            state' = state{params = Polygon.Params{origin, size}}
         in resultNode node{_wnWidget = makeWidget state'}
      where
        offsetY = 7
        paddingD = 20

    render _env _node renderer = do
        polygon <- readIORef polygonRef

        Polygon.forCoords polygon params \(x, y) ->
            let rect = Rect (float2Double x) (float2Double y) 1 1
             in drawRect renderer rect (Just white) Nothing

    State{polygonRef, params} = state
