module Game.Asteroids.Window(
    renderLoop
  ) where

import Apecs (runWith)
import Control.Concurrent.STM
import Control.Monad (unless)
import Linear (V4(..))
import SDL

import Game.Asteroids.Render

renderLoop :: WorldRender world => TVar world -> IO ()
renderLoop worldVar = do
  initializeAll
  w <- atomically . readTVar $ worldVar
  size <- getRenderSize w
  window <- createWindow "Asteroids" defaultWindow {
      windowInitialSize = size
    }
  renderer <- createRenderer window (-1) defaultRenderer
  loop worldVar renderer

loop :: WorldRender world => TVar world -> Renderer -> IO ()
loop worldVar renderer = do
  events <- pollEvents
  let isEventExit event =
        case eventPayload event of
          WindowClosedEvent _ -> True
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
          _ -> False
      exitEvent = any isEventExit events
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  w <- atomically . readTVar $ worldVar
  runWith w $ render renderer w
  present renderer
  unless exitEvent (loop worldVar renderer)
