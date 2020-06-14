module Game.Asteroids.Window(
    renderLoop
  ) where

import SDL
import Linear (V4(..))
import Control.Monad (unless)
import Control.Concurrent.STM

import Game.Asteroids.Render

renderLoop :: WorldRender world => TVar world -> IO ()
renderLoop worldVar = do
  initializeAll
  w <- atomically . readTVar $ worldVar
  window <- createWindow "Asteroids" defaultWindow {
      windowInitialSize = V2 (fromIntegral $ worldRenderWidth w) (fromIntegral $ worldRenderHeight w)
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
  present renderer
  unless exitEvent (loop worldVar renderer)
