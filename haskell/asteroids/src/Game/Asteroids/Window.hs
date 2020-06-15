module Game.Asteroids.Window(
    renderLoop
  ) where

import Apecs (runWith)
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.IORef
import Linear (V4(..))
import SDL

import Game.Asteroids.Render

renderLoop :: forall world . WorldRender world => world -> (world -> IO world) -> IO ()
renderLoop w0 nextWorld = do
  initializeAll
  setHintWithPriority NormalPriority HintRenderVSync EnableVSync
  size <- getRenderSize w0
  window <- createWindow "Asteroids" defaultWindow {
      windowInitialSize = size
    }
  renderer <- createRenderer window (-1) defaultRenderer
  let loop :: WorldRender world => world -> IO ()
      loop w = do
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
        runWith w $ {-# SCC "renderWorld" #-} render renderer w
        present renderer
        w' <- {-# SCC "nextWorld" #-} nextWorld w
        unless exitEvent (loop w')
      {-# INLINABLE loop #-}
  loop w0
