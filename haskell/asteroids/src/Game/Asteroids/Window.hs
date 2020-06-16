module Game.Asteroids.Window(
    renderLoop
  ) where

import Apecs (runWith, Has)
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Maybe (catMaybes)
import Linear (V4(..))
import Paths_asteroids
import SDL
import SDL.Mixer

import Game.Asteroids.Audio
import Game.Asteroids.Render
import Game.Asteroids.World.Event
import Game.Asteroids.World.Audio

renderLoop :: forall world . (WorldRender world, Has world IO AudioState)
  => world -> ([InputEvent] -> world -> IO world) -> IO ()
renderLoop w0 nextWorld = do
  ddir <- getDataDir
  initializeAll
  withAudio defaultAudio 256 $ do
    audio <- loadAudioResources $ ddir <> "/sounds"
    runWith w0 $ setupAudioResources audio
    setHintWithPriority NormalPriority HintRenderVSync EnableVSync
    size <- getRenderSize w0
    window <- createWindow "Asteroids" defaultWindow {
        windowInitialSize = size
      }
    renderer <- createRenderer window (-1) defaultRenderer
    leftRef <- newIORef False
    rightRef <- newIORef False
    thrustRef <- newIORef False
    fireRef <- newIORef False
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
              updateWorldEvent event = case eventPayload event of
                KeyboardEvent keyboardEvent -> let
                  pressed = keyboardEventKeyMotion keyboardEvent == Pressed
                  in case keysymKeycode (keyboardEventKeysym keyboardEvent) of
                    KeycodeLeft -> writeIORef leftRef pressed
                    KeycodeRight -> writeIORef rightRef pressed
                    KeycodeUp -> writeIORef thrustRef pressed
                    KeycodeSpace -> writeIORef fireRef pressed
                    _ -> pure ()
                _ -> pure ()
          traverse_ updateWorldEvent events
          worldEvents <- fmap catMaybes $ let
            ife a r = do
              v <- readIORef r
              pure $ if v then Just a else Nothing
            in sequence [
                  ife InputRotateLeft leftRef
                , ife InputRotateRight rightRef
                , ife InputThrust thrustRef
                , ife InputFire fireRef
                ]
          rendererDrawColor renderer $= V4 0 0 0 255
          clear renderer
          runWith w $ {-# SCC "renderWorld" #-} render renderer w
          present renderer
          w' <- {-# SCC "nextWorld" #-} nextWorld worldEvents w
          unless exitEvent (loop w')
        {-# INLINABLE loop #-}
    loop w0
