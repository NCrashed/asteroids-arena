import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime
import System.Environment

loadSamples :: FilePath -> IO [(Int,Double)]
loadSamples name = do
  cnt <- readFile name
  let ys = fmap read $ lines cnt
  pure $ [0 ..] `zip` ys

main :: IO ()
main = do
  path : _ <- getArgs
  samples <- loadSamples path
  (toFile def "fps.png" :: EC (Layout Int Double) () -> IO ()) $ do
    layout_title .= "Frame per seconds"
    layout_y_axis . laxis_override .= axisGridHide
    plot (line "FPS" [ samples ])
