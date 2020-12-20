import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime
import System.Environment

loadSamples :: FilePath -> IO [(Int,Double)]
loadSamples name = do
  cnt <- readFile name
  let ys = fmap read $ lines cnt
  pure $ [0 ..] `zip` ys

drawSingle :: FilePath -> IO ()
drawSingle path = do
  samples <- loadSamples path
  (toFile def "fps.png" :: EC (Layout Int Double) () -> IO ()) $ do
    layout_title .= "Frame per seconds"
    layout_y_axis . laxis_override .= axisGridHide
    plot (line "FPS" [ samples ])

drawMultiple :: [(String, FilePath)] -> IO ()
drawMultiple pairs = pure ()

makePairs :: [a] -> [(a, a)]
makePairs (x1:x2:xs) = (x1, x2) : makePairs xs
makePairs _ = []

main :: IO ()
main = do
  args <- getArgs
  let help = fail "Expecting either 'single fps.out' or 'multiple Haskell fps1.out Rust fps2.out' for consolidated plot"
  case args of
    ["single", path] -> drawSingle path
    "multiple" : pairs -> drawMultiple $ makePairs pairs
    _ -> help
