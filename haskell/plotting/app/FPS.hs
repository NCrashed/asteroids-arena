import Data.Bifunctor
import Data.Foldable (for_)
import Data.Time.LocalTime
import Data.Traversable (for)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.Environment

loadSamples :: FilePath -> IO [(Double,Double)]
loadSamples name = do
  cnt <- readFile name
  let ys = ((\(a, b) -> (read a, read $ drop 1 b))  . break (',' ==)) <$> lines cnt
  let fpsAvg = sum (snd <$> ys) / fromIntegral (length ys)
  pure $ first (/ fpsAvg) <$> ys

drawSingle :: FilePath -> IO ()
drawSingle path = do
  samples <- loadSamples path
  (toFile def "fps.png" :: EC (Layout Double Double) () -> IO ()) $ do
    layout_title .= "Frame per seconds"
    layout_y_axis . laxis_override .= axisGridHide
    plot (line "FPS" [ samples ])

drawMultiple :: [(String, FilePath)] -> IO ()
drawMultiple pairs = do
  samples <- for pairs $ \(title, path) -> do
    ss <- loadSamples path
    pure (title, ss)
  (toFile def "fps_many.png" :: EC (Layout Double Double) () -> IO ()) $ do
    layout_title .= "Frame per seconds"
    layout_y_axis . laxis_override .= axisGridHide
    for_ samples $ \(title, vs) -> plot (line title [ vs ])

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
