import Data.Bifunctor
import Data.Foldable (for_)
import Data.List (sort)
import Data.Time.LocalTime
import Data.Traversable (for)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import System.Environment

loadSamples :: FilePath -> IO [(Double,Double)]
loadSamples name = do
  cnt <- readFile name
  let ys = ((\(a, b) -> (read a, read $ drop 1 b))  . break (',' ==)) <$> lines cnt
  let maxTick = fst $ last ys
  pure $ first (/ maxTick) <$> ys

printStats :: FilePath -> IO ()
printStats path = do
  samples <- loadSamples path
  let fps = snd <$> samples
  let maxFps = maximum fps
      minFps = minimum fps
      avgFps = sum fps / fromIntegral (length fps)
      meanFps = head . drop ((length fps `div` 2) - 1) $ sort fps
      sqdFps = sum ((^ 2) . (subtract avgFps) <$> fps) / fromIntegral (length fps - 1)
      disFps = sqrt sqdFps
      disFpsP = 100 * disFps / avgFps
  putStrLn $ unlines [
      "Maximum FPS:    " <> show (round maxFps :: Int)
    , "Minimum FPS:    " <> show (round minFps :: Int)
    , "Average FPS:    " <> show (round avgFps :: Int)
    , "Mean FPS:       " <> show (round meanFps :: Int)
    , "FPS dispertion: " <> show (round disFps :: Int)
    , "Dispertion %:   " <> show (round disFpsP :: Int)
    ]

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
    layout_x_axis . laxis_title .= "Relative session time"
    for_ samples $ \(title, vs) -> plot (line title [ vs ])

drawBars :: String -> String -> [(String, Int)] -> IO ()
drawBars descr title pairs = toFile def "bars.png" $ do
  layout_title .= descr
  layout_x_axis . laxis_generate .= autoIndexAxis (map fst pairs)
  plot $ fmap plotBars $ bars [title] (addIndexes (map (pure . snd) pairs))

makePairs :: [a] -> [(a, a)]
makePairs (x1:x2:xs) = (x1, x2) : makePairs xs
makePairs _ = []

main :: IO ()
main = do
  args <- getArgs
  let help = fail $ unlines [
        "Expecting either:",
        "single fps.out",
        "stats fps.out",
        "multiple Haskell fps1.out Rust fps2.out",
        "code Haskell 100 Rust 200",
        "memory Haskell 100 Rust 200"
        ]
  case args of
    ["single", path] -> drawSingle path
    ["stats", path] -> printStats path
    "multiple" : pairs -> drawMultiple $ makePairs pairs
    "code" : pairs -> drawBars "Lines of code" "Lines" $ fmap (second read) $ makePairs pairs
    "memory" : pairs -> drawBars "Maximum residence memory" "Kilobytes" $ fmap (second read) $ makePairs pairs
    _ -> help
