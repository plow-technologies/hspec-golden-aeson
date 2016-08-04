module Test.Utils where

import           Control.Monad
import           Data.Tuple
import           System.IO.Silently
import           Test.Hspec
import           Test.Hspec.Core.Runner

hspecSilently :: Spec -> IO (Summary, String)
hspecSilently s = swap <$> capture (hspecResult s)

shouldTestAs :: Spec -> Summary -> IO ()
shouldTestAs spec expected = do
  (summary, output) <- hspecSilently spec
  when (summary /= expected) $
    expectationFailure $
      "spec didn't yield the expected summary:\n" ++
      "  expected: " ++ show expected ++ "\n" ++
      "  got:      " ++ show summary ++ "\n" ++
      "output of the test-suite:\n" ++
      indent output

shouldProduceFailures :: Spec -> Int -> IO ()
shouldProduceFailures spec expected = do
  (summary, output) <- hspecSilently spec
  when (summaryFailures summary /= expected) $
    expectationFailure $
      "spec didn't yield the expected number of failures summary:\n" ++
      "  expected: " ++ show expected ++ "\n" ++
      "  got:      " ++ show (summaryFailures summary) ++ "\n" ++
      "output of the test-suite:\n" ++
      indent output

indent :: String -> String
indent = unlines . map ("    " ++ ) . lines
