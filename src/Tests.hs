-- |

module Tests where

import           Extras
import           Test.QuickCheck
import           Test.Hspec

runQc :: IO ()
runQc = quickCheck prop_a


prop_i :: Int -> Bool
prop_i x = (x + 1) > x

prop_a :: Cell -> Bool
prop_a X     = True
prop_a O     = True
prop_a Empty = True



instance Arbitrary Cell where
  arbitrary = elements [X, O, Empty]
