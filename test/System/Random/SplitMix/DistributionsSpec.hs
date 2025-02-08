module System.Random.SplitMix.DistributionsSpec where

import System.Random.SplitMix.Distributions (sampleRunT, samplesRunT, stdNormal)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "repeated sampleRunT should behave like samplesRunT" $ do
    let
      s0 = (123, 1)
    (z1, s1) <- sampleRunT s0 stdNormal
    (z2, s2) <- sampleRunT s1 stdNormal
    (z2', s2') <- samplesRunT 2 s0 stdNormal
    it "final PRNG state is the same" $ do
      s2' `shouldBe` s2
    it "final sample value is the same" $ do
      last z2' `shouldBe` z2
