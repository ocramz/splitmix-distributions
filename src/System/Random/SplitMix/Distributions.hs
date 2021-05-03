{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
{-|
Random samplers for few common distributions, with an interface similar to that of @mwc-probability@.

= Usage

Compose your random sampler out of simpler ones thanks to the Applicative and Monad interface, e.g. this is how you would declare and sample a binary mixture of Gaussian random variables:

@
import Control.Monad (replicateM)
import System.Random.SplitMix.Distributions (Gen, sample, bernoulli, normal)

process :: `Gen` Double
process = do
  coin <- `bernoulli` 0.7
  if coin
    then
      `normal` 0 2
    else
      normal 3 1

dataset :: [Double]
dataset = `sample` 1234 $ replicateM 20 process
@

and sample your data in a pure (`sample`) or monadic (`sampleT`) setting.

== Implementation details

The library is built on top of @splitmix@, so the caveats on safety and performance that apply there are relevant here as well.


-}
module System.Random.SplitMix.Distributions (
  -- * Distributions
  -- ** Continuous
  stdUniform, uniformR,
  exponential,
  stdNormal, normal,
  beta,
  gamma,
  -- ** Discrete
  bernoulli,
  -- * PRNG
  -- ** Pure
  Gen, sample,
  -- ** Monadic
  GenT, sampleT,
  withGen
                                            ) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import GHC.Word (Word64)

-- erf
import Data.Number.Erf (InvErf(..))
-- mtl
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.State (MonadState(..), modify)
-- splitmix
import System.Random.SplitMix (SMGen, mkSMGen, splitSMGen, nextInt, nextInteger, nextDouble)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)

-- | Random generator
--
-- wraps 'splitmix' state-passing inside a 'StateT' monad
--
-- useful for embedding random generation inside a larger effect stack
newtype GenT m a = GenT { unGen :: StateT SMGen m a } deriving (Functor, Applicative, Monad, MonadState SMGen, MonadTrans, MonadIO)

-- | Pure random generation
type Gen = GenT Identity

-- | Monadic evaluation
sampleT :: Monad m =>
            Word64 -- ^ random seed
         -> GenT m a -> m a
sampleT seed gg = evalStateT (unGen gg) (mkSMGen seed)

-- | Pure evaluation
sample :: Word64 -- ^ random seed
        -> Gen a
        -> a
sample seed gg = evalState (unGen gg) (mkSMGen seed)


-- | Bernoulli trial
bernoulli :: Double -- ^ bias parameter \( 0 \lt p \lt 1 \)
          -> Gen Bool
bernoulli p = withGen (bernoulliF p)

-- | Uniform between two values
uniformR :: Double -- ^ low
         -> Double -- ^ high
         -> Gen Double
uniformR lo hi = scale <$> stdUniform
  where
    scale x = x * (hi - lo) + lo

-- | Standard normal
stdNormal :: Gen Double
stdNormal = normal 0 1

-- | Uniform in [0, 1)
stdUniform :: Gen Double
stdUniform = withGen nextDouble

-- | Beta distribution, from two standard uniform samples
beta :: Double -- ^ shape parameter \( \alpha \gt 0 \) 
     -> Double -- ^ shape parameter \( \beta \gt 0 \)
     -> Gen Double
beta a b = go
  where
    go = do
      (y1, y2) <- sample2
      if
        y1 + y2 <= 1
        then pure (y1 / (y1 + y2))
        else go
    sample2 = f <$> stdUniform <*> stdUniform
      where
        f u1 u2 = (u1 ** (1/a), u2 ** (1/b))

-- | Gamma distribution, using Ahrens-Dieter accept-reject (algorithm GD):
--
-- Ahrens, J. H.; Dieter, U (January 1982). "Generating gamma variates by a modified rejection technique". Communications of the ACM. 25 (1): 47–54
gamma :: Double -- ^ shape parameter \( k \gt 0 \)
      -> Double -- ^ scale parameter \( \theta \gt 0 \)
      -> Gen Double
gamma k th = do
  xi <- sampleXi
  us <- replicateM n (log <$> stdUniform)
  pure $ th * xi - sum us
  where
    sampleXi = do
      (xi, eta) <- sample2
      if eta > xi ** (delta - 1) * exp (- xi)
        then sampleXi
        else pure xi
    (n, delta) = (floor k, k - fromIntegral n)
    ee = exp 1
    sample2 = f <$> stdUniform <*> stdUniform <*> stdUniform
      where
        f u v w
          | u <= ee / (ee + delta) =
            let xi = v ** (1/delta)
            in (xi, w * xi ** (delta - 1))
          | otherwise =
            let xi = 1 - log v
            in (xi, w * exp (- xi))


-- | Normal distribution
normal :: Double -- ^ mean
       -> Double -- ^ standard deviation \( \sigma \gt 0 \)
       -> Gen Double
normal mu sig = withGen (normalF mu sig)

-- | Exponential distribution
exponential :: Double -- ^ rate parameter \( \lambda > 0 \)
            -> Gen Double
exponential l = withGen (exponentialF l)

-- | Wrap a 'splitmix' PRNG function
withGen :: Monad m =>
           (SMGen -> (a, SMGen)) -- ^ explicit generator passing (e.g. 'nextDouble')
        -> GenT m a
withGen f = GenT $ do
  gen <- get
  let
    (b, gen') = f gen
  put gen'
  pure b

exponentialF :: Double -> SMGen -> (Double, SMGen)
exponentialF l g = (exponentialICDF l x, g') where (x, g') = nextDouble g

normalF :: Double -> Double -> SMGen -> (Double, SMGen)
normalF mu sig g = (normalICDF mu sig x, g') where (x, g') = nextDouble g

bernoulliF :: Double -> SMGen -> (Bool, SMGen)
bernoulliF p g = (x < p , g') where (x, g') = nextDouble g


-- | inverse CDF of normal rv
normalICDF :: InvErf a =>
              a -- ^ mean
           -> a -- ^ std dev
           -> a -> a
normalICDF mu sig p = mu + sig * sqrt 2 * inverf (2 * p - 1)

-- | inverse CDF of exponential rv
exponentialICDF :: Floating a =>
                   a -- ^ rate
                -> a -> a
exponentialICDF l p = (- 1 / l) * log (1 - p)
