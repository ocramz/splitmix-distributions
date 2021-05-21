0.9 :

- ensure it builds with ghc 8.6.5 (== stackage lts 14.27) as well
- relax lower bound

0.8 :

- add `exceptions` dependency
- add MonadThrow (GenT m) instance

0.7 :

- add sampleIO, samplesIO
- clarify pure vs IO-based sampling in the docs

0.6 :

- add Chinese Restaurant Process

0.5 :

- add Log-normal, Laplace, Weibull distributions

0.4 :

- add Discrete, Categorical, Zipf-Mandelbrot distributions

0.3 :

- type signatures of all generators are now parametrized over some Monad m rather than Identity. This allows for more flexibility on the use site.

0.2 :

- add Pareto, Dirichlet, multinomial distributions

