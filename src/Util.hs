-- | Utility functions not provided by the standard libraries.
module Util where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Functor.Foldable (Base, Recursive, cata, para)

{- | Recursion schemes doesn't provide adequate combinators for monadic folds.
This helper (aptly named `magic` as it seems entirely useless in isolation) lifts
a fold into a monadic context.
The first parameter is the fold, and the second is transformation to apply to
lift the monadic action from the fold return value.
-}
magic :: (Monad m, Traversable t) => ((t a -> m c) -> d) -> (a -> m b) -> (t b -> m c) -> d
magic f g = (traverse g >=>) >>> f

-- | Monadic catamorphism. `id` is used as the transformation function.
cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> t -> m a
cataM = magic cata id

{- | Monadic paramorphism. The transform function is `(a, m b) -> m (a, b)` to change
pull out the monadic action from the recursive call.
-}
paraM :: (Recursive t, Traversable (Base t), Monad m) => (Base t (t, a) -> m a) -> t -> m a
paraM = magic para \(x, y) -> (x,) <$> y
