module Data.Semigroup.Act where

import Data.Semigroup
import qualified Numeric.Natural.Internal as NI

-- | Represents an action of semigroup @g@ to set @a@.
--
-- Laws: @'Endo' . 'act'@ must be a homomorphism of semigroups.
class Semigroup g => SemigroupAct g a where
    act :: g -> (a -> a)

-- | Represents an action of monoid @g@ to set @a@.
--
-- Laws: @'Endo' . 'act'@ must be a homomorphism of monoids.
class (Monoid g, SemigroupAct g a) => MonoidAct g a where

-- | A wrapper for constructing a monoid action from 'Option'.
newtype OptionSet g a = OptionSet { getOptionSet :: a }

instance (SemigroupAct g a, Semigroup g)
  => SemigroupAct (Option g) (OptionSet g a) where
    act (Option Nothing)  x = x
    act (Option (Just g)) (OptionSet x) = OptionSet $ (act g) x
instance (SemigroupAct g a, Monoid g)
  => MonoidAct (Option g) (OptionSet g a) where


-- | A wrapper for a group acting on itself.
newtype SelfAct a = SelfAct a
  deriving (Show, Read, Eq, Ord)
instance Functor SelfAct where
    fmap f (SelfAct x) = SelfAct (f x)
instance Semigroup g => Semigroup (SelfAct g) where
    (SelfAct x) <> (SelfAct y) = SelfAct $ x <> y
instance (Monoid g) => Monoid (SelfAct g) where
    mempty = SelfAct mempty
    mappend (SelfAct x) (SelfAct y) = SelfAct $ x `mappend` y
instance Semigroup g => SemigroupAct (SelfAct g) (SelfAct g) where
    act = (<>)
instance (Semigroup g, Monoid g) => MonoidAct (SelfAct g) (SelfAct g) where

-- | A wrapper for represeting the action of natural numbers with
-- multiplication on a monoid.
newtype Repeat a = Repeat { unwrapRepeat :: a }
  deriving (Show, Read, Eq, Ord)
instance Functor Repeat where
    fmap f (Repeat x) = Repeat (f x)
-- | The implementation uses 'times1p' which is defined very efficiently
-- for most semigroups.
instance (Monoid w, NI.Whole n) => SemigroupAct (Product n) (Repeat w) where
    act (Product m) (Repeat x)
        | m == 0    = Repeat mempty
        | otherwise = Repeat . unwrapMonoid $ times1p (NI.unsafePred m) (WrapMonoid x)
