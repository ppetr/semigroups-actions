module Data.Semigroup.Act where

import Data.Semigroup
import Numeric.Natural.Internal

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

-- | A wrapper for an integer acting on an 'Enum'. If the resulting index is
-- out of the enum bounds, an exception is raised.
newtype EnumIntAct a = EnumIntAct a
  deriving (Show, Read, Eq, Ord)
instance Functor EnumIntAct where
    fmap f (EnumIntAct x) = EnumIntAct (f x)
instance (Integral n, Enum a) => SemigroupAct (Sum n) (EnumIntAct a) where
    act (Sum n) = fmap (toEnum . (+ (fromIntegral n)) . fromEnum)

-- | A wrapper for an integer acting on an instance of both 'Enum' and 'Bounded'.
-- The index wrap around the bounds, so
-- @Sum 1 `act` (EnumBoundedIntAct maxBound) == (EnumBoundedIntAct minBound)@ etc.
newtype EnumBoundedIntAct a = EnumBoundedIntAct a
  deriving (Show, Read, Eq, Ord)
instance Functor EnumBoundedIntAct where
    fmap f (EnumBoundedIntAct x) = EnumBoundedIntAct (f x)
instance (Bounded a, Enum a, Integral n) => SemigroupAct (Sum n) (EnumBoundedIntAct a) where
    act (Sum n) = fmap shift
      where
        shift x = toEnum $ mn + ((fromEnum x - mn + fromIntegral n) `mod` l)
          where
            mn = fromEnum (asTypeOf minBound x)
            l  = fromEnum (asTypeOf maxBound x) - mn + 1

-- | A wrapper for represeting the action of natural numbers with
-- multiplication on a monoid.
newtype Repeat a = Repeat { unwrapRepeat :: a }
  deriving (Show, Read, Eq, Ord)
instance Functor Repeat where
    fmap f (Repeat x) = Repeat (f x)
-- | The implementation uses 'times1p' which is defined very efficiently
-- for most semigroups.
instance (Monoid w, Whole n) => SemigroupAct (Product n) (Repeat w) where
    act (Product m) (Repeat x)
        | m == 0    = Repeat mempty
        | otherwise = Repeat . unwrapMonoid $ times1p (unsafePred m) (WrapMonoid x)
