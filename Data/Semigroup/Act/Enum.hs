module Data.Semigroup.Act.Enum where

import Data.Semigroup
import Data.Semigroup.Act

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
