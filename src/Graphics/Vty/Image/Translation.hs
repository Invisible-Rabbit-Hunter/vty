{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Image.Translation (Translation(..), translate, translateX, translateY) where
import Data.Monoid.Action (Action (..))
import Data.Monoid (Sum(..))

data Translation a = Translation {translationX, translationY :: !a}
  deriving (Show, Eq, Functor)
  deriving (Semigroup, Monoid) via Sum (Translation a)

instance Num a => Num (Translation a) where
  Translation dx dy + Translation dx' dy' = Translation (dx + dx') (dy + dy')
  Translation dx dy * Translation dx' dy' = Translation (dx * dx') (dy * dy')

  fromInteger i = Translation (fromInteger i) (fromInteger i)
  abs (Translation dx dy) = Translation (abs dx) (abs dy)
  signum (Translation dx dy) = Translation (signum dx) (signum dy)
  (-) :: Num a => Translation a -> Translation a -> Translation a
  Translation dx dy - Translation dx' dy' = Translation (dx - dx') (dy - dy')
  negate (Translation dx dy) = Translation (negate dx) (negate dy)

translate :: Action (Translation a) b => a -> a -> b -> b
translate dx dy = act (Translation dx dy)

-- | Translates an object in the horizontal direction.
translateX :: (Action (Translation a) b, Num a) => a -> b -> b
translateX x = translate x 0

-- | Translates an object in the vertical direction.
translateY :: (Action (Translation a) b, Num a) => a -> b -> b
translateY = translate 0
