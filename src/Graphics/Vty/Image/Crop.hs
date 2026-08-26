{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Graphics.Vty.Image.Crop (Crop (..), crop) where

import Data.Monoid.Action (Action (..))
import Graphics.Vty.Image.BBox (BBox (..))
import Graphics.Vty.Image.Translation (Translation (..))

data Crop a
  = Invalid
  | Crop !(BBox a)
  deriving (Show, Eq, Functor)

instance (Ord a) => Semigroup (Crop a) where
  Invalid <> _ = Invalid
  _ <> Invalid = Invalid
  Crop b1 <> Crop b2 =
    let top = max (bboxTop b1) (bboxTop b2)
        bottom = min (bboxBottom b1) (bboxBottom b2)
        left = max (bboxLeft b1) (bboxLeft b2)
        right = min (bboxRight b1) (bboxRight b2)
     in if top < bottom && left < right
          then Crop (BBox top bottom left right)
          else Invalid

instance (Ord a, Bounded a) => Monoid (Crop a) where
  mempty = Crop (BBox minBound maxBound minBound maxBound)

instance (Ord a) => Action (Crop a) (Maybe (BBox a)) where
  act c b = case c <> maybe Invalid Crop b of
    Invalid -> Nothing
    Crop b' -> Just b'

instance (Ord a, Bounded a, Num a) => Action (Translation a) (Crop a) where
  act _ Invalid = Invalid
  act t (Crop BBox {..}) =
    Crop
      BBox
        { bboxTop = if bboxTop == minBound || bboxTop == maxBound then bboxTop else bboxTop + translationY t,
          bboxBottom = if bboxBottom == minBound || bboxBottom == maxBound then bboxBottom else bboxBottom + translationY t,
          bboxLeft = if bboxLeft == minBound || bboxLeft == maxBound then bboxLeft else bboxLeft + translationX t,
          bboxRight = if bboxRight == minBound || bboxRight == maxBound then bboxRight else bboxRight + translationX t
        }

crop :: (Action (Crop a) b) => BBox a -> b -> b
crop = act . Crop