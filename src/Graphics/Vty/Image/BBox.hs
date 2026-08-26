{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Vty.Image.BBox (BBox (..), bboxHeight, bboxWidth, inside) where

import Control.DeepSeq
import Data.Monoid.Action (Action (..))
import Graphics.Vty.Image.Translation (Translation (..))
import Graphics.Vty.Attributes (Attr)

data BBox a = BBox {bboxTop, bboxBottom, bboxLeft, bboxRight :: a} deriving (Show, Eq, Functor)

instance (NFData a) => NFData (BBox a) where
  rnf (BBox t b l r) = t `deepseq` b `deepseq` l `deepseq` r `seq` ()

instance (Ord a) => Semigroup (BBox a) where
  BBox t1 b1 l1 r1 <> BBox t2 b2 l2 r2 = BBox (min t1 t2) (max b1 b2) (min l1 l2) (max r1 r2)

instance (Num a) => Action (Translation a) (BBox a) where
  act (Translation dx dy) (BBox t b l r) =
    BBox
      (dy + t)
      (dy + b)
      (dx + l)
      (dx + r)

instance Action Attr (BBox a) where
  act _ b = b

instance Action Attr (Maybe (BBox a)) where
  act _ b = b

bboxHeight :: (Num a) => BBox a -> a
bboxHeight BBox {..} = bboxBottom - bboxTop

bboxWidth :: (Num a) => BBox a -> a
bboxWidth BBox {..} = bboxRight - bboxLeft

inside :: Ord a => (a, a) -> BBox a -> Bool
inside (x,y) BBox{..} = (bboxTop <= y && y < bboxBottom) && (bboxLeft <= x && x < bboxRight)