{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Vty.Image.Internal
  ( Image (Prim, Compose, Act, Empty),
    Prim (..),
    getBBox,
    ppImageStructure,
    clipText,
    foldImage,
  )
where

import Control.DeepSeq
import GHC.Generics
import Graphics.Text.Width
import Graphics.Vty.Attributes

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))
import Data.Monoid.Action (Action (..))
import qualified Data.Text.Lazy as TL
import Graphics.Vty.Image.BBox (BBox (..))
import Graphics.Vty.Image.Crop (Crop (..))
import Graphics.Vty.Image.Translation (Translation (..))

clipText :: TL.Text -> Int -> Int -> TL.Text
clipText txt leftSkip rightClip =
  -- CPS would clarify this I think
  let (toDrop, padPrefix) = clipForCharWidth leftSkip txt 0
      txt' = if padPrefix then TL.cons '…' (TL.drop (toDrop + 1) txt) else TL.drop toDrop txt
      (toTake, padSuffix) = clipForCharWidth rightClip txt' 0
      txt'' = TL.append (TL.take toTake txt') (if padSuffix then TL.singleton '…' else TL.empty)
      -- Note: some characters and zero-width and combining characters
      -- combine to the left, so keep taking characters even if the
      -- width is zero.
      clipForCharWidth w t n
        | TL.null t = (n, False)
        | w < cw = (n, w /= 0)
        | otherwise = clipForCharWidth (w - cw) (TL.tail t) (n + 1)
        where
          cw = safeWcwidth (TL.head t)
   in txt''

-- | This is the internal representation of Images. Use the constructors
-- in "Graphics.Vty.Image" to create instances.
--
-- Images are:
--
-- * a horizontal span of text
--
-- * a horizontal or vertical join of two images
--
-- * a two dimensional fill of the 'Picture's background character
--
-- * a cropped image
--
-- * an empty image of no size or content.
data Prim
  = SpanAt
      { pattr :: Attr,
        px, py :: Int,
        pstr :: TL.Text,
        outputWidth, charWidth :: Int
      }
  | BGFill {fillBox :: BBox Int}
  deriving (Show, Eq)

-- | This is the internal representation of Images. Use the constructors
-- in "Graphics.Vty.Image" to create instances.
--
-- Images are:
--
-- * a primitive (e.g. a span of text at a coordinate).
--
-- * a composition of subimages, overlayed atop one another.
--
-- * an action (cropping, translation, etc.) performed on a subimage.
--
-- * an empty image of no size or content.
data Image
  = Prim Prim
  | -- | Invariant: u == bbox first <> bbox second
    Compose {bbox :: Maybe (BBox Int), partFirst :: Image, partSecond :: Image}
  | -- | Interpreted as a translated cropping of a translation the subimage (i.e. crop (translate dx dy b) (translate dx dy t))
    Act {attr :: Attr, crop :: Crop Int, translation :: Translation Int, subimage :: Image}
  | Empty
  deriving (Show, Eq, Generic)

-- | pretty print just the structure of an image.
ppImageStructure :: Image -> String
ppImageStructure = go 0
  where
    go indent img = tab indent ++ pp indent img
    tab indent = concat $ replicate indent "  "
    pp _ (Prim p) = "Prim(" ++ show p ++ ")"
    pp i (Compose {bbox = b, partFirst = f, partSecond = s}) =
      "Compose("
        ++ show b
        ++ ")\n"
        ++ go (i + 1) f
        ++ "\n"
        ++ go (i + 1) s
    pp i (Act {crop = c, translation = t, subimage = s}) =
      "Act("
        ++ show c
        ++ ","
        ++ show t
        ++ ")\n"
        ++ go (i + 1) s
    pp _ Empty = "Empty"

instance NFData Image where
  rnf Empty = ()
  rnf (Act a c t s) = a `seq` c `seq` t `seq` s `seq` ()
  rnf (Compose u f s) = u `deepseq` f `deepseq` s `seq` ()
  rnf (Prim p) = p `seq` ()

primBBox :: Prim -> BBox Int
primBBox (SpanAt _ x y _ dw _) = BBox y (y + 1) x (x + dw)
primBBox (BGFill box) = box


getBBox :: Image -> Maybe (BBox Int)
getBBox Empty = Nothing
getBBox (Compose bbox _ _) = bbox
getBBox (Act a c t i) = act a $ act (act t c) $ act t <$> getBBox i
getBBox (Prim p) = Just (primBBox p)

-- -- | The width of an Image. This is the number display columns the image
-- -- will occupy.
-- imageWidth :: Image -> Int
-- imageWidth HorizText {outputWidth = w} = w
-- imageWidth HorizJoin {outputWidth = w} = w
-- imageWidth VertJoin {outputWidth = w} = w
-- imageWidth BGFill {outputWidth = w} = w
-- imageWidth Crop {outputWidth = w} = w
-- imageWidth EmptyImage = 0

-- -- | The height of an Image. This is the number of display rows the
-- -- image will occupy.
-- imageHeight :: Image -> Int
-- imageHeight HorizText {} = 1
-- imageHeight HorizJoin {outputHeight = h} = h
-- imageHeight VertJoin {outputHeight = h} = h
-- imageHeight BGFill {outputHeight = h} = h
-- imageHeight Crop {outputHeight = h} = h
-- imageHeight EmptyImage = 0

-- | Append in the 'Semigroup' instance is equivalent to '<->'.
instance Semigroup Image where
  Empty <> i = i
  i <> Empty = i
  i <> i' = Compose (getBBox i <> getBBox i') i i'

-- | Append in the 'Monoid' instance is equivalent to '<->'.
instance Monoid Image where
  mempty = Empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance Action (Translation Int) Image where
  act _ Empty = Empty
  act d (Act a c d' t) = Act a (act d c) (d <> d') t
  act d t = Act mempty mempty d t

-- translate d (crop c (translate d' t)) == crop (translate d c) (translate (d <> d') x)

instance Action (Crop Int) Image where
  act _ Empty = Empty
  act c (Act a c' d t) = Act a (c <> c') d t
  act c t = Act mempty c mempty t

instance Action Attr Image where
  act _ Empty = Empty
  act a (Act a' c d t) = Act (a <> a') c d t
  act a t = Act a mempty mempty t

foldImage :: (Action Attr r, Action (Translation Int) r, Action (Crop Int) r, Monoid r) => (Prim -> r) -> Image -> r
foldImage f = go
  where
    go = \case
      Prim p -> f p
      Empty -> mempty
      Compose _ t1 t2 -> go t1 <> go t2
      Act a c d t -> act a (act (act d c) (act d (go t)))

instance Action (Crop Int) (Maybe Prim) where
  act crop = (=<<) \case
    SpanAt {..} ->
      case crop of
        Invalid -> Nothing
        Crop bbox -> do
          guard (bboxTop bbox <= py && py < bboxBottom bbox)
          let left = max px (bboxLeft bbox)
              right = min (px + outputWidth) (bboxRight bbox)
          guard (left < right)
          let str' = clipText pstr (left - px) (right - left)
          pure SpanAt {px = left, pstr = str', ..}
    BGFill box' -> BGFill <$> act crop (Just box')

instance Action (Crop Int) [Prim] where
  act crop = mapMaybe (act crop . Just)

instance Action (Translation Int) Prim where
  act t = \case
    s@SpanAt {px, py} -> s {px = px + translationX t, py = py + translationY t}
    s@BGFill {fillBox} -> s {fillBox = act t fillBox}

instance Action (Translation Int) [Prim] where
  act t = map (act t)

instance Action (Translation Int) (Maybe Prim) where
  act t = fmap (act t)

instance Action Attr Prim where
  act a = \case
    s@SpanAt {pattr} -> s {pattr = a <> pattr}
    s -> s

-- | combines two images side by side
--
-- Combines text chunks where possible. Assures outputWidth and
-- outputHeight properties are not violated.
--
-- The result image will have a width equal to the sum of the two images
-- width. And the height will equal the largest height of the two
-- images. The area not defined in one image due to a height mismatch
-- will be filled with the background pattern.
-- horizJoin :: Image -> Image -> Image
-- horizJoin EmptyImage i = i
-- horizJoin i EmptyImage = i
-- horizJoin i0@(HorizText a0 t0 w0 cw0) i1@(HorizText a1 t1 w1 cw1)
--   | a0 == a1 = HorizText a0 (TL.append t0 t1) (w0 + w1) (cw0 + cw1)
--   -- assumes horiz text height is always 1
--   | otherwise = HorizJoin i0 i1 (w0 + w1) 1
-- horizJoin i0 i1
--   -- If the images are of the same height then no padding is required
--   | h0 == h1 = HorizJoin i0 i1 w h0
--   -- otherwise one of the images needs to be padded to the right size.
--   | h0 < h1 -- Pad i0
--     =
--       let padAmount = h1 - h0
--        in HorizJoin (VertJoin i0 (BGFill w0 padAmount) w0 h1) i1 w h1
--   | h0 > h1 -- Pad i1
--     =
--       let padAmount = h0 - h1
--        in HorizJoin i0 (VertJoin i1 (BGFill w1 padAmount) w1 h0) w h0
--   where
--     w0 = imageWidth i0
--     w1 = imageWidth i1
--     w = w0 + w1
--     h0 = imageHeight i0
--     h1 = imageHeight i1
-- horizJoin _ _ = error "horizJoin applied to undefined values."

-- | combines two images vertically
--
-- The result image will have a height equal to the sum of the heights
-- of both images. The width will equal the largest width of the two
-- images. The area not defined in one image due to a width mismatch
-- will be filled with the background pattern.
-- vertJoin :: Image -> Image -> Image
-- vertJoin EmptyImage i = i
-- vertJoin i EmptyImage = i
-- vertJoin i0 i1
--   -- If the images are of the same width then no background padding is
--   -- required
--   | w0 == w1 = VertJoin i0 i1 w0 h
--   -- Otherwise one of the images needs to be padded to the size of the
--   -- other image.
--   | w0 < w1 =
--       let padAmount = w1 - w0
--        in VertJoin (HorizJoin i0 (BGFill padAmount h0) w1 h0) i1 w1 h
--   | w0 > w1 =
--       let padAmount = w0 - w1
--        in VertJoin i0 (HorizJoin i1 (BGFill padAmount h1) w0 h1) w0 h
--   where
--     w0 = imageWidth i0
--     w1 = imageWidth i1
--     h0 = imageHeight i0
--     h1 = imageHeight i1
--     h = h0 + h1
-- vertJoin _ _ = error "vertJoin applied to undefined values."
