{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.Palette.RandomColor
import Data.Colour.Palette.Types
import System.Random
import Control.Monad.Random
import Control.Monad.State.Lazy
import Test.QuickCheck as QC (Gen, generate, elements, vectorOf, infiniteListOf)


tileUL :: Kolor -> Kolor -> QDiagram B V2 Double Any
tileUL c1 c2 = center (my_triangle <> my_rect)
  where
    my_triangle = stroke (closeLine (lineFromOffsets [0 ^& 1, 1.2 ^& 0])) # lw 0 # fc c1 # alignTL
    my_rect = rect 1.2 1 # lw 0 # fc c2 # alignTL

tileUR :: Kolor -> Kolor -> QDiagram B V2 Double Any
tileUR c1 c2 = reflectX $ tileUL c1 c2
tileLL :: Kolor -> Kolor -> QDiagram B V2 Double Any
tileLL c1 c2 = reflectY $ tileUL c1 c2
tileLR :: Kolor -> Kolor -> QDiagram B V2 Double Any
tileLR c1 c2 = (reflectX . reflectY) $ tileUL c1 c2

data Twist = LeftTwist | RightTwist deriving (Eq, Show)

data Row = Row 
    { polarity :: Bool
    , twists :: [Twist]
    } deriving (Eq, Show)

newtype Rug = Rug [Row]

flip_twist LeftTwist  = RightTwist
flip_twist RightTwist = LeftTwist

flip_twists row = Row (polarity row) (flip_twist <$> (twists row))

flip_polarity :: Row -> Row
flip_polarity (Row pol ts) = Row (not pol) ts

row_ld_even = cat (r2 (1, 0)) $ take 58 $ cycle [tileUL blue white, tileLR blue white]
row_ld_odd  = cat (r2 (1, 0)) $ take 58 $ cycle [tileLR blue white, tileUL blue white]

row_rd_even = reflectY row_ld_even
row_rd_odd  = reflectY row_ld_odd

rug = cat (r2 (0, -1)) $ take 126 $ cycle[row_ld_even, row_ld_odd]

xor a b = a /= b

row2diagram (Row pol ts) flip = cat (r2 (1, 0)) $ fmap go enumed_cells
    where
      enumed_cells = zip ((xor pol) <$> cycle [False, True]) ts
      go (b, LeftTwist)  = if (b `xor` flip) then (tileLL blue red) else (tileUR blue red)
      go (b, RightTwist) = if (b `xor` flip) then (tileUL blue red) else (tileLR blue red)


rug2diagram rug = cat (r2 (0, -1)) $ [row2diagram r flip | (flip, r) <- enumedrows]
    where 
      enumedrows = zip (cycle [True, False]) rug

boring_rug = replicate 126 $ Row True $ replicate 58 RightTwist

rug1 = replicate 126 $ Row True $ take 58 $ cycle $ (replicate 7 RightTwist) ++ (replicate 2 LeftTwist)

rug2 = fmap go $ zip [0..] rug1
    where
      go (n, row) = if even (n `div` 6) then (flip_polarity . flip_twists) row else row

rug3 = fmap go $ zip (cycle [True, True, False, False, False, False, False]) rug1
    where
      go (flp, row) = if flp then (flip_polarity . flip_twists) row else  row

rug4 = (replicate 63 baserow) ++ (replicate 63 ((flip_polarity . flip_twists) baserow ))
  where
    base = cycle $ (replicate 7 RightTwist) ++ (replicate 2 LeftTwist)
    baserow = Row True $ (take 29 base) ++ (reverse (flip_twist <$> (take 29 base)))

out_diagram = strutX 10 ||| (rug2diagram boring_rug) ||| strutX 10 ||| (rug2diagram rug1) ||| strutX 10
              ||| (rug2diagram rug2) ||| strutX 10 ||| (rug2diagram rug3) ||| strutX 10 ||| (rug2diagram rug4)

out_diagram2 = rug2diagram boring_rug

scrap_lengths = [34,36..45]
random_length = QC.elements scrap_lengths
random_lengths = generate $ QC.infiniteListOf random_length

infinite_weft :: [Int] -> [a] -> [a]
infinite_weft is xs = concat $ zipWith replicate is xs

random_hue = generate $ QC.elements all_hues
all_hues = [HueRed, HueOrange, HueYellow, HueGreen, HueBlue, HuePurple, HuePink]

randomColors hue lum = sequence (replicate 7 (randomColor hue lum))

weft_palette :: [Kolor] -> QDiagram B V2 Double Any
weft_palette ks = foldr go (mempty ::  QDiagram B V2 Double Any) ks
  where
    go k dia = dia ||| (square 2 # fc k # lw none)

data ShuttleState = ShuttleState 
    { prev_was_dark :: Bool -- true if previous twist ended in a dark weft. False if light
    } deriving (Eq, Show)

data LoomState = LoomState 
    { row_dir_ltor :: Bool
    , prevrow_bottom_dark :: Bool
    } deriving (Eq, Show)

weave_cell LeftTwist  c1 c2 = tileUL c1 c2
weave_cell RightTwist c1 c2 = tileLL c1 c2

weave_row :: [Twist] -> [(Kolor, Kolor)] -> QDiagram B V2 Double Any
weave_row twists cs = centerX $ hcat $ zipWith3 go twists cs flips
  where
    go t (c1, c2) flp = flp $ weave_cell t c1 c2
    flips =  cycle [id, (reflectX . reflectY)]

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs = take n xs : splitList n (drop n xs)

--[[(Kolor, Kolor)] -> QDiagram B V2 Double Any]


weave_rug :: [[Twist]] -> [(Kolor, Kolor)] -> QDiagram B V2 Double Any 
weave_rug twistss ccs = vcat $ zipWith ($) flipLR rows
  where
    rows = (take 126 $ (weave_row <$> twistss) <*> split_ccs)
    swap (a, b) = (b, a)
    split_ccs = zipWith ($) (cycle [fmap id, fmap swap]) $ take 126 $ splitList 58 ccs
    flipLR = cycle [id,  reflectX . reflectY]

random5rugs :: IO (QDiagram B V2 Double Any)
random5rugs = do
  w0 <-  twoRandomWefts
  w1 <-  twoRandomWefts
  w2 <-  twoRandomWefts
  w3 <-  twoRandomWefts
  w4 <-  twoRandomWefts
  return $ hsep 2 [weave_rug boring_rug2 w0, weave_rug boring_rug2 w1, weave_rug boring_rug2 w2, weave_rug boring_rug2 w3, weave_rug boring_rug2 w4]

twoRandomWefts :: IO ([(Kolor, Kolor)])
twoRandomWefts = do
  r_hue1 <- random_hue
  r_hue2 <- random_hue
  colors1 <- evalRandIO (randomColors r_hue1 LumBright)
  colors2 <- evalRandIO (randomColors r_hue2 LumDark)
  lens1 <- random_lengths
  lens2 <- random_lengths
  let weft1 = drop 1 $ infinite_weft lens1 (cycle colors1)
      weft2 = infinite_weft lens2 (cycle colors2)
  return $ zip weft1 weft2

boring_rug2 = replicate 126 $ replicate 58 RightTwist

main :: IO ()
main = do
  wefts <- twoRandomWefts
  r2 <- random5rugs
  renderSVG "/mnt/c/Users/dansv/Desktop/test.svg" (mkSizeSpec2D (Just 3500) (Just 2200)) r2
  --renderSVG "/mnt/c/Users/dansv/Desktop/test.svg" (mkSizeSpec2D (Just 3500) (Just 2200)) ((weft_palette (take 100 weft1)) === (weft_palette (take 100 weft2)) )
  --renderSVG "/mnt/c/Users/dansv/Desktop/test2.svg" (mkSizeSpec2D (Just 300) (Just 220)) $ out_diagram
