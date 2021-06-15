{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Reanimate
import Reanimate.Scene
import Control.Lens.Operators
import Codec.Picture (PixelRGBA8(..))

-- My Thoughts on Reanimate:
-- This is a really cool library that I appreciate.
-- It is trying to bring practicality to the
-- Haskell ecosystem with something really cool.

-- However, I am sad to say it is a bit unfriendly
-- to work with. The library is quite hard to learn
-- and working with it seems a little weird. I want
-- to keep on the declarative side, but I feel like
-- it is underpowered. But when I work with the
-- imperative side, I question, why not just work
-- with something easier like Manim? The documentation
-- is also quite lacking in clearly explained examples.

-- Also, there are lenses all over the imperative side.
-- I have been trying to learn Haskell for about a month
-- now but having to also get into lenses seems a bit much.
-- After Functors, Applicatives, Monads, and other Category
-- theory things to learn, Lenses on top of that just
-- makes Haskell seem like a chore to learn. It seems
-- that Lenses are pretty widespread though so I guess
-- I will have to pick them up some day :(

-- I will be glad to revisit this someday though!
-- I feel like with the Haskell Foundation starting
-- to make Haskell more accessible, the whole library
-- ecosystem might start becoming much better to use!
-- Though there is a possibility that the problems
-- I'm facing have been from the fact that I'm
-- learning Haskell sub optimally and that I am not
-- the brightest

equation :: SVG
equation = scale 3 $ center $
    latexAlign "e^{\\pi i} +  1 = 0"

equationParts :: [SVG]
equationParts = coloredParts
    where parts = [snd (splitGlyphs [x] equation) | x <- [0..7]]
          colors = withFillColorPixel <$>
              [nord7, nord9, nord11, nord13, nord15, nord8, nord10]
          coloredParts = [ color part
                         | (color, part) <- zip colors parts]

background :: SVG
background = mkBackgroundPixel nord0

environment :: Animation -> Animation
environment = addStatic background
    . mapA (withStrokeWidth 0)

centerItem :: Duration -> SVG -> Animation
centerItem time svg = scene $ do
    let goCenter t = do
            oCenterX %= \origin -> fromToS origin 0 t
            oCenterY %= \origin -> fromToS origin 0 t

    item <- oNew svg
    oShow item
    oTweenS item time goCenter

fadeInEulerIdenity :: Animation 
fadeInEulerIdenity = environment
    $ pauseAtEnd 0.5
    $ applyE (overEnding 1 fadeOutE)
        $ pauseAtEnd 2
        $ seqA
            ( setDuration 5
            $ foldl1 andThen [applyE fillInE $ staticFrame 1 part
                             | part <- equationParts])
            ( seqA (pauseAtEnd 0.5
                   $ foldl1 parA $ staticFrame 1 (head equationParts)
                   : [applyE fadeOutE $ staticFrame 1 part
                     | part <- tail equationParts])
                   (centerItem 2 $ head equationParts))

main :: IO ()
main = reanimate fadeInEulerIdenity

-- Nord Color Palette
nord0 :: PixelRGBA8
nord0 = PixelRGBA8 46 52 64 255

nord1 :: PixelRGBA8
nord1 = PixelRGBA8 59 66 82 255

nord2 :: PixelRGBA8
nord2 = PixelRGBA8 67 76 94 255

nord3 :: PixelRGBA8
nord3 = PixelRGBA8 76 86 106 255

nord4 :: PixelRGBA8
nord4 = PixelRGBA8 216 222 233 255

nord5 :: PixelRGBA8
nord5 = PixelRGBA8 229 233 240 255

nord6 :: PixelRGBA8
nord6 = PixelRGBA8 236 239 244 255

nord7 :: PixelRGBA8
nord7 = PixelRGBA8 143 188 187 255

nord8 :: PixelRGBA8
nord8 = PixelRGBA8 136 192 208 255

nord9 :: PixelRGBA8
nord9 = PixelRGBA8 129 161 193 255

nord10 :: PixelRGBA8
nord10 = PixelRGBA8 94 129 172 255

nord11 :: PixelRGBA8
nord11 = PixelRGBA8 191 97 106 255

nord12 :: PixelRGBA8
nord12 = PixelRGBA8 208 135 112 255

nord13 :: PixelRGBA8
nord13 = PixelRGBA8 235 203 139 255

nord14 :: PixelRGBA8
nord14 = PixelRGBA8 163 190 140 255

nord15 :: PixelRGBA8
nord15 = PixelRGBA8 180 142 173 255

nordPalette :: [PixelRGBA8]
nordPalette = [ nord0
              , nord1
              , nord2
              , nord3
              , nord4
              , nord5
              , nord6
              , nord7
              , nord8
              , nord9
              , nord10
              , nord11
              , nord12
              , nord13
              , nord14
              , nord15 ]
