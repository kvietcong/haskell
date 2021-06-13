{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Reanimate
import Reanimate.Scene
import Control.Lens.Operators
import Codec.Picture (PixelRGBA8(..))

-- Honest thoughts about Reanimate:
-- It seems to be a really cool library! I like
-- how Haskell has a way to animate things in such
-- an expressive way. However, for me at this moment,
-- it seems that the user experience isn't that great
-- I'd be happy to revisit it in the future but
-- it is pretty hard to work with rn. This may also
-- have to do with the state of the Haskell ecosystem.
-- It isn't too user friendly so I might just stick
-- to my explorations in category theory rather than
-- trying to do anything user facing with Haskell

equation :: SVG
equation = scale 3 $ center $
    latexAlign "e^{\\pi i} +  1 = 0"

background :: SVG
background = mkBackgroundPixel nord0

environment :: Animation -> Animation
environment = addStatic background
    . mapA (withStrokeWidth 0)
    . mapA (withFillColorPixel nord6)

fadeInOut :: Animation 
fadeInOut = environment $ scene $ do
    wait 0.25

    play $ applyE fadeInE $ staticFrame 0.5 equation
    play $ staticFrame 1 equation
    play $ applyE fadeOutE $ staticFrame 0.5 equation

    wait 0.1

    play $ mkAnimation 2 $ \t ->
        partialSvg t $ pathify $ mkCircle (screenHeight / 3)
    play $ applyE fadeOutE
         $ staticFrame 0.75 $ mkCircle $ screenHeight / 3

    wait 0.1

    obj <- oNew equation
    oShowWith obj oDraw
    oHideWith obj oFadeOut
    
    wait 0.25

main :: IO ()
-- main = reanimate $ playThenReverseA fadeInOut
main = reanimate fadeInOut

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
