{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
--{-# LANGUAGE TemplateHaskell #-}


module Main where

import GHC.Conc (numCapabilities) -- check number of threads available

import Prelude hiding ((<*))
--import Graphics.GPipe.Expr
import Graphics.GPipe

import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)


--type VBuffer = Buffer os (B4 Float, B3 Float)
-- type VShader = (ContextHandler ctx, MonadIO m, MonadException m) => ContextT ctx os m (CompiledShader os x)

main :: IO ()
main = do
  putStrLn $ "Number of threads: " ++ show numCapabilities
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.WindowConfig 500 500 "GPipe Fractal" Nothing [] Nothing)
    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V3 1 0 0)
                               , (V4   1  (-1) 0 1, V3 0 1 0)
                               , (V4   1    1  0 1, V3 0 0 1)
                               , (V4 (-1)   1  0 1, V3 1 1 1)
                               ]

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream
      let fragmentStream2 = myFragmentShader fragmentStream
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream2

    mainLoop vertexBuffer shader win

mainLoop :: (ContextColorFormat c, Color c Float ~ V3 Float) =>
                      Buffer os (B4 Float, B3 Float)
                      -> (PrimitiveArray Triangles (B4 Float, B3 Float) -> Render os ())
                      -> Window os c ds
                      -> ContextT GLFW.Handle os IO ()
mainLoop vertexBuffer shader win = do
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleFan vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    mainLoop vertexBuffer shader win


myFragmentShader :: FragmentStream (V3 (S F Float)) -> FragmentStream (V3 (S F Float))
myFragmentShader = withRasterizedInfo singleFragmentTransformer

singleFragmentTransformer :: V3 (S F Float) -> RasterizedInfo -> V3 (S F Float)
singleFragmentTransformer _ (RasterizedInfo (V4 x y _ _) _ _) = colorFunc $ Complex (x / 125.0 - 2) (y / 125.0 - 2)

colorFunc :: Complex -> V3 (S F Float)
colorFunc c = V3 r' g' b'
  where
    fractl :: S F Int
    fractl = fractal c
    color1 = toFloat fractl / toFloat maxIters
    r' = color1
    g' = color1
    b' = color1

mag :: Complex -> S F Float
mag (Complex r' i') = sqrt (r' * r' + i' * i')

mag2 :: Complex -> S F Float
mag2 (Complex r' i') = (r' * r') + (i' * i')



data Complex = Complex {
  r :: S F Float,
  i :: S F Float
}

cTP :: Complex -> (S F Float, S F Float)
cTP (Complex r' i') = (r', i')

pTC :: (S F Float, S F Float) -> Complex
pTC (r', i') = Complex r' i'

--frIter :: (S F Float, S F Float) -> (S F Float, S F Float) -> (S F Float, S F Float)
frIter :: (S F Float, S F Float) -> (S F Float, S F Float)
--frIter (zr,zi) (cr,ci) = (zr * zr - zi * zi + cr, 2 * zr * zi + ci)
--frIter (zr,zi) = (zr * zr - zi * zi, zr * zi + zr * zi)
--frIter (zr,zi) = (zr - zi, zi + zr) -- already bugged
--frIter (zr',zi') = (0, zi' + zr' + 0.1)
frIter (zr',zi') = (zi' + zr' + 0.1, 0)
-- ( 0.2, 1.75)
-- (-0.2, 1.75)

maxIters :: S F Int
maxIters = 10

myMult :: Complex -> Complex -> Complex
myMult (Complex xr xi) (Complex yr  yi) = Complex (xr * yr - xi * yi) (xr * yi + xi * yr)

myPlus :: Complex -> Complex -> Complex
myPlus (Complex xr xi) (Complex yr  yi) = Complex (xr + yr) (xi + yi)

fractal :: Complex -> S F Int
fractal c = mod' ( fst $ while mcond miter (0, cTP c)) maxIters
  where
      miter :: (FInt, (FFloat, FFloat)) ->  (FInt, (FFloat, FFloat))
--      miter (i', z') = (i' + 1, frIter z' (cTP c))
      miter (i', z') = (i' + 1, frIter z')
      mcond :: (S F Int, (S F Float, S F Float)) -> S F Bool
      mcond (i', z') = (i' <* maxIters) &&* ((mag2 (pTC z')) <* 4.0)
