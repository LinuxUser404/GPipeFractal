{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
--{-# LANGUAGE TemplateHaskell #-}

module Main where

import GHC.Conc (numCapabilities) -- check number of threads available

import Prelude hiding ((<*))
import Graphics.GPipe   
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW  
import Control.Monad (unless)  

main = do
  putStrLn $ "Number of threads: " ++ show numCapabilities
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "OpenGL Graphics")
    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 4  
    writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V3 1 0 0)  
                               , (V4   1  (-1) 0 1, V3 0 1 0)  
                               , (V4   1    1  0 1, V3 0 0 1)  
                               , (V4 (-1)   1  0 1, V3 1 1 1)  
                               ]  
                        
    shader <- compileShader $ do  
      primitiveStream <- toPrimitiveStream id
      fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 1920 1080), DepthRange 0 1)) primitiveStream
      let fragmentStream2 = myFragmentShader fragmentStream
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream2
      
    mainLoop vertexBuffer shader win
    
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



myFragmentShader = withRasterizedInfo singleFragmentTransformer

singleFragmentTransformer :: V3 (S F Float) -> RasterizedInfo -> V3 (S F Float)
singleFragmentTransformer _ (RasterizedInfo (V4 x y _ _) _ _) = colorFunc $ Complex (x / 2000.0 - 2.0) (y / 1000.0 - 0.5)

colorFunc :: Complex -> V3 (S F Float)
colorFunc c = V3 r g b
  where
    z = Complex 0.0 0.0
    fractl :: S F Int
    fractl = fractal c z
    r = 0.0
    g = (/64) $ toFloat $ mod' 256 fractl
    b = 0.0

magnitude :: Complex -> S F Float
magnitude (Complex r i) = sqrt (r * r + i * i)

magnitude2 :: Complex -> S F Float
magnitude2 (Complex r i) = r * r + i * i



data Complex = Complex {
  r :: S F Float,
  i :: S F Float
}

complexToPair :: Complex -> (S F Float, S F Float)
complexToPair (Complex r i) = (r, i)

pairToComplex :: (S F Float, S F Float) -> Complex
pairToComplex (r, i) = Complex r i



maxIters :: S F Int
maxIters = 2^8

myMult :: Complex -> Complex -> Complex
myMult (Complex xr xi) (Complex yr  yi) = Complex (xr * yr - xi * yi) (xr * yi + xi * yr)

myPlus :: Complex -> Complex -> Complex
myPlus (Complex xr xi) (Complex yr  yi) = Complex (xr + yr) (xi + yi)

fractal :: Complex -> Complex -> S F Int
fractal c (Complex zr zi) = fst $ while mcond miter ( 0, (zr,zi))
  where
      miter :: (S F Int, (S F Float, S F Float)) ->  (S F Int, (S F Float, S F Float))
      miter (i', (zr', zi')) = (i' + 1, complexToPair $ myPlus (myMult (Complex zr' zi') (Complex zr' zi')) c)
      mcond :: (S F Int, (S F Float, S F Float)) -> S F Bool
      mcond (i', (zr', zi')) = (i' <* maxIters) &&* ((magnitude2 (Complex zr' zi')) <* 1024.0)
