{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
--{-# LANGUAGE TemplateHaskell #-}

module Main where

import GHC.Conc (numCapabilities) -- check number of threads available
   
import Graphics.GPipe   
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW  
import Control.Monad (unless)  
import Data.Complex
  
main =    do
  putStrLn $ "Number of threads: " ++ show numCapabilities
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "GPipeFractal")
    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 4  
    writeBuffer vertexBuffer 0 [ (V4 (-1) (-1) 0 1, V3 1 0 0)  
                               , (V4   1  (-1) 0 1, V3 0 1 0)  
                               , (V4   1    1  0 1, V3 0 0 1)  
                               , (V4 (-1)   1  0 1, V3 1 1 1)  
                               ]  
                        
    shader <- compileShader $ do  
      primitiveStream <- toPrimitiveStream id  
      fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 600), DepthRange 0 1)) primitiveStream
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
singleFragmentTransformer _ (RasterizedInfo (V4 x y _ _) _ _) = colorFunc (x / 100.0) (y / 100.0)

colorFunc :: (Floating a, OrdB a) => a -> a -> V3 a
colorFunc x y = V3 r g b
  where
    --z :: (Num b) => Complex b
    z = x :+ y
    c = ((x/10.0) -0.2) :+ (y/10.0)
--    fractl = fractal c z 0.0
--    col1 = (\( _ , iter) -> iter) fractl
    r = 0.5 + 0.5 * cos (10.0 * x)
    g = 0.5 + 0.5 * cos (10.0 * y)
    b = 0.5 + 0.5 * cos (10.0 * (Main.magnitude (myMult z z)))

magnitude :: (Floating a) => Complex a -> a
magnitude (r :+ i) = sqrt (r * r + i * i)

maxIters :: Int
maxIters = 100

myMult :: (Floating a) => Complex a -> Complex a -> Complex a
myMult (xr :+ xi) (yr :+ yi) = (xr * yr - xi * yi) :+ (xr * yi + xi * yr)

myPlus :: (Floating a) => Complex a -> Complex a -> Complex a
myPlus (xr :+ xi) (yr :+ yi) = (xr + yr) :+ (xi + yi)

