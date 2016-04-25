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
  runContextT GLFW.newContext (ContextFormatColor RGB8) $ do  
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
      drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream2
      
    mainLoop vertexBuffer shader   
    
mainLoop vertexBuffer shader = do    
  render $ do   
    clearContextColor (V3 0 0 0)   
    vertexArray <- newVertexArray vertexBuffer  
    let primitiveArray = toPrimitiveArray TriangleFan vertexArray  
    shader primitiveArray   
  swapContextBuffers  
    
  closeRequested <- GLFW.windowShouldClose   
  unless closeRequested $  
    mainLoop vertexBuffer shader   



myFragmentShader = withRasterizedInfo singleFragmentTransformer

singleFragmentTransformer :: V3 (S F Float) -> RasterizedInfo -> V3 (S F Float)
singleFragmentTransformer _ (RasterizedInfo (V4 x y _ _) _ _) = colorFunc (x / 100.0) (y / 100.0)

colorFunc :: (Floating a, OrdB a) => a -> a -> V3 a
colorFunc x y = V3 r g b
  where
    --z :: (Num b) => Complex b
    z = 0.0 :+ 0.0
    c = ((x/10.0) -0.2) :+ (y/10.0)
    fractl = fractal c z 0.0
    col1 = (\( _ , iter) -> iter) fractl
    r = col1
    g = 0.5 + 0.5 * cos (10.0 * y)
    b = 0.5 + 0.5 * cos (10.0 * (Main.magnitude (myMult z z)))

magnitude :: (Floating a) => Complex a -> a
magnitude (r :+ i) = sqrt (r * r + i * i)

{-
myComplex :: (Floating a) => a -> a -> Complex a
myComplex r i = r :+ i
-}

maxIters :: Int
maxIters = 100

myMult :: (Floating a) => Complex a -> Complex a -> Complex a
myMult (xr :+ xi) (yr :+ yi) = (xr * yr - xi * yi) :+ (xr * yi + xi * yr)

myPlus :: (Floating a) => Complex a -> Complex a -> Complex a
myPlus (xr :+ xi) (yr :+ yi) = (xr + yr) :+ (xi + yi)

fractal :: (Floating a, OrdB a) => Complex a -> Complex a -> a -> (Complex a, a)
fractal c z iter -- = ((1.0 :+ 1.0), 0.0)
--  | iter >= fromIntegral(maxIters)   = ((1.0 :+ 1.0), 0.0)
--  | ((realPart (myMult z z)) > 4.0) = ((myPlus (myMult z z) c), iter)
  | otherwise                         = fractal c (myPlus (myMult z z) c) (iter + 1)

