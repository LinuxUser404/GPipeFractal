{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

module Main where


import GHC.Conc (numCapabilities) -- check number of threads available
{-import Codec.Picture  (writePng, Pixel, Image)
import Codec.Picture.Types (unsafeFreezeImage, unsafeWritePixel, MutableImage( .. ))
import Control.Monad.ST( ST, runST )
import Control.Monad( foldM, liftM, ap )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Graphics.GPipe   -- also imports linear and boolean
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW  as GLFW  
import Control.Monad (unless)  -}

{-
import Codec.Picture  (generateImage, writePng)
import Data.Word      (Word8)
-}

--import Data.Complex   (Complex(..), magnitude)

import Graphics.GPipe    
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW   
import "lens" Control.Lens
import Control.Monad (unless)   
import Data.Word (Word32)
import Control.Applicative (pure)
import Data.Monoid (mappend)

{-
runContextT factory format monadAction
B4 Float - buffer type that contains 4 float values
-}

main :: IO ()
main =    do
  putStrLn $ "Number of threads: " ++ show numCapabilities
  runContextT GLFW.newContext (ContextFormatColor RGB8) $ do    
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4    
    writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]  
    tex <- newTexture2D R8 (V2 8 8) 1  
    let whiteBlack = cycle [minBound,maxBound] :: [Word32]  
        blackWhite = tail whiteBlack  
    writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))   
    shader <- compileShader $ myShader tex

    renderLoop $ do    
      clearContextColor 0.5   
      vertexArray <- newVertexArray vertexBuffer    
      shader (toPrimitiveArray TriangleStrip vertexArray)  
   
myShader tex = do    
      primitiveStream <- toPrimitiveStream id  
      let primitiveStream2 = fmap (\pos2d -> (make3d pos2d, pos2d)) primitiveStream  
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 width height), DepthRange 0 1)) primitiveStream2  
      let filter = SamplerFilter Nearest Nearest Nearest Nothing
          edge = (pure Repeat, undefined)
      samp <- newSampler2D (const (tex, filter, edge))
      let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing  
          fragmentStream2 = fmap sampleTexture fragmentStream  
      drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2  
   
make3d (V2 x y) = projMat !*! viewMat !* V4 x y 0 1  
  where       
    viewMat = lookAt' (V3 0.5 0.5 (-1.0)) (V3 0.5 0.5 1.0) (V3 0.0 1.0 0.0)   
    projMat = perspective (pi/2) (16.0/9.0) 1 100   
   
renderLoop rendering = do   
  render rendering  
  swapContextBuffers    
  closeRequested <- GLFW.windowShouldClose    
  unless closeRequested $    
    renderLoop rendering  
     
-- Copy of lookAt from linear with normalize replaced with signorm   
lookAt' eye center up =  
  V4 (V4 (xa^._x) (xa^._y) (xa^._z) xd)  
     (V4 (ya^._x) (ya^._y) (ya^._z) yd)  
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)  
     (V4 0     0     0     1)  
  where za = signorm $ center - eye  
        xa = signorm $ cross za up  
        ya = cross xa za  
        xd = -dot xa eye  
        yd = -dot ya eye  
        zd = dot za eye     

width :: Int
width = 800
height :: Int
height = 450
aspect :: Float
aspect = (fromIntegral(width) / fromIntegral(height))

maxIters :: Int
maxIters = 1000

{-
fractal :: RealFloat a => Complex a -> Complex a -> Int -> (Complex a, Int)
fractal c z iter
    | iter >= maxIters = (1 :+ 1, 0)  -- invert values inside the holes
    | magnitude z > 2  = (z', iter)
    | otherwise        = fractal c z' (iter + 1)
  where
    z' = z * z + c

realize :: RealFloat a => (Complex a, Int) -> a
realize (z, iter) = (fromIntegral iter - log (log (magnitude z))) /
                     fromIntegral maxIters
-}

{-
main = do   
  putStrLn $ "Number of threads: " ++ show numCapabilities
  mainLoop
  -}
{-
mainLoop = runContextT GLFW.newContext (ContextFormatColor RGB8) $ do   
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4   
    writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
    tex <- newTexture2D R8 (V2 8 8) 1
    let whiteBlack = cycle [minBound,maxBound] :: [Word32]
        blackWhite = tail whiteBlack
    writeTexture2D tex 0 0 (V2 8 8) (cycle (take 8 whiteBlack ++ take 8 blackWhite))
    colorTex <- newTexture2D RG8 (V2 width height) 1
    --depthTex <- newTexture2D Depth16 (V2 width height) 1
    --shader1 <- compileShader $ makeShader1 tex
    --shader2 <- compileShader $ makeShader2 colorTex
    --renderLoop render shader renderer1 vertexBuffer colorTex depthTex shader1
-}
{-
makeShader1 tex = do
  texMappedFragmentStream <- getProjectedFragments 256 (V3 0.5 (-0.8) (-0.8)) (V3 0.5 0.5 0) (V3 0 1 0)  textureMappedPrimitives
  solidFragmentStream <- getProjectedFragments 256 (V3 (-0.6) (-0.6) 0.8) (V3 0.25 0.25 0) (V3 0 1 0) solidPrimitives
  let filter = SamplerFilter Nearest Nearest Nearest Nothing
      edge = (pure ClampToEdge, 0)
  samp <- newSampler2D (const (tex, filter, edge))
  let sampleTexture = sample2D samp SampleAuto Nothing Nothing
      texMappedFragmentStream2 = filterFragments ((>* 0.5) . sampleTexture) texMappedFragmentStream           
      texMappedFragmentStream3 = fmap (const (V2 1 0)) texMappedFragmentStream2
      solidFragmentStream2 = fmap (const (V2 0 1)) solidFragmentStream
      fragmentStream = solidFragmentStream2 `mappend` texMappedFragmentStream3
      fragmentStream2 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream 
  draw (\s -> (NoBlending, depthImage s, DepthOption Less True)) fragmentStream2 $ \ a -> do
    drawColor (\ s -> (colorImage s, pure True, False)) a
-}
{-
makeShader2 colorTex = do   
      fragmentStream <- getProjectedFragments 800 (V3 1 2 2) (V3 0.5 0.5 0) (V3 0 1 0) id
     
      let filter = SamplerFilter Linear Linear Nearest Nothing
          edge = (pure ClampToEdge, 0)
      samp <- newSampler2D (const (colorTex, filter, edge))
      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = fmap ((\(V2 r g) -> V3 r 0 g) . sampleTexture) fragmentStream           
      drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2
-}
