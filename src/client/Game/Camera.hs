module Game.Camera(
    Camera(..)
  , CameraId(..)
  , cameraWire
  , cameraMatrix
  , cameraToWorld
  , cameraFromWorld
  ) where

import Control.DeepSeq
import Control.Lens 
import Control.Wire
import Control.Wire.Unsafe.Event
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.SDL

data Camera = Camera {
  cameraId :: !CameraId
, cameraPos :: !(V2 Double)
, cameraZoom :: !Double
} deriving (Generic)

instance NFData Camera 

newtype CameraId = CameraId { unCameraId :: Int } deriving (Eq, Show, Generic)
instance NFData CameraId 

data CameraMessage

instance ActorMessage CameraId where
  type ActorMessageType CameraId = CameraMessage
  toCounter = unCameraId
  fromCounter = CameraId

cameraWire :: (CameraId -> Camera) -> AppActor CameraId a Camera 
cameraWire initialCamera = makeActor $ \i -> stateWire (initialCamera i) $ proc (_, c) -> do 
  forceNF
    . moveCamera (V2 0 (-cameraSpeed)) ScancodeS 
    . moveCamera (V2 0 cameraSpeed) ScancodeW 
    . moveCamera (V2 cameraSpeed 0) ScancodeD
    . moveCamera (V2 (-cameraSpeed) 0) ScancodeA
    . zoomCamera 0.1 -< c
  where 
    cameraSpeed :: Double 
    cameraSpeed = 0.1

    moveCamera :: V2 Double -> Scancode -> AppWire Camera Camera
    moveCamera dv k = proc c -> do 
      e <- keyPressing k -< ()
      let newCam = c {
            cameraPos = cameraPos c + dv 
          }
      returnA -< event c (const newCam) e

    zoomCamera :: Double -> AppWire Camera Camera 
    zoomCamera z = proc c -> do 
      e <- mouseScrollY -< ()
      let newCam k = c {
            cameraZoom = min 3 $ max 0.01 $ cameraZoom c + 0.2 * z * k
          }
      returnA -< event c (newCam . fromIntegral) e 

-- | Calculate transformation matrix for camera
cameraMatrix :: Camera -> M33 Double
cameraMatrix Camera{..} = translate2D (V2 (-cameraPos^._x) (-cameraPos^._y))
  !*! scale2D (V2 (-cameraZoom) (-cameraZoom))

-- | Transform camera local coords to world
cameraToWorld :: Camera -> V2 Double -> V2 Double
cameraToWorld c v = inv33 (cameraMatrix c) `applyTransform2D` v

-- | Transform world coords to camera coords
cameraFromWorld :: Camera -> V2 Double -> V2 Double 
cameraFromWorld c v = cameraMatrix c `applyTransform2D` v