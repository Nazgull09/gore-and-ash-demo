module Game.UI(
    module ReExport
  , uiActor
  ) where

import Consts
import Data.Word
import Foreign.C.Types
import Control.Wire
import Control.Wire.Unsafe.Event
import Linear
import Linear.Affine
import Data.Text

import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.SDL 
import Game.GoreAndAsh.Logging
import SDL hiding (Event)

import Game.UI.Data as ReExport
import Graphics.Square

uiActor :: (UIId -> UI) -> AppActor UIId a UI
uiActor initUI = makeActor $ \i -> stateWire (initUI i) (mainController i)
  where 
    mainController :: UIId -> AppWire (a, UI) UI
    mainController i = proc (_, ui) -> do
      liftGameMonad3 drawRectangle -< (uiPos ui, uiSize ui, uiColor ui)
      traceEvent (pack . show) . uiClick -< ui
      returnA -< ui 

    uiClick :: AppWire UI (Event (V2 Double))
    uiClick = proc ui -> do
      ep <- mouseClick ButtonLeft -< ()
      ev <- filterE2 inRect -< (ui, ep)
      mapE2 transRect -< (ui, ev)
      where 
        inRect :: UI -> V2 Double -> Bool
        inRect ui (V2 x y) = let 
          V2 sx sy = uiSize ui 
          V2 px py = uiPos ui
          in x > px && x < px+sx && 
             y > py && y < py+sy

        transRect :: UI -> V2 Double -> V2 Double
        transRect ui (V2 px py) = let
          V2 uix uiy = uiPos ui
          V2 uisx uisy = uiSize ui
          in V2 ((px - uix)/uisx) ((py - uiy)/uisy)
-- | TODO: move to core
filterE2 :: (a -> b -> Bool) -> AppWire (a, Event b) (Event b)
filterE2 f = arr $ \(a, eb) -> case eb of
  NoEvent -> NoEvent
  Event b -> if f a b then Event b else NoEvent 

mapE2 :: (a -> b -> c) -> AppWire (a, Event b) (Event c)
mapE2 f = arr $ \(a, eb) -> case eb of
  NoEvent -> NoEvent
  Event b -> Event (f a b) 

-- | Draw rectangle to main window
--
-- TODO: move to separate module Graphics.Rectangle
drawRectangle :: MonadSDL m 
  => V2 Double -- ^ Position
  -> V2 Double -- ^ Size
  -> V3 Double -- ^ Color RGB
  -> GameMonadT m ()
drawRectangle pos size col = do
  mwr <- sdlGetWindowM mainWindowName
  case mwr of 
    Nothing -> return ()
    Just (w, r) -> do 
      wsize <- fmap (fmap fromIntegral) . get $ windowSize w
      rendererDrawColor r $= transColor col 
      fillRect r $ Just $ transformedSquare wsize
  where
    transColor :: V3 Double -> V4 Word8
    transColor (V3 r g b) = V4 (round $ r * 255) (round $ g * 255) (round $ b * 255) 255

    modelMtx :: V2 Double -> M33 Double 
    modelMtx wsize = viewportTransform2D 0 wsize !*! translate2D pos

    transformedSquare :: V2 Double -> Rectangle CInt
    transformedSquare wsize = Rectangle (P topleft) (botright - topleft) 
      where
      topleft = fmap round . applyTransform2D (modelMtx wsize) $ 0
      botright = fmap round . applyTransform2D (modelMtx wsize) $ size 