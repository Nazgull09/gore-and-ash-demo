module Game.UI.Data(
    UI(..)
  , UIId(..)
  , UIMessage(..)
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Typeable
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Actor

data UI = UI {
  uiId :: !UIId
, uiPos :: !(V2 Double)
, uiColor :: !(V3 Double) 
, uiSize :: !(V2 Double)
} deriving (Generic, Show)

instance NFData UI 

newtype UIId = UIId { unUIId :: Int } deriving (Eq, Show, Generic)
instance NFData UIId 
instance Hashable UIId 

data UIMessage = UIMessageStub deriving (Typeable, Generic)
instance NFData UIMessage 

instance ActorMessage UIId where
  type ActorMessageType UIId = UIMessage
  toCounter = unUIId
  fromCounter = UIId