-- port of hyperlith game-of-life https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Data.Text as T

import Control.Concurrent.STM qualified as STM -- this is required to create effectfull monad in IO / main
import Control.Lens
import Control.Monad (forM_, zipWithM_)
import Data.Text (pack)
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (send)
import Effectful.Labeled.State (modify)
import Effectful.Reader.Dynamic
import Web.Atomic.CSS
import Web.Hyperbole as Hyperbole
import Web.Hyperbole.Effect.Client (trigger)
import Web.Hyperbole.Effect.Hyperbole
import Web.Hyperbole.Effect.OAuth2 (AuthFlow (state))
import Web.Hyperbole.HyperView (ViewAction (toAction), encodeAction)
import Web.Hyperbole.HyperView.ViewId
import Web.Hyperbole.Server.Socket (sendUpdateView)
import Web.Hyperbole.Types.Event

-- import Data.Foldable (forM_)

port :: Int
port = 3000
boardSize :: Int
boardSize = 50
_s :: Length
_s = PxRem 20
black :: HexColor
black = HexColor "#000FA"

main :: IO ()
main = do
  putStrLn $ "Listening on port: " <> show port
  board <- STM.newTVarIO defaultState
  run port $ do
    -- liveApp quickStartDocument (runPage page)
    liveApp quickStartDocument (runReader board . runConcurrent $ runPage page)

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar GameState) :> es) => Page es '[GamePage]
page = do
  board <- getState
  pure $ do
    el ~ bold . fontSize 24 $
      "Game of Life"
    hyper GamePage $ boardView board

-- hyper board ~ bold $ messageView "Hello"

-- hyper Message2 ~ bold $ messageView "World!"

data Color = Red | Green | Blue
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToParam, FromParam)

instance ToColor Color where
  colorValue Red = "#FF0000"
  colorValue Green = "#00FF00"
  colorValue Blue = "#0000FF"

data Cell = Dead | Alive Color
  -- deriving (Show, Generic, ViewId)
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToParam, FromParam)
toggleCell :: Cell -> Cell
toggleCell Dead = Alive Red
toggleCell (Alive c) = Dead
newtype GameState = GameState [[Cell]]
  deriving (Show, Read, Eq, Generic)

data GamePage = GamePage -- this is like a ViewModel binding in MVVM? Seems like it dosn't need to have an actual value
  deriving (Generic, ViewId)

-- instance Default GameState where
--   def = [[Dead | _ <- [1 .. boardSize]] | _ <- [1 .. boardSize]]
defaultState :: GameState
defaultState = GameState [[Dead | _ <- [1 .. boardSize]] | _ <- [1 .. boardSize]]

getState :: (Concurrent :> es, Reader (TVar GameState) :> es) => Eff es GameState
getState = readTVarIO =<< ask

modifyState :: (Concurrent :> es, Reader (TVar GameState) :> es) => (GameState -> GameState) -> Eff es GameState
modifyState f = do
  var <- ask
  atomically $ do
    modifyTVar var f
    readTVar var

toggleCellAt :: Int -> Int -> GameState -> GameState
toggleCellAt x y (GameState board) =
  -- let newCell = toggleCell $ board !! x !! y
  GameState $ board & ix y . ix x %~ toggleCell

-- data PageActions
--   = SyncFromServer
--   | TapCell Int Int

instance (Reader (TVar GameState) :> es, Concurrent :> es) => HyperView GamePage es where
  -- data Action GamePage = SyncFromServer GameState | TapCell Int Int
  data Action GamePage = FetchState | TapCell Int Int
    deriving (Generic, ViewAction)

  update (TapCell x y) = do
    -- board <- getState -- TODO: update state
    -- pure $ cellView x y $ toggleCell $ board !! x !! y
    board <- modifyState (toggleCellAt x y)
    -- sendUpdate board
    pure $
      boardView board
  update FetchState = do
    boardView <$> getState

-- update (SyncFromServer st) = do
--   pure $ boardView st

cellView :: Int -> Int -> Cell -> View GamePage ()
cellView y x cell = do
  button (TapCell x y)
    ~ border 1
    ~ bg (case cell of Dead -> black; Alive c -> colorValue c)
    ~ height _s
    ~ width _s
    $ text
    $ case cell of
      Dead -> "X"
      Alive _ -> "O"

boardView :: GameState -> View GamePage ()
boardView (GameState state) = do
  el @ onLoad FetchState 1000 $ do
    zipWithM_
      ( \y r ->
          zipWithM_ (cellView y) [0 ..] r
      )
      [0 ..]
      state

-- there is no way to push / broadcast updates see https://github.com/seanhess/hyperbole/issues/36
-- sendUpdate :: (Hyperbole :> es, Reader (TVar GameState) :> es) => GameState -> Eff es ()
-- sendUpdate st =
--   let target = TargetViewId $ encodeViewId GamePage
--    in do
--         -- st <- getState -- read TVar
--         send $ TriggerAction target $ toAction $ SyncFromServer st
