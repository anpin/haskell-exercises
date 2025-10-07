-- Simple game of life implementation
-- inspired by hyperlith game-of-life example
-- https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Control.Concurrent.STM qualified as STM -- this is required to create effectfull monad in IO / main
import Control.Lens
import Control.Lens.Internal.Deque qualified as V.Vector
import Control.Monad (forM_, forever, zipWithM_)
import Data.ByteString.Char8 as Bs
import Data.FileEmbed
import Data.List.NonEmpty qualified as Vector
import Data.String qualified as Data.ByteString
import Data.Text (pack)
import Data.Text as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (send)
import Effectful.Labeled.State (modify)
import Effectful.Reader.Dynamic
import Web.Atomic (ToClassName (toClassName), ToStyle (style))
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
boardSize = 30
black :: HexColor
black = HexColor "#000FA"
__css :: View c ()
__css =
  Hyperbole.style $
    Bs.concat
      [ ".cell { transition: background 0.1s ease; }"
      , Bs.pack $ ".board { background: grey; display: grid; aspect-ratio: 1/1; grid-template-rows: repeat(" ++ show boardSize ++ ", 1fr); grid-template-columns: repeat(" ++ show boardSize ++ ", 1fr); }"
      ]
main :: IO ()
main = do
  Prelude.putStrLn $ "Listening on port: " <> show port
  board <- STM.newTVarIO defaultState
  Prelude.putStrLn "Default state set!"
  -- runEff $ runConcurrent $ runReader board $ startBackgroundLoop 500_000
  -- Prelude.putStrLn "Background loop started!"
  run port $ do
    -- liveApp quickStartDocument (runPage page)
    -- liveApp quickStartDocument (runReader board . runConcurrent $ runPage page)
    liveApp quickStartDocument $ runReader board . runConcurrent $ do
      startBackgroundLoop 500_000
      runPage page

page :: (Hyperbole :> es, Concurrent :> es, Reader (TVar GameState) :> es) => Page es '[GamePage]
page = do
  board <- getState
  pure $ do
    __css
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
  deriving (Show)
toggleCell :: Cell -> Cell
toggleCell Dead = Alive Red
toggleCell (Alive c) = Dead
newtype GameState = GameState (V.Vector Cell)
  deriving (Show)

data GamePage = GamePage -- this is like a ViewModel binding in MVVM? Seems like it dosn't need to have an actual value
  deriving (Generic, ViewId)

-- instance Default GameState where
--   def = [[Dead | _ <- [1 .. boardSize]] | _ <- [1 .. boardSize]]
defaultState :: GameState
defaultState =
  let mid = (boardSize `div` 2)
   in insertRow 1 1 glider $
        insertRow (boardSize - 4) (boardSize - 4) glider $
          insertRow mid mid glider $
            GameState $
              V.generate (boardSize * boardSize) (const Dead)

getState :: (Concurrent :> es, Reader (TVar GameState) :> es) => Eff es GameState
getState = readTVarIO =<< ask

modifyState :: (Concurrent :> es, Reader (TVar GameState) :> es) => (GameState -> GameState) -> Eff es GameState
modifyState f = do
  var <- ask
  atomically $ do
    modifyTVar' var f
    readTVar var
type XCoord = Int
type YCoord = Int
toggleCellAt :: XCoord -> YCoord -> GameState -> GameState
toggleCellAt x y (GameState board) =
  GameState $ board & ix (idx x y) %~ toggleCell

instance (Reader (TVar GameState) :> es, Concurrent :> es) => HyperView GamePage es where
  data Action GamePage = FetchState | TapCell Int Int
    deriving (Generic, ViewAction)

  update (TapCell x y) = do
    boardView <$> modifyState (toggleCellAt x y)
  update FetchState = do
    boardView <$> getState

cellView :: Int -> Int -> Cell -> View GamePage ()
cellView y x cell = do
  button (TapCell x y)
    ~ border 1
    ~ bg (case cell of Dead -> black; Alive c -> colorValue c)
    ~ cls "cell"
    $ text ""

-- \$ case cell of
--   Dead -> "X"
--   Alive _ -> "O"

boardView :: GameState -> View GamePage ()
boardView (GameState state) =
  do
    -- el $ text $ pack $ show state -- use me  for debug
    el
    @ onLoad FetchState 500
    ~ cls "board"
    $ zipWithM_ -- both imapF_ and V.zipWithM_ show empty page, so we convert to list
      ( \i c ->
          let (x, y) = idxToCoord i
           in cellView y x c
      )
      ([0 ..])
      (V.toList state)

-- there is no way to push / broadcast updates see https://github.com/seanhess/hyperbole/issues/36
-- sendUpdate :: (Hyperbole :> es, Reader (TVar GameState) :> es) => GameState -> Eff es ()
-- sendUpdate st =
--   let target = TargetViewId $ encodeViewId GamePage
--    in do
--         -- st <- getState -- read TVar
--         send $ TriggerAction target $ toAction $ SyncFromServer st

startBackgroundLoop :: (Concurrent :> es, Reader (TVar GameState) :> es) => Int -> Eff es ()
startBackgroundLoop d = do
  _ <- async $ forever $ do
    _ <- modifyState gofStep
    threadDelay d
  pure ()

gofStep :: GameState -> GameState
gofStep state =
  GameState $
    V.imap
      ( \i cell ->
          let (x, y) = idxToCoord i in cellTransition cell $ countLiveNeighbours x y state
      )
      (case state of GameState s -> s)

-- flatten 2D index to 1D
{-# INLINE idx #-}
idx :: XCoord -> YCoord -> Int
idx x y = x * boardSize + y
{-# INLINE idxToCoord #-}
idxToCoord :: Int -> (XCoord, YCoord)
idxToCoord i =
  i `quotRem` boardSize

{-# INLINE isLive #-}
isLive :: V.Vector Cell -> XCoord -> YCoord -> Int
isLive s x y
  | x < 0 || x >= boardSize || y < 0 || y >= boardSize = 0
  | otherwise = case V.unsafeIndex s (idx x y) of
      Alive _ -> 1
      Dead -> 0
{-# INLINE countLiveNeighbours #-}
countLiveNeighbours :: XCoord -> YCoord -> GameState -> Int
countLiveNeighbours x y (GameState s) =
  isLive s (x - 1) (y - 1)
    + isLive s (x - 1) y
    + isLive s (x - 1) (y + 1)
    + isLive s x (y - 1)
    + isLive s x (y + 1)
    + isLive s (x + 1) (y - 1)
    + isLive s (x + 1) y
    + isLive s (x + 1) (y + 1)

cellTransition :: Cell -> Int -> Cell
cellTransition cell n =
  case cell of
    Dead | n == 3 -> Alive Red
    Alive c | n == 2 || n == 3 -> Alive c
    _ -> Dead

-- glider
glider :: V.Vector (V.Vector Cell)
glider =
  V.fromList
    [ V.fromList [Alive Red, Dead, Dead]
    , V.fromList [Dead, Alive Red, Dead]
    , V.fromList [Alive Red, Alive Red, Alive Red]
    ]

insertRow :: XCoord -> YCoord -> V.Vector (V.Vector Cell) -> GameState -> GameState
insertRow x y patch (GameState s) =
  let updates =
        V.ifoldl'
          ( \acc dy r ->
              let !y' = y + dy
               in if y' < 0 || y' >= boardSize
                    then acc
                    else
                      V.ifoldl'
                        ( \acc2 dx cell ->
                            let !x' = x + dx
                             in if x' < 0 || x' >= boardSize
                                  then acc2
                                  else (idx x' y', cell) : acc2
                        )
                        acc
                        r
          )
          []
          patch
   in GameState $ s V.// updates
