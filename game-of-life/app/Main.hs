-- port of hyperlith game-of-life https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj
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
boardSize = 20
_s :: Length
_s = PxRem 20
black :: HexColor
black = HexColor "#000FA"

__css :: View c ()
__css =
  Hyperbole.style $
    Bs.concat
      [ ".cell { transition: background 0.6s ease; }"
      , Bs.pack $ ".board { background: white; width: min(100% - 2rem, 30rem); display: grid; aspect-ratio: 1/1; grid-template-rows: repeat(" ++ show boardSize ++ ", 1fr); grid-template-columns: repeat(" ++ show boardSize ++ ", 1fr); }"
      ]
main :: IO ()
main = do
  Prelude.putStrLn $ "Listening on port: " <> show port
  board <- STM.newTVarIO defaultState
  runEff $ runConcurrent $ runReader board $ startBackgroundLoop 1000
  run port $ do
    -- liveApp quickStartDocument (runPage page)
    liveApp quickStartDocument (runReader board . runConcurrent $ runPage page)

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
  -- deriving (Show, Generic, ViewId)
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToParam, FromParam)
toggleCell :: Cell -> Cell
toggleCell Dead = Alive Red
toggleCell (Alive c) = Dead
newtype GameState = GameState (V.Vector Cell)
  deriving (Show, Read, Eq, Generic)

data GamePage = GamePage -- this is like a ViewModel binding in MVVM? Seems like it dosn't need to have an actual value
  deriving (Generic, ViewId)

-- instance Default GameState where
--   def = [[Dead | _ <- [1 .. boardSize]] | _ <- [1 .. boardSize]]
defaultState :: GameState
defaultState =
  let mid = (boardSize `div` 2)
   in insertRow mid mid glider $
        GameState $
          V.generate (boardSize * boardSize) (const Dead)

getState :: (Concurrent :> es, Reader (TVar GameState) :> es) => Eff es GameState
getState = readTVarIO =<< ask

modifyState :: (Concurrent :> es, Reader (TVar GameState) :> es) => (GameState -> GameState) -> Eff es GameState
modifyState f = do
  var <- ask
  atomically $ do
    modifyTVar var f
    readTVar var
type XCoord = Int
type YCoord = Int
toggleCellAt :: XCoord -> YCoord -> GameState -> GameState
toggleCellAt x y (GameState board) =
  GameState $ board & ix (idx x y) %~ toggleCell

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
    -- ~ height _s
    -- ~ width _s
    ~ cls "cell"
    $ text
    $ case cell of
      Dead -> "X"
      Alive _ -> "O"

boardView :: GameState -> View GamePage ()
boardView (GameState state) = do
  -- el $ text $ pack $ show state
  el
  -- @ onLoad FetchState 1000
  -- . style "grid-template-columns" ("repeat(" <> pack (show boardSize) <> ", " <> pack (show _s) <> ")")
  ~ cls "board"
  $ do
    zipWithM_ -- V.zipWithM_ shows empty page, so we convert to list
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
  -- GameState $ V.map (toggleCell) board
  GameState $
    V.imap
      ( \i cell ->
          let (x, y) = idxToCoord i in cellTransition cell $ getn x y state
      )
      (case state of GameState s -> s)

-- flatState :: GameState -> [(XCoord, YCoord, Cell)]
-- flatState (GameState board) =
--   Prelude.concat $
--     Prelude.zipWith
--       ( \y r ->
--           Prelude.zipWith
--             ( \x c ->
--                 (x, y, c)
--             )
--             [0 .. boardSize]
--             r
--       )
--       [0 .. boardSize]
--       board

-- getn :: XCoord -> YCoord -> GameState -> [(XCoord, YCoord, Cell)]
-- getn cx cy (GameState board) =
--   -- let valid x' y' =

--   Prelude.concat $
--     Prelude.zipWith
--       ( \y r ->
--           Prelude.zipWith
--             ( \x c ->
--                 (x, y, c)
--             )
--             [0 .. boardSize]
--             r
--       )
--       [0 .. boardSize]
--       board
neighbors :: V.Vector (XCoord, YCoord)
neighbors =
  V.fromList
    [ (-1, -1)
    , (-1, 0)
    , (-1, 1)
    , (0, -1)
    , (0, 1)
    , (1, -1)
    , (1, 0)
    , (1, 1)
    ]

-- flatten 2D index to 1D
idx :: XCoord -> YCoord -> Int
idx r c = r * boardSize + c
idxToCoord :: Int -> (XCoord, YCoord)
idxToCoord i =
  i `quotRem` boardSize

-- coords :: V.Vector (XCoord, YCoord)
-- coords =
--   -- V.map idxToCoord $ V.fromList [0 .. boardSize * (boardSize - 1)]
--   V.map idxToCoord $ V.generate (boardSize * boardSize) id
idxN :: XCoord -> YCoord -> V.Vector Int
idxN x y =
  V.mapMaybe
    ( \(dx, dy) ->
        let x' = x + dx
            y' = y + dy
         in if 0 <= y' && y' < boardSize && 0 <= x' && x' < boardSize
              then Just (idx x' y')
              else Nothing
    )
    neighbors

getn :: XCoord -> YCoord -> GameState -> Int
getn x y (GameState s) =
  let n = idxN x y
   in V.length $ V.filter (\case Alive _ -> True; _ -> False) $ V.map (s V.!) n

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
