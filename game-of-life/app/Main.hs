{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import Web.Atomic.CSS
import Web.Hyperbole

main :: IO ()
main = do
  putStrLn $ "Listening on port: " <> show port
  run port $ do
    liveApp quickStartDocument (runPage page)

port :: Int
port = 3000

page :: (Hyperbole :> es) => Page es '[Message]
page = do
  pure $ do
    hyper Message1 ~ bold $ messageView "Hello"
    hyper Message2 ~ bold $ messageView "World!"

data Message = Message1 | Message2
  deriving (Generic, ViewId)

instance HyperView Message es where
  data Action Message = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView msg = do
  button (Louder msg) ~ border 1 $ text msg
