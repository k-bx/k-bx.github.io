#!/usr/bin/env stack
-- stack --resolver=lts-13.1 script --package text,transformers
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Except
import qualified Data.Text as T
import Data.Text (Text)

newtype ValidUrl =
  ValidUrl Text
  deriving (Show, Eq)

data InputForm = InputForm
  { inpUsername :: Text
  , inpHomepage :: Text
  } deriving (Show, Eq)

data OutputForm = OutputForm
  { outUsername :: Text
  , outPassword :: ValidUrl
  } deriving (Show, Eq)

lengthBetween :: Monad m => Int -> Int -> Text -> ExceptT Text m Text
lengthBetween n m txt =
  if T.length txt < n || T.length txt > m
    then throwE
           ("Length must be between " <> T.pack (show n) <> " and " <>
            T.pack (show m))
    else return txt

validateUrl :: Monad m => Text -> ExceptT Text m ValidUrl
validateUrl txt
  -- exercise to a reader
 = throwE "Invalid URL"

main :: IO ()
main = do
  let inpForm = InputForm "usr" "httpbadurl"
  outForm <-
    runExceptT $
    OutputForm <$> lengthBetween 4 20 (inpUsername inpForm) <*>
    validateUrl (inpHomepage inpForm)
  print outForm
