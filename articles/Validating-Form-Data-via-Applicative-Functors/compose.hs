#!/usr/bin/env stack
-- stack --resolver=lts-13.1 script --package either,text
{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Compose
import qualified Data.Text as T
import Data.Text (Text)
import Data.Either.Validation

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

throwE :: Applicative m => err -> Compose m (Validation err) a
throwE err = Compose (pure (Failure err))

lengthBetween ::
     Applicative m => Int -> Int -> Text -> Compose m (Validation [Text]) Text
lengthBetween n m txt =
  if T.length txt < n || T.length txt > m
    then throwE
           [ "Length must be between " <> T.pack (show n) <> " and " <>
             T.pack (show m)
           ]
    else pure txt

validateUrl :: Applicative m => Text -> Compose m (Validation [Text]) ValidUrl
validateUrl txt
  -- exercise to a reader
 = throwE ["Invalid URL"]

main :: IO ()
main = do
  let inpForm = InputForm "usr" "httpbadurl"
  outForm <-
    getCompose $
    OutputForm <$> lengthBetween 4 20 (inpUsername inpForm) <*>
    validateUrl (inpHomepage inpForm)
  print outForm
