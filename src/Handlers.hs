{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handlers where

import           Data.Monoid          (mconcat)
import           Data.Text.Lazy       (Text)
import           Database.Persist     (get, selectList)
import           Database.Persist.Sql (Entity, SqlPersistM, toSqlKey)
import           Model
import           Text.Blaze.Html5

getPeople :: SqlPersistM Html
getPeople = do
    (people :: [Entity Person]) <- selectList [] []
    return . h1 . toHtml . show $ people

getPerson :: Integer -> SqlPersistM Html
getPerson personId = do
    (person :: Maybe Person) <- get personKey
    return . h1 . toHtml . show $ person
  where
    personKey = toSqlKey (fromIntegral personId)

getRoot :: Html
getRoot = h1 "Scotty, nobody is here!"

getWord :: Text -> Html
getWord word = h1 . toHtml $ mconcat ["Scotty, ", word, " me up!"]
