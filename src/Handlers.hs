{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Handlers where

import           Data.Conduit         (Source)
import           Data.Conduit         (Conduit, Sink, ($$), (=$=))
import qualified Data.Conduit.List    as CL
import           Data.Monoid          (mconcat)
import           Data.Monoid          (Monoid (mappend, mempty))
import           Data.Text.Lazy       (Text)
import           Database.Persist     (get, selectSource)
import           Database.Persist.Sql (Entity, SqlPersistM, toSqlKey)
import           Model
import           Text.Blaze.Html5

getPeople :: SqlPersistM Html
getPeople = selectAllPeople =$= peopleEntityToHtml $$ foldSink

selectAllPeople :: Source SqlPersistM (Entity Person)
selectAllPeople = selectSource [] []

peopleEntityToHtml :: (Monad m) => Conduit (Entity Person) m Html
peopleEntityToHtml = CL.map (h1 . toHtml . show)

foldSink :: (Monad m, Monoid a) => Sink a m a
foldSink = CL.fold mappend mempty

getPerson :: Integer -> SqlPersistM Html
getPerson personId = do
    (person :: Maybe Person) <- get personKey
    return . h1 . toHtml . show $ person
  where
    personKey = toSqlKey . fromIntegral $ personId

getRoot :: Html
getRoot = h1 "Scotty, nobody is here!"

getWord :: Text -> Html
getWord word = h1 . toHtml $ mconcat ["Scotty, ", word, " me up!"]
