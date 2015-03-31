{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Handlers where

import           Data.Conduit         (Conduit, Sink, Source, ($$), (=$=))
import qualified Data.Conduit.List    as CL
import           Data.Monoid          (Monoid (mconcat, mappend, mempty))
import           Data.Text.Lazy       (Text)
import           Database.Persist     (get, selectSource)
import           Database.Persist.Sql (Entity, SqlPersistM, entityVal, toSqlKey)
import           Model
import           Prelude              hiding (span)
import           Text.Blaze.Html5

getPeople :: SqlPersistM Html
getPeople = selectAllPeople =$= peopleToHtml $$ foldSink

selectAllPeople :: Source SqlPersistM (Entity Person)
selectAllPeople = selectSource [] []

peopleToHtml :: (Monad m) => Conduit (Entity Person) m Html
peopleToHtml = CL.map personToHtml

personToHtml :: Entity Person -> Html
personToHtml entity =
    p $ do
        span . toHtml $ personName person
        span . toHtml $ personAge person
  where
    person = entityVal entity

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
