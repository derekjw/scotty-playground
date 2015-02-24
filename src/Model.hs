{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Data.Text.Lazy      (Text)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateModel"] [persistLowerCase|
Person
    name Text
    age Int
    deriving Show
|]
