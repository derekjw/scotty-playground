{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Model where

import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateModel"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]
