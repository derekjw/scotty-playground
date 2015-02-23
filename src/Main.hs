{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (LoggingT,
                                                       runStderrLoggingT)
import           Database.Persist                     (insert)
import           Database.Persist.Sql                 (ConnectionPool,
                                                       SqlPersistM,
                                                       runMigration,
                                                       runSqlPersistMPool)
import           Database.Persist.Sqlite              (withSqlitePool)
import           Handlers
import           Model
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory
import           System.Environment                   (getEnv)
import           System.IO.Error
import           Text.Blaze.Html                      (Html)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Web.Scotty                           (ActionM, ScottyM, get,
                                                       html, middleware, param,
                                                       scotty)

main :: IO ()
main = do
    removeIfExists "my.db"
    runStderrLoggingT . withSqlPool $ \pool -> liftIO $ do

        runSql pool $ do
            runMigration migrateModel
            _ <- insert $ Person "John Doe" 35
            _ <- insert $ Person "Jane Doe" 32
            return ()

        port <- getPort
        scotty port $ do
            middleware logStdoutDev
            router pool

router :: ConnectionPool -> ScottyM ()
router pool = do

    get "/people/:personId" $ do
        personId <- param "personId"
        blazeSql pool $ getPerson personId

    get "/people" $
        blazeSql pool getPeople

    get "/" $
        blazeSql pool getPeople

    get "/:word" $ do
        beam <- param "word"
        blaze $ getWord beam

withSqlPool :: (ConnectionPool -> LoggingT IO ()) -> LoggingT IO ()
withSqlPool = withSqlitePool "my.db" 10

getPort :: IO Int
getPort = fmap read $ getEnv "PORT"

blaze :: Html -> ActionM ()
blaze = html . renderHtml

blazeSql :: ConnectionPool -> SqlPersistM Html -> ActionM ()
blazeSql pool sql = liftIO (runSql pool sql) >>= blaze

runSql :: ConnectionPool -> SqlPersistM a -> IO a
runSql = flip runSqlPersistMPool

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
