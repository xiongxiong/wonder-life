module Main (main) where

import Server (app, migrate)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.MySQL (ConnectInfo (connectDatabase, connectHost, connectPassword, connectPort, connectUser), defaultConnectInfo, runMigration, runSqlPersistMPool, withMySQLPool)
import Katip (ColorStrategy (..), Severity (..), Verbosity (..), closeScribes, defaultScribeSettings, initLogEnv, mkHandleScribe, permitItem, registerScribe, runKatipContextT)
import Network.Wai.Handler.Warp (run)
import RIO (IO, MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), bracket, join, stdout, ($), (.), (<$>), (=<<))

main :: IO ()
main = do
  runStdoutLoggingT . withMySQLPool conn 10 $
    liftIO
      <$> runSqlPersistMPool
        ( do
            runMigration migrate
            backend <- ask
            handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
            let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "WonderLife" "dev"
            liftIO . bracket mkLogEnv closeScribes $ \logEnv -> do
              join . runKatipContextT logEnv () "main" . runReaderT (run 8081 <$> app) $ backend
        )

conn :: ConnectInfo
conn =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectPort = 3306,
      connectUser = "root",
      connectPassword = "pass@1234",
      -- connectDatabase = "task_demo"
      connectDatabase = "task_test"
    }
