{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
import Foundation
import Application () -- for YesodDispatch instance
import Yesod
import Yesod.Core
import Yesod.Static
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql

connStr :: ConnectionString
connStr = "dbname=d5o7scosh5ta9t host=ec2-54-243-249-173.compute-1.amazonaws.com user=cvgufqvioxqrtd password=kflOupZJb91IthXi0XG-hSgfrJ port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       static@(Static settings) <- static "static"
       warp 8080 (App static pool)