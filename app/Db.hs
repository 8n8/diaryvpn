module Db (Db, initDb, query) where


import qualified Control.Concurrent.STM.TQueue
import qualified Diary
import qualified System.IO
import qualified Control.Concurrent.STM
import qualified Control.Exception
import qualified System.IO.Error


data Db
    = Db
        { lock :: Control.Concurrent.STM.TQueue.TQueue ()
        , diary :: Diary.Diary
        }


initDb :: IO Db
initDb =
    do
    lock_ <- Control.Concurrent.STM.atomically $
        do
        q <- Control.Concurrent.STM.TQueue.newTQueue
        Control.Concurrent.STM.TQueue.writeTQueue q ()
        return q
    diary_ <- Diary.initDiary
    result <- Control.Exception.try $
        System.IO.withFile
            "db"
            System.IO.ReadMode
            (\handle -> Diary.fromFile handle diary_)
    case result of
        Left err ->
            if System.IO.Error.isDoesNotExistError err then
                return ()

            else
                Control.Exception.throw err

        Right () ->
            return ()
    return $ Db lock_ diary_


query :: (Diary.Diary -> IO a) -> Db -> IO a
query f db =
    do
    Control.Concurrent.STM.atomically $ Control.Concurrent.STM.TQueue.readTQueue (lock db)
    result <- f (diary db)
    Control.Concurrent.STM.atomically $
        Control.Concurrent.STM.TQueue.writeTQueue (lock db) ()
    System.IO.withFile
        "db"
        System.IO.WriteMode
        (\handle -> Diary.toFile handle (diary db))
    return result
