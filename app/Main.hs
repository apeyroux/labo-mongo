{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}

import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.Text as T

data Cnx = Cnx {
  cnxPipe :: Pipe
  , cnxAccessMode :: AccessMode
  , cnxDatabase :: Database
  , cnxCollection :: Collection }

data Book = Book {
  bookTitle :: T.Text
  , bookAuthors :: [T.Text] } deriving Show

insertBook :: Book -> ReaderT Cnx IO ()
insertBook b@Book{..} = do
  cnx@Cnx{..} <- ask
  _ <- access cnxPipe cnxAccessMode cnxDatabase $ do
    insert cnxCollection ["authors" =: bookAuthors, "title"  =: bookTitle]
  pure ()

main :: IO ()
main = do
  putStrLn "Start ..."
  pipe <- connect (host "127.0.0.1")
  runReaderT (insertBook (Book "Introduction to Functional Programming" ["Richard Bird", "Philip Wadler"])) (Cnx pipe master "store" "books")
  pure ()
