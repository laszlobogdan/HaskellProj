module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Data.Maybe (fromJust)

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit =
  do
    let empty_db = DB.empty
    DB.save empty_db
    return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts =
  do
    database_load <- DB.load
    case database_load of
      Success snippet_list -> putStrLn $ entrySnippet (fromJust (DB.findFirst (\x -> entryId x == getOptId getOpts) snippet_list))
      Error err -> putStrLn "Failed to load DB"

-- | Handle the search command
handleSearch :: (TestableMonadIO m, Show FmtEntry) => SearchOptions -> m ()
handleSearch (SearchOptions searchOpts) =
  do
    database_load <- DB.load
    case database_load of
      Success snippets ->
          let entries = DB.findAll (matchedByAllQueries searchOpts) snippets
          in 
            if length entries == 0
              then putStrLn "No entries found"
              else mapM_ (putStrLn. extractEntrySnippet) entries
      Error err -> putStrLn "Failed to load DB"
     where
        extractEntrySnippet :: Entry -> String
        extractEntrySnippet entry = show (FmtEntry entry)


-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  snip <- readFile (addOptFilename addOpts)
  db <- DB.load

  case db of
    Error _  -> putStrLn "Failed to load DB"
    Success (DB.SnippetDB entries) ->
      case findIndex (\entry -> snip == entrySnippet entry) entries of
        Just index ->do
          let tmp = "[" ++ show index ++ "] " ++ show (entryFilename (entries !! index))
          putStrLn $ "Entry with this content already exists: \n" ++ "["++show (entryId (entries !! index)) ++ "] " ++ entryFilename (entries !! index) ++ ":"
        Nothing -> do
          let db' = DB.insertWith (\id -> makeEntry id snip addOpts) (DB.SnippetDB entries)
          saveResult <- DB.save db'
          case saveResult of
            Error err -> putStrLn ("Failed to save DB: " ++ show err)
            Success _ -> putStrLn "Entry successfully added"
  where
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex p xs = case filterIndex p xs of
                   []    -> Nothing
                   (i:_) -> Just i

    filterIndex :: (a -> Bool) -> [a] -> [Int]
    filterIndex p = go 0
      where
        go _ []     = []
        go i (x:xs) = if p x then i : go (i+1) xs else go (i+1) xs

    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: (TestableMonadIO m, Show FmtEntry) => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
