{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    renderQuery,
    Statements (..)
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Exception (IOException, catch)
import System.IO (openFile, IOMode(ReadMode), hClose)
import Control.Monad (forever)
import Control.Concurrent.STM(STM, TVar, atomically, readTVar, writeTVar, readTVarIO)
import Control.Applicative ((<|>), many)
import qualified Lib2
import Control.Concurrent (Chan, writeChan, readChan, newChan)
import Control.Monad (forM_)
import Data.List (stripPrefix, isPrefixOf)
import Debug.Trace (trace)

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save content replyChan -> do
      writeFile "state.txt" content
      writeChan replyChan ()

    Load replyChan -> do
      fileExists <- fileExists "state.txt"
      if fileExists
        then do
          content <- readFile "state.txt"
          writeChan replyChan content
        else
          writeChan replyChan ""

fileExists :: FilePath -> IO Bool
fileExists path = catch (do
  handle <- openFile path ReadMode
  hClose handle
  return True) handleException
  where
    handleException :: IOException -> IO Bool
    handleException _ = return False

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)



-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = do
  case Lib2.parseExactWord "load" input of
    Right (_, rest) -> Right (LoadCommand, rest)
    Left _ -> do
      case Lib2.parseExactWord "save" input of 
        Right (_, rest) -> Right (SaveCommand, rest)
        Left _ -> do
          (statements, rest) <- parseStatements input
          Right (StatementCommand statements, rest)


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input
  | null input = Left "Error, empty input :("
  | take 5 input == "BEGIN" = parseBatch (drop 5 input) 
  | otherwise = do 
      (query, rest) <- Lib2.parseQuery input
      Right (Single query, rest)
  where 
    parseBatch :: String -> Either String (Statements, String)
    parseBatch str = do
      let cleanedInput = dropWhile (`elem` " \n\t") str 
      (queries, rest) <- manyEither parseQueryWithSemicolon cleanedInput
      let trimmedRest = dropWhile (`elem` " \n\t") rest 
      case trimmedRest of
        ('E':'N':'D':remaining) -> Right (Batch queries, dropWhile (`elem` " \n\t") remaining)
        _ -> Left "Batch statements must end with 'END'"


    manyEither :: (String -> Either String (a, String)) -> String -> Either String ([a], String)
    manyEither parser input = go input []
      where
        go str acc =
          case parser str of
            Right (result, rest) -> go rest (acc ++ [result])
            Left _ -> Right (acc, str)  

  
    parseQueryWithSemicolon :: String -> Either String (Lib2.Query, String)
    parseQueryWithSemicolon input = do
      let cleanedInput = dropWhile (`elem` " \n\t") input 
      (query, rest) <- Lib2.parseQuery cleanedInput
      let afterQuery = dropWhile (`elem` " \n\t") rest 
      (_, remaining) <- Lib2.parseChar ';' afterQuery 
      return (query, remaining)



-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  let (width, height) = Lib2.boardSize state
      pieceQuery = Lib2.DefinePiece (Lib2.piece state)
      boardQuery = Lib2.DefineBoard width height
      moveTreeQuery = Lib2.DefineMoveTree (Lib2.moveTree state)
      queries = [boardQuery, pieceQuery, moveTreeQuery]
  in Batch queries

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.DefineBoard width height) =
    "Define board as " ++ show width ++ "x" ++ show height
renderQuery (Lib2.DefinePiece piece) =
    "Validate for " ++ piece
renderQuery (Lib2.DefineMoveTree moveTree) =
    "Move tree " ++ renderMoveTree moveTree
renderQuery Lib2.ShowState =
    "Show state"
renderQuery Lib2.Run =
    "Run"

renderMoveTree :: Lib2.MoveTree -> String
renderMoveTree (Lib2.MoveTree pos subtrees) =
    "(" ++ renderPosition pos ++ concatMap renderMoveTree subtrees ++ ")"

renderPosition :: Lib2.Position -> String
renderPosition (Lib2.Position file rank) =
    [file] ++ show rank

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) =
  "BEGIN\n" ++ concatMap (\q -> renderQuery q ++ ";\n") queries ++ "END"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of

  StatementCommand statements -> atomically $ do
    currentState <- readTVar stateVar
    let result = processStatements currentState statements
    case result of
      Left err -> return $ Left err
      Right (msgs, newState) -> do
        writeTVar stateVar newState
        return $ Right (Just $ unlines msgs)

  SaveCommand -> do
    currentState <- readTVarIO stateVar
    let serializedState = renderStatements (marshallState currentState)
    ackChan <- newChan
    writeChan ioChan (Save serializedState ackChan)
    readChan ackChan
    return $ Right (Just "State saved successfully")

  LoadCommand -> do
    responseChan <- newChan
    writeChan ioChan (Load responseChan)
    content <- readChan responseChan
    case parseStatements content of
      Left err -> return $ Left $ "Failed to load state: " ++ err
      Right (statements, _) -> atomically $ do
        currentState <- readTVar stateVar
        let result = processStatements currentState statements
        case result of
          Left err -> return $ Left err
          Right (_, newState) -> do
            writeTVar stateVar newState
            return $ Right (Just "State loaded successfully")

-- | Helper function to process a batch or single statement
processStatements :: Lib2.State -> Statements -> Either String ([String], Lib2.State)
processStatements state (Single query) =
  case Lib2.stateTransition state query of
    Left err -> Left err
    Right (msg, newState) -> Right (msg, newState)

processStatements state (Batch queries) = 
  foldl processQuery (Right ([], state)) queries
  where
    processQuery :: Either String ([String], Lib2.State) -> Lib2.Query -> Either String ([String], Lib2.State)
    processQuery (Left err) _ = Left err
    processQuery (Right (msgs, st)) query = 
      case Lib2.stateTransition st query of
        Left err -> Left err
        Right (msg, newState) -> Right (msgs ++ msg, newState)