{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lib2
    ( Query(..),
      parseQuery,
      parseDefineBoard,
      parseExactWord,
      parseSize,
      parseDefinePiece,
      parsePiece,
      parseDefineMoveTree,
      parseMoveTree,
      parseTree,
      parseSubtrees,
      parsePosition,
      knightOffsets,
      generateKnightMoves,
      generateBishopMoves,
      generateQueenMoves,
      generateRookMoves,
      validateMove,
      isValidKnightMove,
      isValidBishopMove,
      isValidRookMove,
      isValidQueenMove,
      validateMovesInTree,
      checkMoveTree',
      getStartPosition,
      isWithinBounds,
      positionToTuple,
      MoveTree(..),
      Position(..),
      State(..),
      Parser,
      emptyState,
      stateTransition,
      parseChar,
      parseLetters,
      parseWord,
      parseNumber,
    ) where

import qualified Data.Char as C
import qualified Data.List as L

data MoveTree = MoveTree Position [MoveTree]
    deriving (Eq, Show)

data Position = Position Char Int
    deriving (Eq, Show)

type Parser a = String -> Either String (a, String)

data Query
    = DefineBoard Int Int  -- Define board with width and height
    | DefinePiece String    -- Define the piece to validate (e.g., "knight", "pawn")
    | DefineMoveTree MoveTree  -- Define the move tree
    | ShowState  -- Show the current state
    | Run  -- Run validation
    deriving (Eq, Show)

parseQuery :: String -> Either String (Query, String)
parseQuery input
    | null input = Left "Input cannot be empty."
    | otherwise =
        case and3' (\_ _ _ -> ()) (parseExactWord "Define") (parseExactWord "board") (parseExactWord "as") input of
            Right (_, rest) ->
                case parseDefineBoard rest of
                    Right (query, rest') -> Right (query, rest')
                    Left err -> Left err
            _ ->
                case and2' (\_ _ -> ()) (parseExactWord "Validate") (parseExactWord "for") input of
                    Right (_, rest) ->
                        case parseDefinePiece rest of
                            Right (query, rest') -> Right (query, rest')
                            Left err -> Left err
                    _ ->
                        case and2' (\_ _ -> ()) (parseExactWord "Move") (parseExactWord "tree") input of
                            Right (_, rest) ->
                                case parseMoveTree rest of
                                    Right (moveTree, rest') -> Right (DefineMoveTree moveTree, rest')
                                    Left err -> Left err
                            _ ->
                                case and2' (\_ _ -> ()) (parseExactWord "Show") (parseExactWord "state") input of
                                    Right (_, rest) -> Right (ShowState, rest)
                                    _ ->
                                        case parseExactWord "Run" input of
                                            Right (_, rest) -> Right (Run, rest)
                                            _ -> Left "Invalid input format."



-- Parse "Define board as <width>x<height>"
parseDefineBoard :: Parser Query
parseDefineBoard input =
    case parseSize input of
        Right ((w, h), rest) -> Right (DefineBoard w h, rest)
        Left err -> Left err

-- <board_dimensions> ::= "4x4" |  "5x5" |  "6x6" |  "7x7" |  "8x8" 
parseSize :: Parser (Int, Int)
parseSize =
    \input ->
        case and3' (\w _ h -> (w, h)) parseNumber (parseChar 'x') parseNumber input of
            Right ((w, h), rest) ->
                if isValidSize w h
                    then Right ((w, h), rest)
                    else Left "Invalid board size: must be one of 4x4, 5x5, 6x6, 7x7, or 8x8"
            Left err -> Left err
  where
    -- Check if the sizes are valid
    isValidSize :: Int -> Int -> Bool
    isValidSize 4 4 = True
    isValidSize 5 5 = True
    isValidSize 6 6 = True
    isValidSize 7 7 = True
    isValidSize 8 8 = True
    isValidSize _ _ = False

-- Parse "Validate for <piece>"
parseDefinePiece :: Parser Query
parseDefinePiece input =
    case parsePiece input of
        Right (piece, rest) -> Right (DefinePiece piece, rest)
        Left err -> Left err


-- <define_piece> ::= "Validate for " <piece>
parsePiece :: Parser String
parsePiece input =
    case parsePieceOr input of
        Right (piece, rest) -> Right (piece, rest)
        Left err -> Left err
    where
        parsePieceOr =
            parseExactWord "knight" `or2`
            parseExactWord "rook" `or2`
            parseExactWord "queen" `or2`
            parseExactWord "king" `or2`
            parseExactWord "bishop"


-- <define_move_tree> ::= "Move tree " <move_tree>
parseDefineMoveTree :: Parser Query
parseDefineMoveTree input =
    case parseMoveTree input of
        Right (tree, rest) -> Right (DefineMoveTree tree, rest)
        Left err -> Left err



parseMoveTree :: String -> Either String (MoveTree, String)
parseMoveTree input =
    case parseTree (dropWhile (== ' ') input) of
        Right (tree, rest) -> Right (tree, rest)
        Left err           -> Left $ "Invalid move tree format: " ++ err


-- <move-tree> ::= '(' <position> <subtrees> ')'
parseTree :: Parser MoveTree
parseTree input =
    case and4' (\_ pos subtrees _ -> MoveTree pos subtrees)
               (parseChar '(')
               parsePosition
               parseSubtrees
               (parseChar ')') input of
        Right (tree, rest) -> Right (tree, rest)
        Left err           -> Left err


-- Parse a list of subtrees (multiple move trees separated by parentheses)
parseSubtrees :: Parser [MoveTree]
parseSubtrees input =
    case parseTree input of
        Right (subtree, rest) ->
            case parseSubtrees rest of
                Right (subtrees, finalRest) -> Right (subtree : subtrees, finalRest)
                Left _ -> Right ([subtree], rest)
        Left _ -> Right ([], input)

-- <position> ::= <file> <rank>
parsePosition :: Parser Position
parsePosition (file:rank:rest)
    | C.isLetter file && C.isDigit rank =
        Right (Position file (read [rank]), rest)
parsePosition _ = Left "Invalid position format"


data State = State
    { boardSize :: (Int, Int)
    , piece     :: String
    , moveTree  :: MoveTree
    } deriving (Show, Eq)

-- Initial state
emptyState :: State
emptyState = State (6, 6) "knight" (MoveTree (Position 'A' 1) [])

stateTransition :: State -> Query -> Either String ([String], State)
stateTransition state (DefineBoard w h) =
    let newState = state { boardSize = (w, h) }
        message = "Board defined with size: " ++ show (w, h)
    in Right ([message], newState) 

stateTransition state (DefinePiece p) =
    let newState = state { piece = p }
        message = "Piece defined as: " ++ p
    in Right ([message], newState) 

stateTransition state (DefineMoveTree tree) =
    let newState = state { moveTree = tree }
        message = "Move tree defined."
    in Right ([message], newState) 

stateTransition state Run =
    let (w, h) = boardSize state
        currentMoveTree = moveTree state
        startPos = positionToTuple (getStartPosition currentMoveTree) 
    in if not (isWithinBounds startPos (w, h))
        then Left $ "Starting position " ++ show (getStartPosition currentMoveTree) ++ " is out of bounds for the board size."
        else 
            let moveValidationResults = checkMoveTree' currentMoveTree (getStartPosition currentMoveTree) state
                invalidMoves = filter (not . snd) moveValidationResults
            in if null invalidMoves
                then Right (["All moves validated successfully."], state)
                else 
                    let invalidMovePositions = map (show . fst) invalidMoves
                        errorMessage = "Invalid moves found at positions: " ++ unwords invalidMovePositions
                    in Left errorMessage


stateTransition state ShowState =
    let boardSizeMsg = "Current board size: " ++ show (boardSize state)
        pieceMsg = "Current piece: " ++ piece state
        moveTreeMsg = "Current move tree: " ++ show (moveTree state)
    in Right ([boardSizeMsg, pieceMsg, moveTreeMsg], state)

------------------------------------------------ General parsers ------------------------------------------------
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f p1 p2 = \input ->
    case p1 input of
        Right (v1, rest1) ->
            case p2 rest1 of
                Right (v2, rest2) -> Right (f v1 v2, rest2)
                Left err2 -> Left err2
        Left err1 -> Left err1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f p1 p2 p3 = \input ->
    case p1 input of
        Right (v1, rest1) ->
            case p2 rest1 of
                Right (v2, rest2) ->
                    case p3 rest2 of
                        Right (v3, rest3) -> Right (f v1 v2 v3, rest3)
                        Left err3 -> Left err3
                Left err2 -> Left err2
        Left err1 -> Left err1

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f a b c d = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 p1 p2 = \input ->
    case p1 input of
        Right result -> Right result
        Left _ -> p2 input

parseChar :: Char -> Parser Char
parseChar c (h:t) | c == h    = Right (c, t)
parseChar c _                 = Left $ "Expected '" ++ [c] ++ "'"

parseLetters :: Parser String
parseLetters [] = Right ("", [])
parseLetters (c:rest)
    | C.isLetter c = case parseLetters rest of
                        Right (word, remaining) -> Right (c:word, remaining)
                        Left err -> Left err
    | otherwise = Right ("", c:rest)

parseWord :: Parser String
parseWord input =
    case parseLetters input of
        Right (word, rest) -> Right (word, dropWhile C.isSpace rest)
        Left err -> Left err

parseExactWord :: String -> Parser String
parseExactWord expected input =
    case parseWord input of
        Right (word, rest) ->
            if word == expected
            then Right (word, rest)
            else Left ("Expected '" ++ expected ++ "', but found '" ++ word ++ "'")
        Left err -> Left err

parseNumber :: Parser Int
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let digits = L.takeWhile C.isDigit str
        rest = L.drop (length digits) str
    in case digits of
        [] -> Left ("Expected a number but found: " ++ show str)
        _  -> Right (read digits, rest)

---------------------------------------------------------------------------------------------------------------

------------------------------------------- Move tree validation ----------------------------------------------

knightOffsets :: [(Int, Int)]
knightOffsets = [(2, 1), (2, -1), (-2, 1), (-2, -1),
                 (1, 2), (1, -2), (-1, 2), (-1, -2)]

generateKnightMoves :: State -> Position -> [Position]
generateKnightMoves (State (boardWidth, boardHeight) _ _) (Position file rank) =
    filter isValidPosition [ Position (toEnum (fromEnum file + dx)) (rank + dy) | (dx, dy) <- knightOffsets ]
  where
    isValidPosition (Position f r) =
        f >= 'A' && f <= toEnum (fromEnum 'A' + boardWidth - 1) && r >= 1 && r <= boardHeight

generateBishopMoves :: State -> Position -> [Position]
generateBishopMoves (State (boardWidth, boardHeight) _ _) (Position file rank) =
    filter isValidPosition [ Position (toEnum (fromEnum file + i)) (rank + i) | i <- [-boardWidth..boardWidth], i /= 0 ] ++
    filter isValidPosition [ Position (toEnum (fromEnum file + i)) (rank - i) | i <- [-boardWidth..boardWidth], i /= 0 ]
  where
    isValidPosition (Position f r) =
        f >= 'A' && f <= toEnum (fromEnum 'A' + boardWidth - 1) && r >= 1 && r <= boardHeight

generateRookMoves :: State -> Position -> [Position]
generateRookMoves (State (boardWidth, boardHeight) _ _) (Position file rank) =
    [ Position f rank | f <- enumFromTo 'A' (toEnum (fromEnum 'A' + boardWidth - 1)), f /= file ] ++
    [ Position file r | r <- [1..boardHeight], r /= rank ]

generateQueenMoves :: State -> Position -> [Position]
generateQueenMoves state position =
    generateRookMoves state position ++ generateBishopMoves state position

validateMove :: String -> State -> Position -> Bool
validateMove "knight" = isValidKnightMove
validateMove "bishop" = isValidBishopMove
validateMove "rook"   = isValidRookMove
validateMove "queen"  = isValidQueenMove
validateMove _        = \_ _ -> False


isValidKnightMove :: State -> Position -> Bool
isValidKnightMove state targetPosition =
    let currentPosition = case moveTree state of
                            MoveTree pos _ -> pos
    in targetPosition `elem` generateKnightMoves state currentPosition

isValidBishopMove :: State -> Position -> Bool
isValidBishopMove state targetPosition =
    let currentPosition = case moveTree state of
                            MoveTree pos _ -> pos
    in targetPosition `elem` generateBishopMoves state currentPosition

isValidRookMove :: State -> Position -> Bool
isValidRookMove state targetPosition =
    let currentPosition = case moveTree state of
                            MoveTree pos _ -> pos
    in targetPosition `elem` generateRookMoves state currentPosition

isValidQueenMove :: State -> Position -> Bool
isValidQueenMove state targetPosition =
    let currentPosition = case moveTree state of
                            MoveTree pos _ -> pos
    in targetPosition `elem` generateQueenMoves state currentPosition

validateMovesInTree :: String -> State -> MoveTree -> Bool
validateMovesInTree pieceType state (MoveTree _ subtrees) =
    all (\(MoveTree targetPos _) -> validateMove pieceType state targetPos) subtrees



checkMoveTree' :: MoveTree -> Position -> State -> [(Position, Bool)]
checkMoveTree' (MoveTree pos subtrees) currentPos state =
    let pieceType = piece state
        isValid = validateMove pieceType state pos
        newState = state { moveTree = MoveTree pos subtrees }
    in if pos == currentPos
       then concatMap (\subtree -> checkMoveTree' subtree pos newState) subtrees
       else (pos, isValid) : concatMap (\subtree -> checkMoveTree' subtree pos newState) subtrees


getStartPosition :: MoveTree -> Position
getStartPosition (MoveTree pos _) = pos

isWithinBounds :: (Int, Int) -> (Int, Int) -> Bool
isWithinBounds (x, y) (w, h) = x >= 1 && x <= w && y >= 1 && y <= h

positionToTuple :: Position -> (Int, Int)
positionToTuple (Position file rank) =
    (fromEnum file - fromEnum 'A' + 1, rank) 

---------------------------------------------------------------------------------------------------------------