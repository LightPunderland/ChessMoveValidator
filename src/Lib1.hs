module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["define_board_command", "board_dimensions", "define_piece", "start_position", "define_move_tree", "move_tree", "validate_move_tree", "position", "file", "rank"]
