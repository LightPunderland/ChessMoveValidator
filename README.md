# fp-2024

# Chess domain:
This project implements a command-line tool for exploring valid knight moves on a chessboard. The program allows users to define a chessboard of varying sizes, specify the knight's starting position, and build a move tree that represents the knight's potential movement paths. Users can enter move trees and validate them based on the knight's movement rules in chess.

## Main entities:
<board_dimensions> - represents the size of the chessboard, which can be configured by the user.

<move_tree> - a recursive structure representing the knight’s possible paths from the starting position, potentially branching out into different paths at each step.

## Operations:
1) Define Board - This command defines the size of the chessboard.

Example input: "Define board as 8x8"

2) Set Knight Position - This command defines the size of the chessboard.

Example input: "Define board as 8x8"

3) Define Move Tree - This command defines a recursive tree of knight moves from the starting position. Each branch represents a different path the knight can take.

Example input: "Move tree: (E2(F4(E6)(G6))(D4(C6)(B5)))"

This defines a move tree where the knight starts at E2, can move to F4 or D4. From F4, it can move to E6 or G6. from D4, it can move to C6 or B5.

4) Validate Move Tree - This command verifies the knight's movement rules on the defined move tree.

Example input: "Run"

## BNF:

<define_board_command> ::= "Define board as " <board_dimensions>
<board_dimensions> ::= "4x4" |  "5x5" |  "6x6" |  "7x7" |  "8x8" 

<define_knight_command> ::= "Knight at " <start_position>

<define_move_tree> ::= "Move tree: " <move_tree>

<validate_move_tree> ::= "Run"

<move_tree> ::= "(" <position> <move_tree>* ")"
              
<position> ::= <file> <rank>

<start_position> ::= <file> <rank>

<file> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H"
<rank> ::= [1-8]

