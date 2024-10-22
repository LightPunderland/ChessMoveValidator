# fp-2024

# Chess domain:
This project implements a command-line tool for exploring valid chess piece moves on a chessboard. The program allows users to define a chessboard of varying sizes, specify the chess piece whos moves are going to be validated, and build a move tree that represents the pieces potential movement paths. Users can enter move trees and validate them based on a certain pieces movement rules in chess.

## Domain/BNF changes:
1) Extended the programs scope to include verifying moves of rooks, bishops, kings and queens as well as knights, so that the changes in the programs state would be more meaningful.
2) As per the lecturers' advice, removed the "Define knight position" command, as it was deemed redundant since the starting position can be easily inferred from the given move tree.
3) Added the show state command, to check the current configuration of your program.

## Main entities:
<board_dimensions> - represents the size of the chessboard, which can be configured by the user.

<move_tree> - a recursive structure representing the knight’s possible paths from the starting position, potentially branching out into different paths at each step.

## Operations:
1) Define Board - This command defines the size of the chessboard.

Example input: "Define board as 8x8"

2) Define piece - This command specifies which pieces moves are going to be validated.

Example input: "Validate for knight"

3) Define Move Tree - This command defines a recursive tree of knight moves from the starting position. Each branch represents a different path the knight can take.

Example input: "Move tree (E2(F4(E6)(G6))(D4(C6)(B5)))"

This defines a move tree where the knight starts at E2, can move to F4 or D4. From F4, it can move to E6 or G6. from D4, it can move to C6 or B5.

4) Validate Move Tree - This command verifies the piece movement rules on the defined move tree.

Example input: "Run"

5) Show state - shows the current configuration

Example input: "Show state"

## BNF:

<define_board> ::= "Define board as " <board_dimensions>
<board_dimensions> ::= "4x4" |  "5x5" |  "6x6" |  "7x7" |  "8x8" 

<define_piece> ::= "Validate for " <piece>

<define_move_tree> ::= "Move tree " <move_tree>

<show_state> ::= "Show state"

<validate_move_tree> ::= "Run"

<move_tree> ::= "(" <position> <move_tree>* ")"
              
<position> ::= <file> <rank>

<start_position> ::= <file> <rank>

<piece> ::= "knight" | "rook" | "queen" | "king"| "bishop"

<file> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H"
<rank> ::= [1-8]

