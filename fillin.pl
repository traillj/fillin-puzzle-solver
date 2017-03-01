% COMP30020 Declarative Programming
% Project 2
% Author: traillj
%
% Fillin puzzle solver.
% Given a file containing the structure of a fillin puzzle,
% a file with a list of words to use, and a name for the
% solution file, the program writes a solution in that file.
%
% The strategy is to first attempt solving the puzzle without
% search, where a slot can only be filled by one word.
% If the puzzle is not completely solved, words are chosen
% for each slot in turn, and rechosen through backtracking if
% no word can be used to be fill a slot.


% Provides the transpose predicate.
:- ensure_loaded(library(clpfd)).


% Reads a fillin puzzle and a word list file,
% then solves the puzzle and writes the solution
% to a file.
% The puzzle file should have an underscore for
% each blank square, and # for non-fillable
% squares. A line in the file should correspond
% to a line in the fillin puzzle.
% The word list file should contain each word
% to be filled in on a separate line.
main(PuzzleFile, WordlistFile, SolutionFile) :-
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),
    valid_puzzle(Puzzle),
    solve_puzzle(Puzzle, Wordlist, Solved),
    print_puzzle(SolutionFile, Solved).


% Reads a file, with each line
% becoming a list in a list
% of lists.
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

% Reads lines from a stream into
% a list of lists.
read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

% Reads characters from a stream
% into a list. Last is a boolean
% indicating EOF.
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ;   Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).


% Writes a list of lists to a file, with
% each list becoming a line in the file.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

% Writes a list to a stream, adding a newline.
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

% Writes a character to a stream.
% Converts variables to underscore
% characters.
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).


% Holds if all lists in a list of
% lists have the same length.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(same_length(Row), Rows).


%----------------------------------------------------------------------------

% Solves a fillin puzzle. First attempts to solve the puzzle without
% searching, then completely fills the puzzle with search if necessary.
% The filled puzzle is then verified with the remaining words, which
% may require backtracking.
solve_puzzle(Puzzle0, WordList, Puzzle) :-
    init_matrix_vars(Puzzle0, VarPuzzle),
    get_slots(VarPuzzle, Slots),
    solve_without_search(VarPuzzle, Slots, WordList, Puzzle1,
            RemainingWordList1),
    (   ground(Puzzle1)
    ->  Puzzle = Puzzle1
    ;   solve_with_search(Puzzle1, Slots, RemainingWordList1, Puzzle,
            RemainingWordList2),
        verify_words(Slots, RemainingWordList2)
    ).


% Initialises variables in a list
% of lists by converting underscore
% characters to variables.
init_matrix_vars([], []).
init_matrix_vars([Xs0|Xss0], Xss) :-
    init_list_vars(Xs0, Xs),
    Xss = [Xs|Xss1],
    init_matrix_vars(Xss0, Xss1).

% Initialises variables in a
% list by converting underscore
% characters to variables.
init_list_vars([], []).
init_list_vars([X0|Xs0], Xs) :-
    (   X0 == '_'
    ->  Xs = [_|Xs1]
    ;   Xs = [X0|Xs1]
    ),
    init_list_vars(Xs0, Xs1).


% Verifies that all words in the word list
% are equal to a slot in the slots list.
verify_words(_, []).
verify_words(Slots, [Word|WordList]) :-
    verify_word(Slots, Word),
    verify_words(Slots, WordList).

% Verifies that the word is equal
% to a slot in the slots list.
verify_word([Slot|Slots], Word) :-
    (   lists_match(Slot, Word)
    ->  true
    ;   verify_word(Slots, Word)
    ).


% Holds if the lists are equal.
% Can also be used to unify variables
% of a list to match the other list.
lists_match(List, List).


%----------------------------------------------------------------------------

% Gets all slots, horizontal and vertical, of a
% puzzle. Slots is a list of lists, each list is
% a slot to be filled in with a word. Horizontal
% slots come before vertical slots in Slots.
get_slots(Puzzle, Slots) :-
    get_horizontal_slots(Puzzle, Slots1),
    transpose(Puzzle, TransposedPuzzle),
    get_horizontal_slots(TransposedPuzzle, Slots2),
    append(Slots1, Slots2, Slots).

% Gets all horizontal slots of a puzzle.
get_horizontal_slots(Puzzle, Slots) :-
    insert_at_heads(Puzzle, '#', DelimPuzzle),
    append(DelimPuzzle, FlattenedPuzzle),
    split(FlattenedPuzzle, '#', Slots0),
    subtract(Slots0, [[]], Slots).


% Insert an element at the head of
% each list in a list of lists.
insert_at_heads([], _, []).
insert_at_heads([Xs0|Xss0], Elem, Xss) :-
    insert_at_head(Xs0, Elem, Xs),
    Xss = [Xs|Xss1],
    insert_at_heads(Xss0, Elem, Xss1).

% Inserts an element at the head of a list.
insert_at_head(Xs0, Elem, [Elem|Xs0]).


% Splits a list into a list of lists for
% each point in the list where the delimiter
% occurs. The delimiter is not included in
% the result.
split(Xs, Delimiter, SplitList) :-
    split1(Xs, Delimiter, SplitList, []).

% Helper method for split, maintains a
% temporary list of all elements in the
% unsplit list since the last delimiter
% occurrence.
split1([], _, SplitList, RevCurrentList) :-
    reverse(RevCurrentList, CurrentList),
    SplitList = [CurrentList].
split1([X|Xs], Delimiter, SplitList, RevCurrentList) :-
    (   (nonvar(X), X == Delimiter)
    ->  reverse(RevCurrentList, CurrentList),
        SplitList = [CurrentList|SplitList1],
        split1(Xs, Delimiter, SplitList1, [])
    ;   split1(Xs, Delimiter, SplitList, [X|RevCurrentList])
    ).


%----------------------------------------------------------------------------

% Attempts to solve the puzzle by filling slots that only match one word.
% When a slot is filled, the chosen word is removed from the list and the
% function recurses. If no slot with only one match is found, the puzzle
% and remaining word list is unified, regardless of whether the puzzle
% was completed.
solve_without_search(Puzzle0, Slots, WordList, Puzzle, RemainingWordList) :-
    (   fill_unique_match(Slots, WordList, ChosenWord)
    ->  remove_elem(WordList, ChosenWord, RemainingWordList1),
        solve_without_search(Puzzle0, Slots, RemainingWordList1, Puzzle,
            RemainingWordList)
    ;   RemainingWordList = WordList,
        Puzzle = Puzzle0
    ).

% Fills a slot that only matches with one word in the
% word list.
fill_unique_match([Slot|Slots], WordList, ChosenWord) :-
    find_slot_matches(Slot, WordList, Matches),
    length(Matches, NumMatches),
    (   NumMatches =:= 1
    ->  head(Matches, ChosenWord),
        Slot = ChosenWord
    ;   fill_unique_match(Slots, WordList, ChosenWord)
    ).

% Finds all the words in the word list that match a slot.
find_slot_matches(_, [], []).
find_slot_matches(Slot, [Word|WordList], Matches) :-
    (   (\+ \+ lists_match(Slot, Word), same_length(Slot, Word))
    ->  Matches = [Word|Matches1],
        find_slot_matches(Slot, WordList, Matches1)
    ;   find_slot_matches(Slot, WordList, Matches)
    ).

% Unifies the second argument
% with the head of the first.
head([X|_], X).

% Removes the first occurrence
% of an element from a list.
remove_elem([X|Xs], Elem, Result) :-
    (   X == Elem
    ->  Result = Xs
    ;   Result = [X|Result1],
        remove_elem(Xs, Elem, Result1)
    ).


% Solves the puzzle by filling in slots with matching words,
% backtracking and rechoosing other matching words if a no words
% match a slot. Ends when the puzzle has been completely filled.
% The filled puzzle may be incorrect as the remaining words
% may not match any slot.
solve_with_search(Puzzle0, [], WordList, Puzzle0, WordList).
solve_with_search(Puzzle0, [Slot|Slots], WordList, Puzzle,
            RemainingWordList) :-
    (   ground(Slot)
    ->  solve_with_search(Puzzle0, Slots, WordList, Puzzle, RemainingWordList)
    ;   choose_word(Slot, WordList, ChosenWord),
        remove_elem(WordList, ChosenWord, RemainingWordList1),
        solve_with_search(Puzzle0, Slots, RemainingWordList1, Puzzle,
            RemainingWordList)
    ).

% Chooses a word from a word list that matches
% a slot. Unifies the slot with the chosen word.
% Can rechoose all possible words for a slot
% when backtracking.
choose_word(Slot, WordList, ChosenWord) :-
    find_slot_matches(Slot, WordList, Matches),
    get_next(Matches, ChosenWord),
    Slot = ChosenWord.

% Gets the next element in a list
% when backtracking, by leaving a
% choicepoint each time.
get_next([X|_], X).
get_next([_|Xs], Y) :-
    get_next(Xs, Y).
