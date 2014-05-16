/*
    Define the different parts of the ship
*/
shipInput(n).
shipInput(e).
shipInput(s).
shipInput(w).
shipInput(x).
shipInput(o).


/*
    Define all possible horizontal combinations between adjunct cells
*/
legalHorizontalCombination(w, e).
legalHorizontalCombination(w, x).
legalHorizontalCombination(x, x).
legalHorizontalCombination(x, e).
legalHorizontalCombination('~', n).
legalHorizontalCombination(n, '~').
legalHorizontalCombination(e, '~').
legalHorizontalCombination('~', s).
legalHorizontalCombination(s, '~').
legalHorizontalCombination('~', w).
legalHorizontalCombination('~', x).
legalHorizontalCombination(x, '~').
legalHorizontalCombination('~', o).
legalHorizontalCombination(o, '~').


/*
    Define all possible vertical combinations between adjunct cells
*/
legalVerticalCombination(n, s).
legalVerticalCombination(n, x).
legalVerticalCombination(x, x).
legalVerticalCombination(x, s).
legalVerticalCombination('~', n).
legalVerticalCombination('~', e).
legalVerticalCombination(e, '~').
legalVerticalCombination(s, '~').
legalVerticalCombination('~', w).
legalVerticalCombination(w, '~').
legalVerticalCombination('~', x).
legalVerticalCombination(x, '~').
legalVerticalCombination('~', o).
legalVerticalCombination(o, '~').


/*
    Create a column full of water to be placed on the sides of the given grid
*/
appendBorderColumns([[Head | Tail]], [NewRow]) :-
    append([Head | Tail], ['~'], TempRow),
    append(['~'], TempRow, NewRow).

appendBorderColumns([[Head | Tail] | Tail2], [NewRow | Tail3]) :-
    append([Head | Tail], ['~'], TempRow),
    append(['~'], TempRow, NewRow),
    appendBorderColumns(Tail2, Tail3).


/*
    Create a row full of water to be placed above and below the given grid
*/
createBorderRow([[_] | _], ['~']).
createBorderRow([[_ | Tail] | _], ['~' | Row]) :- createBorderRow([Tail], Row).


/*
    Make sure the contents of the columns is allowed with the given checksum
*/
checkColumnChecksum([[Head]], [], 1) :- not(Head == '~').
checkColumnChecksum([[Head] | Tail], [], ColumnChecksum) :-
    ColumnChecksum > 0,
    not(Head == '~'),
    NewColumnChecksum is ColumnChecksum-1,
    checkColumnChecksum(Tail, [], NewColumnChecksum).

checkColumnChecksum([[Head | Tail]], [Tail], 1) :- not(Head == '~').
checkColumnChecksum([[Head | Tail] | Tail2], [Tail | NewTail], ColumnChecksum) :-
    ColumnChecksum > 0,
    not(Head == '~'),
    NewColumnChecksum is ColumnChecksum-1,
    checkColumnChecksum(Tail2, NewTail, NewColumnChecksum).

checkColumnChecksum([['~']], [], 0).
checkColumnChecksum([['~'] | Tail], [], ColumnChecksum) :- checkColumnChecksum(Tail, [], ColumnChecksum).
checkColumnChecksum([['~' | Tail]], [Tail], 0).
checkColumnChecksum([['~' | Tail] | Tail2], [Tail | NewTail], ColumnChecksum) :-
    checkColumnChecksum(Tail2, NewTail, ColumnChecksum).


/*
    Make sure all columns have correct contents in relation to their checksums
*/
checkColumnChecksums([[Head] | Tail], [ColumnChecksum]) :-
    checkColumnChecksum([[Head] | Tail], [], ColumnChecksum).

checkColumnChecksums([[Head | Tail] | Tail2], [FirstColumnChecksum | OtherColumnChecksums]) :-
    checkColumnChecksum([[Head | Tail] | Tail2], NewGrid, FirstColumnChecksum),
    checkColumnChecksums(NewGrid, OtherColumnChecksums).


/*
    Make sure the contents of the row is allowed with the given checksum
*/
checkRowChecksum([], 0).
checkRowChecksum([Head | Tail], Checksum) :- not(Head == '~'), Checksum2 is Checksum-1, checkRowChecksum(Tail, Checksum2).
checkRowChecksum(['~' | Tail], Checksum) :- checkRowChecksum(Tail, Checksum).


/*
    Make sure all rows have correct contents in relation to their checksums
*/
checkRowChecksums([[Head | Tail]], [Checksum]) :- checkRowChecksum([Head | Tail], Checksum).
checkRowChecksums([[Head | Tail] | Tail2], [FirstChecksum | OtherChecksums]) :-
    checkRowChecksum([Head | Tail], FirstChecksum),
    checkRowChecksums(Tail2, OtherChecksums).


/*
    Make sure that the cell has legal adjenct cells
*/
checkCell([['~', '~', '~'], [E21, x, E23], ['~', '~', '~']]) :-
    shipInput(E22), shipInput(E21), shipInput(E23),
    legalHorizontalCombination(E21, E22),
    legalHorizontalCombination(E22, E23).
    
checkCell([['~', '~', '~'], [E21, E22, E23], ['~', '~', '~']]) :-
    not(E22 == x), shipInput(E22), (shipInput(E21); shipInput(E23)),
    legalHorizontalCombination(E21, E22),
    legalHorizontalCombination(E22, E23).

checkCell([['~', E12, '~'], ['~', x, '~'], ['~', E32, '~']]) :-
    shipInput(E22), shipInput(E12), shipInput(E32),
    legalVerticalCombination(E12, E22),
    legalVerticalCombination(E22, E32).

checkCell([['~', E12, '~'], ['~', E22, '~'], ['~', E32, '~']]) :-
    not(E22 == x), shipInput(E22), (shipInput(E12); shipInput(E32)),
    legalVerticalCombination(E12, E22),
    legalVerticalCombination(E22, E32).
    
checkCell([['~', '~', '~'], ['~', o, '~'], ['~', '~', '~']]).


/*
    Check all cells to make sure that they can be placed next to each other
*/
checkAllCells(_, [['~', E12, '~'], [E21, E22, '~'], ['~', '~', '~']]) :-
    checkCell([['~', E12, '~'], [E21, E22, '~'], ['~', '~', '~']]).

checkAllCells(_, [[_, _, '~'], [_, '~', '~'], ['~', '~', '~']]).

checkAllCells(Grid, [['~', E12, '~' | Tail1], [E21, E22, E23 | Tail2], ['~', '~', '~' | Tail3]]) :-
    checkCell([['~', E12, '~'], [E21, E22, E23], ['~', '~', '~']]),
    checkAllCells(Grid, [[E12, '~' | Tail1], [E22, E23 | Tail2], ['~', '~' | Tail3]]).
   
checkAllCells(Grid, [[_, E12, E13 | Tail1], [_, '~', E23 | Tail2], ['~', '~', '~' | Tail3]]) :-
    checkAllCells(Grid, [[E12, E13 | Tail1], ['~', E23 | Tail2], ['~', '~' | Tail3]]).

checkAllCells([_, SecondGridRow | OtherGridRows], [['~', E12, '~'], [E21, E22, '~'], ['~', E32, '~'] | _]) :-
    checkCell([['~', E12, '~'], [E21, E22, '~'], ['~', E32, '~']]),
    checkAllCells([SecondGridRow | OtherGridRows], [SecondGridRow | OtherGridRows]).

checkAllCells([_, SecondGridRow | OtherGridRows], [[_, _, '~'], [_, '~', '~'], [_, _, '~'] | _]) :-
    checkAllCells([SecondGridRow | OtherGridRows], [SecondGridRow | OtherGridRows]).
 
checkAllCells(Grid, [['~', E12, '~' | Tail1], [E21, E22, E23 | Tail2], ['~', E32, '~' | Tail3] | Tail]) :-
    checkCell([['~', E12, '~'], [E21, E22, E23], ['~', E32, '~']]),
    checkAllCells(Grid, [[E12, '~' | Tail1], [E22, E23 | Tail2], [E32, '~' | Tail3] | Tail]).

checkAllCells(Grid, [[_, E12, E13 | Tail1], [_, '~', E23 | Tail2], [_, E32, E33 | Tail3] | Tail]) :-
    checkAllCells(Grid, [[E12, E13 | Tail1], ['~', E23 | Tail2], [E32, E33 | Tail3] | Tail]).


/*
    Retrieve a specific element in a list
*/
getElementInRow([Head], 1, Head) :- !.
getElementInRow([Head | _], 1, Head) :- !.
getElementInRow([_ | Tail], ElementNr, Result) :- ElementNr2 is ElementNr-1, getElementInRow(Tail, ElementNr2, Result).


/*
    Changes a specific element in a list
*/
setElementInRow([_], 1, Element, [Element]) :- !.
setElementInRow([_ | Tail], 1, Element, [Element | Tail]) :- !.
setElementInRow([Head | Tail], ElementNr, Element, Result) :-
    ElementNr > 1,
    NewElementNr is ElementNr-1,
    setElementInRow(Tail, NewElementNr, Element, OldResult),
    append([Head], OldResult, Result).


/*
    Retrieve a specific column in the grid
*/
getColumn([[FirstElement | OtherElements]], ColumnNr, [NeededElementInRow]) :-
    !, getElementInRow([FirstElement | OtherElements], ColumnNr, NeededElementInRow).
getColumn([[FirstElement | OtherElements] | OtherRows], ColumnNr, [NeededElementInRow | OtherElementsInColumn]) :-
    getColumn(OtherRows, ColumnNr, OtherElementsInColumn),
    getElementInRow([FirstElement | OtherElements], ColumnNr, NeededElementInRow).


/*
    Check if the whole list contains of zeros
*/
zeroList([0]).
zeroList([0 | Tail]) :- zeroList(Tail).


/*
    Retrieve the size of a ship
*/
getShipSize([s], [], 1).
getShipSize([e], [], 1).
getShipSize([s | Tail], Tail, 1).
getShipSize([e | Tail], Tail, 1).

getShipSize([x | Tail], Remainder, Size) :-
    !, getShipSize(Tail, Remainder, OldSize),
    Size is OldSize+1.

getShipSize([n | Tail], Remainder, Size) :-
    !, getShipSize(Tail, Remainder, OldSize),
    Size is OldSize+1.

getShipSize([w | Tail], Remainder, Size) :-
    !, getShipSize(Tail, Remainder, OldSize),
    Size is OldSize+1.


/*
    Retrieve the length of a given list
*/
getListLength([], 0) :- !.
getListLength([_], 1) :- !.
getListLength([_ | Tail], Length) :- getListLength(Tail, OldLength), Length is OldLength+1, !.


/*
    Find out how many elements you have already removed from the first row
*/
findColumnNumber([[_], [_]], 1).
findColumnNumber([[_], [_] | _], 1).
findColumnNumber([[_], [_ | Tail]], Result) :- getListLength(Tail, Length), Result is Length+1.
findColumnNumber([[_], [_ | Tail] | _], Result) :- getListLength(Tail, Length), Result is Length+1.
findColumnNumber([[_ | Tail], [_ | Tail2]], Result) :- findColumnNumber([Tail, Tail2], Result).
findColumnNumber([[_ | Tail], [_ | Tail2] | Tail3], Result) :- findColumnNumber([Tail, Tail2 | Tail3], Result), !.


/*
    Check if the grid contains the right amount of ships
*/
checkShips([['~']], Ships) :- !, zeroList(Ships).
checkShips([['~' | Tail]], Ships) :- !, checkShips([Tail], Ships).
checkShips([['~'] | Tail], Ships) :- !, checkShips(Tail, Ships).
checkShips([['~' | Tail] | Tail2], Ships) :- !, checkShips([Tail | Tail2], Ships).
checkShips([[x | Tail] | Tail2], Ships) :- !, checkShips([Tail | Tail2], Ships).
checkShips([[s | Tail] | Tail2], Ships) :- !, checkShips([Tail | Tail2], Ships).
checkShips([[e | Tail] | Tail2], Ships) :- !, checkShips([Tail | Tail2], Ships).

checkShips([[n | Tail] | Tail2], Ships) :-
    !, findColumnNumber([[n | Tail] | Tail2], ColumnNumber),
    getColumn(Tail2, ColumnNumber, Column),
    getShipSize([n | Column], _, Size),
    getElementInRow(Ships, Size, AmountOfShips),
    AmountOfShips > 0,
    NewAmountOfShips is AmountOfShips-1,
    setElementInRow(Ships, Size, NewAmountOfShips, NewShips),
    checkShips([Tail | Tail2], NewShips).

checkShips([[w | Tail] | Tail2], Ships) :-
    !, getShipSize([w | Tail], RowRemainder, Size),
    getElementInRow(Ships, Size, AmountOfShips),
    AmountOfShips > 0,
    NewAmountOfShips is AmountOfShips-1,
    setElementInRow(Ships, Size, NewAmountOfShips, NewShips),
    checkShips([RowRemainder | Tail2], NewShips).

checkShips([[o | Tail] | Tail2], Ships) :-
    !, getElementInRow(Ships, 1, AmountOfShips),
    AmountOfShips > 0,
    NewAmountOfShips is AmountOfShips-1,
    setElementInRow(Ships, 1, NewAmountOfShips, NewShips),
    checkShips([Tail | Tail2], NewShips).


/*
    Output the grid
*/
printGrid([[Head]]) :- write(Head), nl.
printGrid([[Head | Tail]]) :- write(Head), write(' '), printGrid([Tail]).
printGrid([[Head] | Tail]) :- write(Head), nl, printGrid(Tail).
printGrid([[Head | Tail] | Tail2]) :- write(Head), write(' '), printGrid([Tail | Tail2]).


/*
    Entry point of the program
*/
battleship(Grid, ColumnChecksums, RowChecksums, Ships, Grid) :-
    appendBorderColumns(Grid, TempGrid1),
    createBorderRow(TempGrid1, BorderRow),
    append(TempGrid1, [BorderRow], TempGrid2),
    append([BorderRow], TempGrid2, NewGrid),
    checkColumnChecksums(Grid, ColumnChecksums),
    checkRowChecksums(Grid, RowChecksums),
    checkAllCells(NewGrid, NewGrid),
    checkShips(NewGrid, Ships),
    printGrid(Grid), !.

