%:- use_rendering(table).
:- use_module(library(clpfd)).
:- style_check(-singleton).

% Sudoku tradicional.
sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    sudoku_squares(As, Bs, Cs),
    sudoku_squares(Ds, Es, Fs),
    sudoku_squares(Gs, Hs, Is).

sudoku_squares([], [], []).
sudoku_squares(	[N1,N2,N3|Ns1],
          		  [N4,N5,N6|Ns2],
        		    [N7,N8,N9|Ns3])	:-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    sudoku_squares(Ns1, Ns2, Ns3).


% Super Sudoku - Cuadricula 16x16.
super_sudoku(Rows) :-
    length(Rows, 16),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..16,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is,Js,Ks,Ls,Ms,Ns,Os,Ps],
    super_sudoku_squares(As, Bs, Cs, Ds),
    super_sudoku_squares(Es, Fs, Gs, Hs),
    super_sudoku_squares(Is, Js, Ks, Ls),
    super_sudoku_squares(Ms, Ns, Os, Ps).

super_sudoku_squares([], [], [], []).
super_sudoku_squares([N1,N2,N3,N4|Ns1],
        [N5,N6,N7,N8|Ns2],
        [N9,N10,N11,N12|Ns3],
        [N13,N14,N15,N16|Ns4]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16]),
    super_sudoku_squares(Ns1, Ns2, Ns3, Ns4).


% Win Sudoku - Cuatro cuadrantes centrales añadidos.
win_sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    sudoku_squares(As, Bs, Cs),
    sudoku_squares(Ds, Es, Fs),
    sudoku_squares(Gs, Hs, Is),
    win_sudoku_squares(Bs, Cs, Ds),
	  win_sudoku_squares(Fs, Gs, Hs).

win_sudoku_squares(	[_,L1,L2,L3,_,R1,R2,R3,_],
          			    [_,L4,L5,L6,_,R4,R5,R6,_],
        			      [_,L7,L8,L9,_,R7,R8,R9,_])	:-
    all_distinct([L1,L2,L3,L4,L5,L6,L7,L8,L9]),
    all_distinct([R1,R2,R3,R4,R5,R6,R7,R8,R9]).


% Fortress sudoku - Determinadas posiciones tienen que ser > que todos los colindantes.
fortress_sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    sudoku_squares(As, Bs, Cs),
    sudoku_squares(Ds, Es, Fs),
    sudoku_squares(Gs, Hs, Is),
    fortress(Rows).

fortress([ [A1,A2,A3,B1,B2,B3,C1,C2,C3],
           [A4,A5,A6,B4,B5,B6,C4,C5,C6],
           [A7,A8,A9,B7,B8,B9,C7,C8,C9],
           [D1,D2,D3,E1,E2,E3,F1,F2,F3],
           [D4,D5,D6,E4,E5,E6,F4,F5,F6],
           [D7,D8,D9,E7,E8,E9,F7,F8,F9],
           [G1,G2,G3,H1,H2,H3,I1,I2,I3],
           [G4,G5,G6,H4,H5,H6,I4,I5,I6],
           [G7,G8,G9,H7,H8,H9,I7,I8,I9] ])	:-

	  greaterThanList(B4, [A3, A6, A9, B1, B2, B5, B7]),
    greaterThanList(B8, [B5, B6, B7, B9, E1, E3]),
    greaterThanList(D6, [D2, D3, D5, D9, E1, E7]),
    greaterThanList(D8, [D4, D5, D7, D9, G1, G2, G3]),
    greaterThanList(E2, [B7, B9, E1, E3]),
    greaterThanList(E4, [D3, D9, E1, E7]),
    greaterThanList(E5, [E1, E3, E7, E9]),
    greaterThanList(E6, [E3, E9, F1, F7]),
    greaterThanList(E8, [E7, E9, H1, H3]),
    greaterThanList(F2, [C7, C8, C9, F1, F3, F5, F6]),
    greaterThanList(F4, [E3, E9, F1, F5, F7, F8]),
    greaterThanList(H2, [E7, E9, H1, H3, H4, H5]),
    greaterThanList(H6, [H3, H5, H8, H9, I1, I4, I7]).

greaterThanList(_, []).
greaterThanList(X, [H|T]) :-
    X #> H,
    greaterThanList(X, T).

%  Diagonal Sudoku - Restricción de dos diagonales más.
diagonal_sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    sudoku_squares(As, Bs, Cs),
    sudoku_squares(Ds, Es, Fs),
    sudoku_squares(Gs, Hs, Is),
    diagonals(Rows).

diagonals([ [A1,A2,A3,B1,B2,B3,C1,C2,C3],
            [A4,A5,A6,B4,B5,B6,C4,C5,C6],
            [A7,A8,A9,B7,B8,B9,C7,C8,C9],
            [D1,D2,D3,E1,E2,E3,F1,F2,F3],
            [D4,D5,D6,E4,E5,E6,F4,F5,F6],
            [D7,D8,D9,E7,E8,E9,F7,F8,F9],
            [G1,G2,G3,H1,H2,H3,I1,I2,I3],
            [G4,G5,G6,H4,H5,H6,I4,I5,I6],
            [G7,G8,G9,H7,H8,H9,I7,I8,I9] ])	:-

Diag1 = [A1, A5, A9, E1, E5, E9, I1, I5, I9],
Diag2 = [C3, C5, C7, E3, E5, E7, G3, G5, G7],
all_distinct(Diag1),
all_distinct(Diag2).
