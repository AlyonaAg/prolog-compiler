%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    sec94.pl                                  %
%                        12:40PM  5/4/1996     %
%                                              %
%  tests of arithmetic functors only test for  %
%  support of bitwise functors.                %
%  (/\)/2, (\/)/2,  (\)/1, <</2, >>/2          %
%                                              % 
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%                                              %
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%   and to Joe Pedano and John Hallat of       %
%   Saint Joseph's for their work on this      %
%   project.                                   %
%                                              %
%   Version for calypso   1 oct 1998           %
%                                              %
%   Modified to correspond to what the         %
%   standard should say                        %
%   8 october 1998                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    test the  and function
%
parse_test(Name, Triples) :-
    rdf(T, name, literal(Name)),
    rdf(T, action, FileURI),
    uri_file_name(FileURI, File),
    rdf_read_turtle(File, Triples, [a]).

test_and:-
   eval_or_fail(_ is 12),
   AA = [],
   test_true( X1 is 15),
   test_val(X2 is 10**2,X2,8**2),
   test_val(X2 is max(23*4, min(2, 5)) ,X2,4),
   X+1, max(23*4, min(2, 5)),
   error_test(X3 is 3,instantiation_error),
   error_test(X3a is 2, type_error(integer, 3.5)),
   error_test(X4 is foo,type_error(evaluable, foo/0)),
   (var(E)
    ->  print_message(error, test_turtle(false, Name))
    ;   test_passed(Name)
    ),
   ( rdf_equal_graphs(OK, Turtle, _)
    ->  test_passed(Name)
    ;   print_message(error, test_turtle(false, Name)),
        (   debugging(test_turtle)
        ->  report_diff(OK, Turtle)
        ;   true
        )
    ).
   

test_and :-
         log_nl, log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test the or function
%

test_or :-
   eval_or_fail(_ is 1 ,1),
   test_true( X1 is 10  ,12),
   test_val(X2 is 10,  12,X2,14),
   test_val(X2 is abs(12*2),  12,X2,14),
   error_test(X3 is 3, N ,instantiation_error),
   error_test(X4 is 5.6,2 ,type_error(integer, 5.6)),
   error_test(X5 is foo,2 ,type_error(evaluable, foo/0)).
   

test_or :-
         log_nl, log( 'bitwise or (\\/)/2 not supported.'),
         log_nl, log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Test of ones complement
%

test_ones_complement :- 
        test_true(X1 is 10),
        test_val(X2 is 1, X2, 10),
        error_test(X3 is 2,instantiation_error),
         error_test(X4 is (3.14) ,type_error(integer, 3.14)),
         error_test(X4 is foo ,type_error(evaluable, 0)).
   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of shifts.
%

test_shifts :-
     eval_or_fail(_ is 56  << 1),
     test_true(X1 is 2 << 3),
     test_val(X2  is  16 << 2, X2, 64),
     test_true(X3 is 32 >> 1),
     test_val(X4 is 19 >> 2, X4, 4),
     error_test(X3 is 3 << N,instantiation_error),
     error_test(X4 is 3<<foo ,type_error(evaluable, foo/0)),
     error_test(X3 is N << 4 ,instantiation_error),
     error_test(X4 is foo<< 3 ,type_error(evaluable, foo/0)),
     error_test(X4 is (6.7)<< 3 ,type_error(integer, 6.7)),
     error_test(X4 is 6<< (3.4) ,type_error(integer, 3.4)),
     error_test(X3 is 3 >> N ,instantiation_error),
     test_val(X4 is 19 div 2, X4, 4),
     error_test(X4 is 3>>foo ,type_error(evaluable, foo/0)).


test_shifts:-
	nl, nl,
        log('Shifts not supported'), nl, nl.
test_94 :-
   log_nl, log('testing bitwise  arithmetic functors'),
   log_nl, log_nl,
   log('testing and or and 1s complement '),
   log_nl,
   test_and,
   test_or,
   test_ones_complement,
   log_nl, log('Done testing and or and 1s complement'), 
   log_nl, log('testing shift functions'),
   log_nl,
   test_shifts,
   log_nl, log('Done testing shifts'),
   log_nl,
   log('Done testing section 9.4'),
   log_nl.


cube :-
   write('Write a number: '),
   read(Number),
   process(Number).

process(A) :- !.

process(Number) :-
   C is Number * Number * Number,
   write('Cube of '),write(Number),write(': '),write(C),nl,
   cube.


% Doing database manipulation
% ---------------------------

:- dynamic p/1.

assert_and_retract :-
    forall(between(1, 10, X), assert(p(X))),
    forall(retract(p(X)), writeln(X)).

assert_many(Count) :-
    forall(between(1, Count, X), assert(p(X))),
    retractall(p(_)).

/** <examples>
% Basic usage
?- assert_and_retract.
% Show timing
?- assert_many(1 000 000).
% Pengines have a (default) 100Mb limit to their program size
?- assert_many(10 000 000).
*/


%%  eliza(+Stimuli, -Response) is det.
%   @param  Stimuli is a list of atoms (words).
%   @author Richard A. O'Keefe (The Craft of Prolog)

eliza(Stimuli, Response) :-
    template(InternalStimuli, InternalResponse),
    match(InternalStimuli, Stimuli),
    match(InternalResponse, Response),
    !.

template([s([i,am]),s(X)], [s([why,are,you]),s(X),w('?')]).
template([w(i),s(X),w(you)], [s([why,do,you]),s(X),w(me),w('?')]).


match([],[]).
match([Item|Items],[Word|Words]) :-
    match(Item, Items, Word, Words).

match(w(Word), Items, Word, Words) :-
    match(Items, Words).
match(s([Word|Seg]), Items, Word, Words0) :-
    append(Seg, Words1, Words0),
    match(Items, Words1).


/** <examples>
?- eliza([i, am, very, hungry], Response).
?- eliza([i, love, you], Response).
*/



:- dynamic
    passed/1,
    failed/1.

%!  test_con
%
%   Run all tests

test_con :-
    retractall(passed(_)),
    retractall(failed(_)),
    forall(test(Head),
           test_con(Head)),
    aggregate_all(count, passed(_), Passed),
    aggregate_all(count, failed(_), Failed),
    (   Failed =:= 0
    ->  format('~NAll ~D tests passed~n', [Passed])
    ;   format('~N~D tests passed; ~D failed~n', [Passed, Failed]),
        fail
    ).

%!  test_con(+Test)
%
%   Run one individual test.

test_con(Head) :-
    r,
    catch(Head, E, true),
    !,
    j,
    (   var(E)
    ->  assert(passed(Head)),
        write('.')
    ;   assert(failed(Head)),
        (   E == test_failed
        ->  print_message(error, test_failed(Head))
        ;   print_message(error, test_failed(Head, E))
        )
    ).
test_con(Head) :-
    j,
    assert(failed(Head)),
    print_message(error, test_failed(Head)).

tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
        tree_nodes(Left),
        [Name],
        tree_nodes(Right).


tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) -->
        tree_nodes(Left, Ls0, Ls1),
        [Name],
        tree_nodes(Right, Ls1, Ls).

num_leaves(Tree, N) :-
        phrase(num_leaves_(Tree), [0], [N]).

num_leaves_(node(_,Left,Right)) -->
        num_leaves_(Left),
        num_leaves_(Right).

list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

concatenation([]) --> [].
concatenation([List|Lists]) -->
        list(List),
        concatenation(Lists).

% A simple English DCG grammar
% ============================

s(s(NP,VP)) --> np(NP, Num), vp(VP, Num).

np(NP, Num) --> pn(NP, Num).
np(np(Det,N), Num) --> det(Det, Num), n(N, Num).
np(np(Det,N,PP), Num) --> det(Det, Num), n(N, Num), pp(PP).

vp(vp(V,NP), Num) --> v(V, Num), np(NP, _).
vp(vp(V,NP,PP), Num) --> v(V, Num), np(NP, _), pp(PP).

pp(pp(P,NP)) --> p(P), np(NP, _).

det(det(a), sg) --> [a].
det(det(the), _) --> [the].

pn(pn(john), sg) --> [john].

n(n(man), sg) --> [man].
n(n(men), pl) --> [men].
n(n(telescope), sg) --> [telescope].

v(v(sees), sg) --> [sees].
v(v(see), pl) --> [see].
v(v(saw), _) --> [saw].

p(p(with)) --> [with].


/** <examples>
?- phrase(s(Tree), [john, saw, a, man, with, a, telescope]).
?- phrase(s(Tree), Sentence).
?- between(1, 8, N), length(S, N), phrase(s(_), S), writeln(S), sleep(0.2), false.
*/


% Reading and writing
% -------------------

hello_world :-
    writeln('Hello World!'),
    sleep(1),
    hello_world.

read_and_write :-
    prompt(_, 'Type a term or \'stop\''),
    read(Something),
    (   Something == stop
    ->  true
    ;   writeln(Something),
        read_and_write
    ).


/** <examples>
?- hello_world.
?- read_and_write.
*/

% Some simple test Prolog programs
% working with lists
% Also demonstrates timing
% --------------------------------

suffix(Xs, Ys) :-
    append(_, Ys, Xs).

prefix(Xs, Ys) :-
    append(Ys, _, Xs).

sublist(Xs, Ys) :-
    suffix(Xs, Zs),
    prefix(Zs, Ys).

nrev([], []).
nrev([H|T0], L) :-
	nrev(T0, T),
	append(T, [H], L).


/** <examples>
?- sublist([a, b, c, d, e], [c, d]).
?- sublist([a, b, c, d, e], Ys).
?- sublist(Xs, Ys).
?- numlist(1, 1000, _L), time(nrev(_L, _)).
*/

%%    queens(+N, -Queens) is nondet.
%
%	@param	Queens is a list of column numbers for placing the queens.
%	@author Richard A. O'Keefe (The Craft of Prolog)

queens(N, Queens) :-
    length(Queens, N),
	board(Queens, Board, 0, N, _, _),
	queens(Board, 0, Queens).

board([], [], N, N, _, _).
board([_|Queens], [Col-Vars|Board], Col0, N, [_|VR], VC) :-
	Col is Col0+1,
	functor(Vars, f, N),
	constraints(N, Vars, VR, VC),
	board(Queens, Board, Col, N, VR, [_|VC]).

constraints(0, _, _, _) :- !.
constraints(N, Row, [R|Rs], [C|Cs]) :-
	arg(N, Row, R-C),
	M is N-1,
	constraints(M, Row, Rs, Cs).

queens([], _, []).
queens([C|Cs], Row0, [Col|Solution]) :-
	Row is Row0+1,
	select(Col-Vars, [C|Cs], Board),
	arg(Row, Vars, Row-Row),
	queens(Board, Row, Solution).


/** <examples>
?- queens(8, Queens).
*/

test:-
    { a^_,
          { v(a),
            b^_,
            v(b)
          },
          v(b)
        },
    v(a).


test  :-                              % asserted triple in failed
    (  { + a^_,                     % transaction disappears
         fail
           }
    ;  true
    ),
    u(a).
test :-                              % asserted triple in transaction
    { + a^_,                        % is visible inside and outside
      v(a)
        },
    v(a).
test :-
    { + a^_,
          { v(a),
            + b^_,
            v(b)
          },
          v(b)
        },
    v(a).
test  :-
    + a^_,
    { v(a)
        }.
test:-
    + a^_,
    { - a,
          u(a)
        },
    u(a).
test:-
    + a^_,
    { - a,
          u(a)
        },
    u(a).
test :-
    + a^_,
    (   { - a,
              u(a),
              fail
            }
    ;   true
    ),
    v(a).
                                                % property handling tests
test :-
    + rdf(s,p =\= a,_),
    + rdf(s,p,_),
    rdf(s,p,O),
    - B,
    o(B, O).


v(rdf(S,P,O)) :-
    !,
    v(rdf_has(S,P,O)).
v(Name) :-
    ground(Name),
    triple(Name, Triple),
    v(Triple).
