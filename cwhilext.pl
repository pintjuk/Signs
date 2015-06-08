:- use_module(library(assoc)).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
%############# UTILS ################

throw_error(Message) -->
    spaces,nonblanks(S),
    lazy_list_location(file(F, L, C, _)),
    {
        format('In ~w at ~w:~w:~n~|',[F, L, C]),
        format('   syntax error: ~s, found: ~s', [Message, S]),
        halt
    }.
throw_error(Message) -->
    {
        write(Message),
        halt
    }.
%############# PARSER CODE ########################
digit(N)--> [S],{char_type(S, digit(N))}.

n(N)    --> digit(D), n(D, N).
n(A, N) --> digit(D), {A1 is A*10+D}, n(A1, N).
n(N, N) --> [].


spaces--> " ", spaces.
spaces--> "\n", spaces.
spaces--> [].

lcl(C)      --> [C], {char_type(C, lower)}.
var(A)      --> lcl(C), var([C], L), {atom_to_chars(A, L)}.
var(L, N)   --> lcl(C), {append(L, [C], L2)}, var(L2, N). 
var(N, N)   --> [].

a(num(N))   -->  n(N).
a(var(X))   --> var(X).
a(add(X, Y))-->"(",spaces, a(X),spaces, "+",spaces, a(Y), spaces,")".
a(mul(X, Y))-->"(",spaces, a(X),spaces, "*",spaces, a(Y),spaces, ")".
a(sb(X, Y)) -->"(",spaces, a(X),spaces, "-",spaces, a(Y),spaces, ")".
a(div(X, Y))-->"(",spaces, a(X),spaces, "/",spaces, a(Y),spaces, ")".

b(true)     --> "TRUE".
b(false)    --> "FALSE".
b(not(X))   --> "!",!, spaces, (b(X); throw_error('expected boolian expresion')),!.
b(and(X, Y))--> "(", spaces, b(X),spaces, "&", spaces, b(Y),spaces, ")".
b(eq(X, Y)) --> "(", spaces, a(X),spaces, "=", spaces, a(Y),spaces, ")".
b(leq(X, Y))--> "(", spaces, a(X),spaces, "<=",spaces, a(Y),spaces, ")".

statment(asign(Var, Val))   --> 
    var(Var), 
    spaces,
    ":=",
    (
        spaces, a(Val)%;
        % throw_error('expected arethmetic exp!')
    ).
statment(skip)              --> "SKIP".
statment(cond(B, S1, S2))   --> "IF",!,
    (   
        spaces, b(B); 
        throw_error('expecting bool exp!')
    ),!,
    (   
        spaces,"THEN"; 
        throw_error('expected \'THEN\'')
    ),!,
    spaces,
    metastatment(S1), 
    (   
        spaces,"ELSE";
        throw_error('expected \'ELSE\'')
    ),!,
    spaces,
    metastatment(S2), 
    (
        spaces,"END";
        throw_error('expected \'END\'') 
    ),!.

statment(loop(B, S))--> 
    "WHILE",!, 
    spaces, 
    (
        b(B);
        throw_error('expected bool exp')
    ),!,
    spaces, 
    (
        "DO";
        throw_error('expected \'DO\'')
    ),!,
    spaces, 
    metastatment(S),
    spaces, 
    (
        "END";
        throw_error('expected \'END\'')
    ),!.
metastatment(try(S1, S2))   --> spaces, "TRY", spaces, metastatment(S1), spaces, "CATCH", spaces, metastatment(S2), spaces, "END".
metastatment(comp(S1, S2))  --> spaces, statment(S1),spaces,";",spaces, metastatment(S2),spaces.
metastatment(X)             --> statment(X).

%################# Lable ###############################

lableE(var(X), var(X, Num), Num).
lableE(true, true(Num), Num).
lableE(false, false(Num), Num).
lableE(not(X), not(X1, Num), Num):-
    lableE(X, X1, Num).
lableE(and(X, Y), and(X1, Y1), Num):-
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).
lableE(eq(X, Y), eq(X1, Y1, Num), Num):-
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).
lableE(leq(X, Y), leq(X1, Y1, Num), Num):-
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).

lableE(num(N), num(N, Num), Num).

lableE(var(X), var(X1, Num), Num):-
    lableE(X, X1, Num).

lableE(add(X, Y), add(X1, Y1, Num), Num):-
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).

lableE(mul(X, Y), mul(X1, Y1, Num), Num):- 
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).

lableE(sb(X, Y), sb(X1, Y1, Num), Num):-
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).

lableE(div(X, Y), div(X1, Y1, Num), Num):-
    lableE(X, X1, Num),
    lableE(Y, Y1, Num).

lable(comp(S1, S2), comp(S1_l, S2_l), Big, End):-
    lable(S1, S1_l, Big, Inter),
    lable(S2, S2_l, Inter, End).

lable(try(S1, S2), try(S1_l, S2_l, Cur), Big, End):-
    Cur is Big,
    lable(S1, S1_l, Big+1, Inter),
    lable(S2, S2_l, Inter, End).

lable(cond(B, S1, S2), cond(B1, S1_l, S2_l, Cur), Big, End):-
    Cur is Big,
    lableE(B, B1, Cur),
    lable(S1, S1_l, Big+1, Inter),
    lable(S2, S2_l, Inter, End).

lable(loop(B, S1), loop(B1, S1_l, Cur), Big, Cur):-
    Cur is Big,
    lableE(B, B1, Cur),
    lable(S1, S1_l, Big+1, End).

lable(skip, skip(Cur), Big, Big+1):-
    Cur is Big.

lable(asign(Var, Val), asign(Var, Val1, Cur), Big, Big+1):-
    Cur is Big,
    lableE(Val, Val1, Cur).

%################# COMPILER CODE ###################################

ca(num(X, Num), [puch(X, Num)]).
ca(var(X, Num), [fetch(X, Num)]).
ca(add(A1, A2, Num), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [add(Num)]], Code).

ca(mul(A1, A2, Num), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [mult(Num)]], Code).

ca(sb(A1, A2, Num), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [subt(Num)]], Code).

ca(div(A1, A2, Num), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [div(Num)]], Code).

cb(true(Num), [true(Num)]).
cb(false(Num), [false(Num)]).

cb(eq(A1, A2, Num), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [eq(Num)]], Code).

cb(leq(A1, A2, Num), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [le(Num)]], Code).

cb(not(X, Num), Code):-
    cb(X, Code1),
    append([Code1, [neg(Num)]], Code).

cb(and(A1, A2, Num), Code):-
    cb(A1, Code1),
    cb(A2, Code2),
    append([Code2, Code1, [and(Num)]], Code).

cs(asign(X, A, Num), Code):-
    ca(A, ACode),
    append([ACode, [store(X, Num)]], Code).

cs(skip(Num), [noop(Num)]).

cs(comp(S1, S2), Code):-
    cs(S1, Code1),
    cs(S2, Code2),
    append([Code1,Code2], Code).

cs(cond(B, S1, S2, Num), Code):-
    cb(B, Bcode),
    cs(S1, S1code),
    cs(S2, S2code),
    append([Bcode,[branch(S1code, S2code, Num)]], Code).

cs(loop(B, S, Num), [loop(Bcode, Scode, Num)]):-
    cb(B, Bcode),
    cs(S, Scode).

cs(try(S1, S2, Num), Code):-
    cs(S1, S1code),
    cs(S2, S2code),
    append([[try(Num)], S1code, [catch(S2code, Num)]], Code).

%#################### VM CODE ######################
%#################### abstract operations ##########
lub(X, X, X).
lub(none, X, X).
lub(X, none, X).

lub(any, _, any).
lub(_, any, any).
lub(err, _, any).
lub(_, err, any).
lub(z, _, z).
lub(_, z, z).

lub(neg, zero, non_pos).
lub(neg, pos, non_zero).
lub(neg, non_pos, non_pos).
lub(neg, non_zero, non_zero).
lub(neg, non_neg, z).
lub(zero, neg, non_pos).
lub(zero, pos, non_neg).
lub(zero, non_pos, non_pos).
lub(zero, non_zero, z).
lub(zero, non_neg, non_neg).
lub(pos, neg, non_zero).
lub(pos, zero, non_neg).
lub(pos, non_pos, z).
lub(pos, non_zero, non_zero).
lub(pos, non_neg, non_neg).
lub(non_pos, neg, non_pos).
lub(non_pos, zero, non_pos).
lub(non_pos, pos, z).
lub(non_pos, non_zero, z).
lub(non_pos, non_neg, z).
lub(non_zero, neg, non_zero).
lub(non_zero, pos, non_zero).
lub(non_zero, zero, z).
lub(non_zero, non_pos, z).
lub(non_zero, non_neg, z).
lub(non_neg, zero, non_neg).
lub(non_neg, pos, non_neg).
lub(non_neg, neg, z).
lub(non_neg, non_zero, z).
lub(non_neg, non_pos, z).

lub(tt, ff, t).
lub(tt, t, t).
lub(ff, tt, t).
lub(ff, t, t).
lub(t, tt, t).
lub(t, ff, t).

add_s(none, _, none).
add_s(_, none, none).
add_s(any, _, any).
add_s(_, any, any).
add_s(err, _, err).
add_s(_, err, err).
add_s(z, _, z).
add_s(_, z, z).

add_s(neg, neg, neg).
add_s(neg, pos, z).
add_s(neg, non_pos, neg).
add_s(neg, non_zero, z).
add_s(neg, non_neg, z).
add_s(zero, zero, zero).
add_s(zero, X, X).
add_s(X, zero, X).
add_s(pos, neg, z).
add_s(pos, pos, pos).
add_s(pos, non_pos, z).
add_s(pos, non_zero, z).
add_s(pos, non_neg, pos).
add_s(non_pos, neg, neg).
add_s(non_pos, pos, z).
add_s(non_pos, non_zero, z).
add_s(non_pos, non_neg, z).
add_s(non_zero, neg, z).
add_s(non_zero, pos, z).
add_s(non_zero, non_pos, z).
add_s(non_zero, non_neg, z).
add_s(non_neg, neg, z).
add_s(non_neg, pos, pos).
add_s(non_neg, non_pos, z).
add_s(non_neg, non_zero, z).

sub_s(none, _, none).
sub_s(_, none, none).
sub_s(any, _, any).
sub_s(_, any, any).
sub_s(err, _, err).
sub_s(_, err, err).
sub_s(z, _, z).
sub_s(_, z, z).
sub_s(neg, neg, z).
sub_s(neg, zero, neg).
sub_s(neg, pos, neg).
sub_s(neg, non_pos, z).
sub_s(neg, non_zero, z).
sub_s(neg, non_neg, neg).

sub_s(zero, neg, pos).
sub_s(zero, zero, zero).
sub_s(zero, pos, neg).
sub_s(zero, non_pos, non_neg).
sub_s(zero, non_zero, non_zero).
sub_s(zero, non_neg, non_pos).

sub_s(pos, neg, pos).
sub_s(pos, zero, pos).
sub_s(pos, pos, z).
sub_s(pos, non_pos, pos).
sub_s(pos, non_zero, z).
sub_s(pos, non_neg, z).

sub_s(non_pos, neg, z).
sub_s(non_pos, zero, non_pos).
sub_s(non_pos, pos, neg).
sub_s(non_pos, non_pos, z).
sub_s(non_pos, non_zero, z).
sub_s(non_pos, non_neg, non_pos).

sub_s(non_zero, neg, z).
sub_s(non_zero, zero, non_zero).
sub_s(non_zero, pos, z).
sub_s(non_zero, non_pos, z).
sub_s(non_zero, non_zero, z).
sub_s(non_zero, non_neg, z).

sub_s(non_neg, neg, pos).
sub_s(non_neg, zero, non_neg).
sub_s(non_neg, pos, z).
sub_s(non_neg, non_pos, non_neg).
sub_s(non_neg, non_zero, z).
sub_s(non_neg, non_neg, z).

mul_s(none, _, none).
mul_s(_, none, none).
mul_s(any, _, any).
mul_s(_, any, any).
mul_s(err, _, err).
mul_s(_, err, err).
mul_s(zero, _, zero).
mul_s(_, zero, zero).
mul_s(z, _, z).
mul_s(_, z, z).

mul_s(neg, neg, pos).
mul_s(neg, pos, neg).
mul_s(neg, non_pos, non_neg).
mul_s(neg, non_zero, non_zero).
mul_s(neg, non_neg, non_pos).

mul_s(pos, neg, neg).
mul_s(pos, pos, pos).
mul_s(pos, non_pos, non_pos).
mul_s(pos, non_zero, non_zero).
mul_s(pos, non_neg, non_neg).

mul_s(neg, neg, pos).
mul_s(pos, neg, neg).
mul_s(non_pos, neg, non_neg).
mul_s(non_zero, neg, non_zero).
mul_s(non_neg, neg, non_pos).

mul_s(neg, pos, neg).
mul_s(pos, pos, pos).
mul_s(non_pos, pos, non_pos).
mul_s(non_zero, pos, non_zero).
mul_s(non_neg, pos, non_neg).

mul_s(non_pos, non_pos, non_neg).
mul_s(non_pos, non_zero, z).
mul_s(non_pos, non_neg, non_pos).
mul_s(non_zero, non_pos, z).
mul_s(non_zero, non_zero, non_zero).
mul_s(non_zero, non_neg, z).
mul_s(non_neg, non_pos, non_pos).
mul_s(non_neg, non_zero, z).
mul_s(non_neg, non_neg, non_neg).

div_s(none, _, none).
div_s(_, none, none).
div_s(any, _, any).
div_s(_, any, any).
div_s(err, _, err).
div_s(_, err, err).
div_s(_, zero, err).
div_s(any, _, any).
div_s(_, any, any).

div_s(neg, neg, pos).
div_s(neg, pos, neg).
div_s(neg, non_pos, any).
div_s(neg, non_zero, non_zero).
div_s(neg, non_neg, any).

div_s(zero, neg, zero).
div_s(zero, pos, zero).
div_s(zero, non_pos, any).
div_s(zero, non_zero, zero).
div_s(zero, non_neg, any).

div_s(pos, neg, neg).
div_s(pos, pos, pos).
div_s(pos, non_pos, any).
div_s(pos, non_zero, non_zero).
div_s(pos, non_neg, any).

div_s(non_pos, neg, non_neg).
div_s(non_pos, pos, non_pos).
div_s(non_pos, non_pos, any).
div_s(non_pos, non_zero, z).
div_s(non_pos, non_neg, any).

div_s(non_zero, neg, non_zero).
div_s(non_zero, pos, non_zero).
div_s(non_zero, non_pos, any).
div_s(non_zero, non_zero, non_zero).
div_s(non_zero, non_neg, any).

div_s(non_neg, neg, non_pos).
div_s(non_neg, pos, non_neg).
div_s(non_neg, non_pos, z).
div_s(non_neg, non_zero, any).
div_s(non_neg, non_neg, any).

eq_s(none, _, none).
eq_s(_, none, none).
eq_s(any, _, any).
eq_s(_, any, any).
eq_s(err, _, err).
eq_s(_, err, err).
eq_s(z, _, t).
eq_s(_, z, t).

eq_s(neg, neg, t).
eq_s(neg, zero, ff).
eq_s(neg, pos, ff).
eq_s(neg, non_pos, t).
eq_s(neg, non_zero, t).
eq_s(neg, non_neg, ff).

eq_s(zero, neg, ff).
eq_s(zero, zero, tt).
eq_s(zero, pos, ff).
eq_s(zero, non_pos, t).
eq_s(zero, non_zero, ff).
eq_s(zero, non_neg, t).

eq_s(pos, neg, ff).
eq_s(pos, pos, t).
eq_s(pos, zero, ff).
eq_s(pos, non_pos, ff).
eq_s(pos, non_zero, t).
eq_s(pos, non_neg, t).

eq_s(neg, neg, t).
eq_s(zero, neg, ff).
eq_s(pos, neg, ff).
eq_s(non_pos, neg, t).
eq_s(non_zero, neg, t).
eq_s(non_neg, neg, ff).

eq_s(neg, zero, ff).
eq_s(zero, zero, tt).
eq_s(pos, zero, ff).
eq_s(non_pos, zero, t).
eq_s(non_zero, zero, ff).
eq_s(non_neg, zero, t).

eq_s(neg, pos, ff).
eq_s(pos, pos, t).
eq_s(zero, pos, ff).
eq_s(non_pos, pos, ff).
eq_s(non_zero, pos, t).
eq_s(non_neg, pos, t).

eq_s(non_pos, non_pos, t).
eq_s(non_pos, non_zero, t).
eq_s(non_pos, non_neg, t).
eq_s(non_zero, non_pos, t).
eq_s(non_zero, non_zero, t).
eq_s(non_zero, non_neg, t).
eq_s(non_neg, non_pos, t).
eq_s(non_neg, non_zero, t).
eq_s(non_neg, non_neg, t).

leq_s(none, _, none).
leq_s(_, none, none).
leq_s(any, _, any).
leq_s(_, any, any).
leq_s(err, _, err).
leq_s(_, err, err).
leq_s(z, _, t).
leq_s(_, z, t).

leq_s(neg, neg, t).
leq_s(neg, zero, tt).
leq_s(neg, pos, tt).
leq_s(neg, non_pos, t).
leq_s(neg, non_zero, t).
leq_s(neg, non_neg, tt).

leq_s(zero, neg, ff).
leq_s(zero, zero, tt).
leq_s(zero, pos, tt).
leq_s(zero, non_pos, t).
leq_s(zero, non_zero, t).
leq_s(zero, non_neg, tt).

leq_s(pos, neg, ff).
leq_s(pos, pos, ff).
leq_s(pos, zero, t).
leq_s(pos, non_pos, ff).
leq_s(pos, non_zero, t).
leq_s(pos, non_neg, t).

leq_s(neg, neg, t).
leq_s(zero, neg, tt).
leq_s(pos, neg, tt).
leq_s(non_pos, neg, t).
leq_s(non_zero, neg, t).
leq_s(non_neg, neg, tt).
leq_s(neg, zero, ff).
leq_s(zero, zero, tt).
leq_s(pos, zero, tt).
leq_s(non_pos, zero, t).
leq_s(non_zero, zero, t).
leq_s(non_neg, zero, tt).
leq_s(neg, pos, ff).
leq_s(pos, pos, ff).
leq_s(zero, pos, t).
leq_s(non_pos, pos, ff).
leq_s(non_zero, pos, t).
leq_s(non_neg, pos, t).

leq_s(non_pos, non_pos, t).
leq_s(non_pos, non_zero, t).
leq_s(non_pos, non_neg, tt).
leq_s(non_zero, non_pos, t).
leq_s(non_zero, non_zero, t).
leq_s(non_zero, non_neg, t).
leq_s(non_neg, non_pos, t).
leq_s(non_neg, non_zero, t).
leq_s(non_neg, non_neg, t).

not_s(none, none).
not_s(any, any).
not_s(t, t).
not_s(tt, ff).
not_s(ff, tt).

and_s(none, _, none).
and_s(_, none, none).
and_s(any, _, any).
and_s(_, any, any).
and_s(err, _, err).
and_s(_, err, err).
and_s(tt, tt, tt).
and_s(tt, ff, ff).
and_s(tt,  t, t).
and_s(ff, _, ff).
and_s(t, tt, t).
and_s(t, ff, ff).
and_s(t, t, t).

evl([[puch(X)|C], E, S, norm, I],        [C,[X|E], S, norm, I]).


evl([[add|C], [err,_|E], S, norm, I],    [C,[err|E],S, norm, I]).
evl([[add|C], [_,err|E], S, norm, I],    [C,[err|E],S, norm, I]).
evl([[add|C], [Z1,Z2|E], S, norm, I],    [C,[Sum|E],S, norm, I]):-
    Sum is Z1+Z2.

evl([[mult|C], [err,_|E], S, norm, I],   [C,[err|E],S, norm, I]).
evl([[mult|C], [_,err|E], S, norm, I],   [C,[err|E],S, norm, I]).
evl([[mult|C], [Z1,Z2|E], S, norm, I],   [C,[Sum|E],S, norm, I]):-
    Sum is Z1*Z2.

evl([[subt|C], [_,err|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[subt|C], [err,_|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[subt|C], [Z1,Z2|E], S,norm, I],   [C,[Sum|E],S,norm, I]):-
    Sum is Z1-Z2.

evl([[div|C], [_,0|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[div|C], [_,err|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[div|C], [err,_|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[div|C], [Z1,Z2|E], S,norm, I],   [C,[Res|E],S,norm, I]):-
    Res is div(Z1,Z2).

evl([[true|C], E, S, norm, I],           [C,[tt|E],S, norm, I]).

evl([[false|C], E, S, norm, I],          [C,[ff|E],S, norm, I]).


evl([[eq|C], [err, _| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[eq|C], [_, err| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[eq|C], [Z, Z| E] , S, norm, I],  [C,[tt|E],S, norm, I]).
evl([[eq|C], [_, _| E] , S, norm, I],  [C,[ff|E],S, norm, I]).


evl([[le|C], [err, _| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[le|C], [_, err| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[le|C], [Z1, Z2| E] , S, norm, I],  [C,[tt|E],S, norm, I]):-
    Z1=<Z2.
evl([[le|C], [_, _| E] , S, norm, I],  [C,[ff|E],S, norm, I]).

evl([[and|C], [err, _| E] , S, norm, I], [C,[err|E],S, norm, I]).
evl([[and|C], [_, err| E] , S, norm, I], [C,[err|E],S, norm, I]).
evl([[and|C], [tt, tt| E] , S, norm, I], [C,[tt|E],S, norm, I]).
evl([[and|C], [ff, tt| E] , S, norm, I], [C,[ff|E],S, norm, I]).
evl([[and|C], [tt, ff| E] , S, norm, I], [C,[ff|E],S, norm, I]).

evl([[neg|C], [err| E] , S, norm, I],     [C,[err|E],S, norm, I]).
evl([[neg|C], [tt| E] , S, norm, I],     [C,[ff|E],S, norm, I]).
evl([[neg|C], [ff| E] , S, norm, I],     [C,[tt|E],S, norm, I]).

evl([[fetch(X)|C], E , S, norm, I],      [C,[RES|E],SNext, norm, I]):-
    (
        get_assoc(X, S, RES);
        format('error: fetching undefined variable ~w !~n', [X]), halt
    ), SNext=S.

evl([[store(_)|C], [err|E], S, norm, _], [C, E, S, abnorm, 0]). 
evl([[store(X)|C], [Z|E] , S, norm, I],  [C,E,SNext, norm, I]):-
    number(Z), 
    put_assoc(X, S, Z, SNext).

evl([[noop|C], E , S, norm, I],    [C,E,S, norm, I]).

evl([[branch(_, _)|C], [err|E], S, norm, I],     [C, E, S, abnorm, I]).
evl([[branch(C1, C2)|C], [T|E] , S, norm, I],   [Cres,E,S, norm, I]):-
    T=tt, append([C1, C], Cres);
    T=ff, append([C2, C], Cres).

evl([[loop(C1, C2)|C], E , S, norm, I],  [ResCode,E,S, norm, I]):-
    append([C2, [loop(C1, C2)]], BranchBody),
    append([C1, [branch(BranchBody, [noop])], C], ResCode).

evl([[try|C], E, S, abnorm, I], [C, E, S, abnorm, In]):-
    In is I+1.
evl([[try|C], E , S, norm, I],    [C,E,S, norm, I]).

evl([[catch(X)|C], E, S, abnorm, 0], [ResCode, E, S, norm, 0]):-
    append([X, C], ResCode).

evl([[catch(_)|C], E, S, abnorm, I], [C, E, S, abnorm, In]):-
    In is I-1.

evl([[catch(_)|C], E , S, norm, I],    [C,E,S, norm, I]).
evl([[W|C], E, S, abnorm, I], [C, E, S, abnorm, I]):-
    \+ W=catch(_),
    \+ W=try.



run([[],E, S, N, I], [[], E, S, N, I], _).

run([C, E, S, N, I], Result, Debug):-
    (
        Debug, 
        assoc_to_list(S, State), 
        (   
            N=norm,format('State',[]);
            N=abnorm, format('Abnormal State')
        ),
        format(': ~w ~nStack: ~w~n~n', [State, E]) , 
        format('Executing: ~w ~n', C),
        get_single_char(_);
        true
    ),
    evl([C, E, S, N, I], NextConf),
    run(NextConf, Result, Debug).

run(Code, InitState, FinalState, Debug):-
    list_to_assoc(InitState,S),
    run([Code, [], S, norm, 0], [[], _,FS,_,_], Debug),
    assoc_to_list(FS, FinalState).
