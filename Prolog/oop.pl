

%%%% Cantaluppi Camilla 894557
%%%% Carano Antonio 902447

% def_class(persona, [], [field(nome, 'anto'), method(talk, [], write("My name is"))]).
% def_class(studente, [persona], [field(matricola, 1234, integer)]).
% def_class(bambino, [studente], [method(piange, [],
% write("UUUUEEEE"))]).

:-
    dynamic(def_class/3),
    dynamic(def_class/2),
    dynamic(field/2),
    dynamic(field/3),
    dynamic(method/3),
    dynamic(part/2),
    dynamic(make/2),
    dynamic(is_class/1),
    dynamic(make/3),
    dynamic(is_instance/2),
    dynamic(is_instance/3),
    dynamic(instance/3),
    dynamic(class/3),
    dynamic(superclass/2),
    dynamic(is_father/2).


def_class(ClassName, Parents, Parts) :-
    is_class(ClassName), !,
    writeln("Classe gia' esistente"),fail.

def_class(ClassName, Parents, Parts) :-
    are_parents(ClassName, Ancestors),
    escludi(Parents, Ancestors, Relatives),
    append(Parents, Relatives, AllParents),
    assertz(class(ClassName, AllParents, Parts)).

def_class(ClassName, Parents) :-
    is_class(ClassName), !,
    writeln("Classe gia' esistente"),fail.

def_class(ClassName, Parents) :-
    def_class(ClassName, Parents, []).

field(FieldName, Value) :-
    Field =.. [FieldName, Value],
    assertz(Field).

field(FieldName, Value, Type) :-
    Field =.. [FieldName, Value, Type],
    assertz(Field).

method(MethodName, Arglist, Form) :-
    Method =.. [MethodName, Arglist, Form],
    assertz(Method).

part(Field, Method) :-
    Part =.. [Field | Method],
    assertz(Part).

make(InstanceName, ClassName) :-
    make(InstanceName, ClassName, []).

is_class(ClassName) :-
    class(ClassName, _, _).

is_instance(InstanceName) :-
    instance(InstanceName, _, _).

is_instance(InstanceName, ClassName) :-
    instance(InstanceName, ClassName, _).

% mi restituisce una lista di soli metodi
find_methods([], []).

find_methods([method(MethodName, Args, Body) | Rest], [method(MethodName, Args, Body) | Tail]) :-
    find_methods(Rest, Tail).

find_methods([field(_, _) | Rest], MethList) :-
    find_methods(Rest, MethList).

find_methods([field(_, _, _) | Rest], MethList) :-
    find_methods(Rest, MethList).

% mi restituisce una lista di soli campi field)
find_fields([], []).

find_fields([field(FieldName, Value) | Rest], [field(FieldName, Value) | Tail]) :-
    find_fields(Rest, Tail).

find_fields([field(FieldName, Value, Type) | Rest], [field(FieldName, Value, Type) | Tail]) :-
    find_fields(Rest, Tail).

find_fields([method(_, _, _) | Rest], FieldList) :-
    find_fields(Rest, FieldList).



% trova genitori e antenati
are_parents(Child, AllParents) :-
    are_parents_finder(Child, [], AllParents).

%DA FINIRE
are_parents_finder(Child, Visited, AllParents) :-
    class(Child, ParentsList, _),
    escludi(Visited, ParentsList, NewParents),
    append(Visited, NewParents, UpdatedVisited),
    find_all_ancestors(NewParents, UpdatedVisited, AncestorParents),
    append(NewParents, AncestorParents, AllParents), !;
    AllParents = Visited.

% Trova tutti gli antenati per una lista di genitori
find_all_ancestors([], _, []).

find_all_ancestors([Parent|Rest], Visited, Ancestors) :-
    are_parents_finder(Parent, Visited, ParentAncestors),
    find_all_ancestors(Rest, Visited, RestAncestors),
    append(ParentAncestors, RestAncestors, Ancestors), !.



% escludi(ElementiDaRimuovere, ListaOriginale, ListaRisultante). toglie
% tutti gli elementi di una lista da un'altra, e restituisce una lista
% senza quegli elementi.

escludi(_, [], []).

escludi(ElementiDaRimuovere, [Testa | Coda], [Testa | NewList]) :-
    \+ member(Testa, ElementiDaRimuovere),
    escludi(ElementiDaRimuovere, Coda, NewList), !.

escludi(ElementiDaRimuovere, [Testa | Coda], NewList) :-
    member(Testa, ElementiDaRimuovere),
    escludi(ElementiDaRimuovere, Coda, NewList), !.



%%superclass(SuperClass, Class) :-
%%    is_class(SuperClass),
%%    is_class(Class),
%%    are_fathers(X, Class),
%%    same_class(SuperClass, X).
%%
   %%same_class(X, X).


%%%%%%%%%%%%%%%%%%%%%% CHAT GPT
%%% Questa e' la funzione principale da chiamare
%%are_fathers(Child, AllParents) :-
%%    are_fathers_group(Child, [], AllParents).
%%
%%% pregicato per raggruppare tutti i genitori e antenati,
%%% evitando cicli e duplicati
%%are_fathers_group(Child, Visited, AllParents) :-
%%     class(Child, ParentsList, _),
%%     exclude(member(Visited), ParentsList, NewParents),
%%     append(Visited, NewParents, UpdatedVisited),
%%     find_all_ancestors(NewParents, UpdatedVisited, AncestorParents),
%%     append(NewParents, AncestorParents, AllParents), !;
%%     AllParents = Visited.
%%
%%% Trova tutti gli antenati per una lista di genitori
%%find_all_ancestors([], _, []).
%%find_all_ancestors([Parent|Rest], Visited, Ancestors) :-
%%    are_fathers_group(Parent, Visited, ParentAncestors),
%%    find_all_ancestors(Rest, Visited, RestAncestors),
%%    append(ParentAncestors, RestAncestors, Ancestors), !.

%sub_set([FirstL], SecondL) :-











