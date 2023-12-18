%%%% Cantaluppi Camilla 894557
%%%% Carano Antonio 902447
:-
    dynamic(class\3),
    dynamic(parts_in_class/3),.

% is_class verifica che la classe CN sia già esistente e blocca il 
% backtracking
% retractall Rimuove le informazioni     
% esistenti relative alla classe da reinserire.

def_class(ClassName, Parents) :-
    is_class(ClassName), !, -
    retractall(parts_in_class(_, _, ClassName)), 
    retractall(father_class_of(_, ClassName)),  
    is_a_set(Parents),
    is_a_set(Parts),
    are_slot_terms(CN, SVs),
    are_parents(CN, Ps),
    findall(Child, father_class_of(CN, Child), ChildrenCs),
    my_maplist(c_creation_components, ChildrenCs, ChildrenParents, ChildrenBodies),
    my_maplist(def_class, ChildrenCs, ChildrenParents, ChildrenBodies), !.
    assert(class(ClassName, Parents, [])).

def_class(ClassName, Parents, Parts) :-
    is_class(ClassName), !,
    assert(class(ClassName, Parents, Parts)).

field(FieldName, Value) :-
    Field =.. [FieldName, Value],
    assert(Field).

field(FieldName, Value, Type) :-
    Field =.. [FieldName, Value, Type],
    assert(Field).


method(MethodName, Arglist, Form) :-
    Method =.. [MethodName, Arglist, Form],
    assert(Method).

part(Field, Method) :-
    Part =.. [Field | Method],
    assert(Part).

%%%%%%%%%%%%%%%%%%%%%%% fino a qui tutto giusto (spero)
make(InstanceName, ClassName) :-
    is_instance(InstanceName, ClassName),
    .

_____________________% 2. Creazione di un'Istanza (make)
% make(InstanceName, ClassName, Fields): Crea un'istanza di una classe
% con i campi specificati.
make(InstanceName, ClassName, Fields) :-
    class(ClassName, Parents, Parts),
    assert(instance(InstanceName, ClassName, Parents, Parts, Fields)).

% make/2 � una versione semplificata di make/3 con una lista di campi vuota.
make(Instance, Class) :-
    make(Instance, Class, []).


___________________

% is_class(ClassName): Verifica se ClassName è il nome di una classe definita
is_class(ClassName) :-
    class(ClassName, _, _).

% is_instance(Instance): Verifica se Instance e' un'istanza di una
% qualsiasi classe.
is_instance(Value) :-
    instance(Value, _, _).

% is_instance(Instance, ClassName): Verifica se Instance e' un'istanza
% della classe ClassName.
is_instance(Value, ClassName) :-
    instance(Value, ClassName, _).
