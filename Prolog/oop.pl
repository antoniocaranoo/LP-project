

%%%% Cantaluppi Camilla 894557
%%%% Carano Antonio 902447

% def_class(persona, [], [field(nome, 'anto'), method(talk, [], write("My
% name is"))]).
%def_class(studente, [persona], [field(matricola, 1234, integer)]).

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

def_class(ClassName, Parents) :-
    is_class(ClassName), !,
    writeln("Classe gia' esistente"),fail.


def_class(ClassName, Parents, Parts) :-
    assertz(class(ClassName, Parents, Parts)).

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

superclass(SuperClass, Class) :-
    is_class(SuperClass),
    is_class(Class),
    is_father(Class, X),
    same_class(SuperClass, X).

same_class(X, X).

is_father(Class, _) :-
    class(Class, [], _), !.

is_father(Class, Parent) :-
    class(Class, [Parent | Rest], _),
    is_father(Class, Rest).


