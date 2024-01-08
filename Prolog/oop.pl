%%%% -*- Mode Prolog -*-
%%%% oop.pl

%%%% Cantaluppi Camilla 894557
%%%% Carano Antonio 902447

:-
	dynamic(is_class/1),
	dynamic(c_creation_components/3),
	dynamic(field_in_class/2),
	dynamic(field_in_instance/2),
	dynamic(father_class_of/2),
	dynamic(instance_of/2),
	dynamic(method_in_class/2),
	dynamic(method_in_instance/2).

def_class(ClassName, _, _) :-
	is_class(ClassName), !,
	fail.

def_class(ClassName, Parents, Parts) :-
	is_a_set(Parents),
	is_a_set(Parts),
	are_parts(ClassName, Parts),
	are_parents(ClassName, Parents),
	assertz(is_class(ClassName)),
	assertz(c_creation_components(ClassName, Parents, Parts)), !.

is_a_set([]).

is_a_set([(A,B)|Rest]) :-
	list_member((A,B),Rest), !, fail.

is_a_set([(_,_)|Rest]) :-
	is_a_set(Rest), !.

is_a_set([A|Rest]) :-
	list_member(A,Rest), !, fail.

is_a_set([_|Rest]) :-
	is_a_set(Rest), !.

are_parents(_, []).

are_parents(ClassName, [Parent | _]) :-
	is_class(Parent),
	is_superclass(ClassName, Parent), !, fail.

are_parents(ClassName, [Parent | Ps]) :-
	is_class(Parent),
	assertz(father_class_of(Parent, ClassName)),
	is_inheritor(ClassName, Parent),
	are_parents(ClassName, Ps), !.

is_inheritor(ClassName, P) :-
	findall(field(FieldName, FieldValue, Type), field_in_class(field(FieldName, FieldValue, Type), P), Fields),
	inherit_fields_from_supc(ClassName, Fields),
	findall(method(MethodName, Args, RawBody), method_in_class(method(MethodName, Args, (RawBody)), P), ML),
	inherit_methods_from_supc(ClassName, ML).

inherit_methods_from_supc(_, []).

inherit_methods_from_supc(ClassName, [method(MethodName, _, _) | Ms]) :-
	method_in_class(method(MethodName, _, _), ClassName),
	inherit_methods_from_supc(ClassName, Ms), !.

inherit_methods_from_supc(ClassName, [method(MN, Args, Body) | Ms]) :-
	assertz(method_in_class(method(MN, Args, (Body)), ClassName)),
	inherit_methods_from_supc(ClassName, Ms).

inherit_fields_from_supc(_, []) :- !.

% QUI CONTROLLA CHE IL TIPO NON SIA PIU' "AMPIO" DI QUELLO DELLA SUPERCLASSE
inherit_fields_from_supc(ClassName, [field(FN, _, TypeSp) | Fields]) :-
	field_in_class(field(FN, _, TypeCN), ClassName), !,			
	check_type(TypeSp, TypeCN),
	inherit_fields_from_supc(ClassName, Fields), !.

% se inherit ecc non va a buon fine bisogna fallire la creazione della classe e
% quindi fare un retractall di tutte le cose che sono state asserite per
% la creazione della classe


inherit_fields_from_supc(ClassName, [field(FieldName, FieldValue, Type) | Fields]) :-
	assertz(field_in_class(field(FieldName, FieldValue, Type), ClassName)),
	inherit_fields_from_supc(ClassName, Fields), !.

% GESTIONE TYPE
check_type(float, integer).

check_type(numeric, float).

check_type(numeric, integer).

check_type(X, X).


% check_type(FieldSuperClass, FieldCurrentClass).
check_type(TypeSp, TypeCN) :-
	is_class(TypeSp),
	is_class(TypeCN),
	is_superclass(TypeCN, TypeSp).

% INTEGER, REAL, STRING. LIST?

is_type(X, integer) :-
	integer(X).

is_type(X, float) :-
	float(X).

is_type(X, number) :-
	number(X).

is_type(X, string) :-
	string(X).	

is_type(X, atom) :-
	atom(X).

is_type(X, compound) :-
	compound(X).

is_type(X, DefinedClass) :-
	inst(X, instance(_, DefinedClass, _)).

% make(IN, CN, Slots)
% cosa passare se l'istanza non esiste ancora?
%is_type(, DefinedClass) :-
%	is_instance(X, DefinedClass).

are_parts(_, []) :- !.

are_parts(ClassName, [method(MN, ArgL, Form) | Parts]) :-
	is_method_term(ClassName, method(MN, ArgL, Form)),
	are_parts(ClassName, Parts), !.

are_parts(ClassName, [field(FN, FV, Type) | Parts]) :-
	is_type(FV, Type),
	asserta(field_in_class(field(FN, FV, Type), ClassName)),	
	are_parts(ClassName, Parts), !.

are_parts(ClassName, [field(FN, FV) | Parts]) :-
	asserta(field_in_class(field(FN, FV, _), ClassName)),	
	are_parts(ClassName, Parts), !.

is_instance(instance(Instance, ClassName, _)) :-
	instance_of(Instance, ClassName).

is_instance(instance(Instance, ClassName, Attrs), SCN) :-
	instance_of(Instance, ClassName),
	is_superclass(SCN, ClassName),
	findall(Term, field_in_instance(Term, Instance), Attrs).

inst(Instance, instance(Instance, ClassName, Attrs)) :-
	instance_of(Instance, ClassName),
	findall(Term, field_in_instance(Term, Instance), Attrs).

% is_superclass(SuperClasse, Sottoclasse).
is_superclass(SuperClass, SubClass) :-
	father_class_of(X, SubClass),
	is_superclass(SuperClass, X).

is_superclass(SuperClass, SubClass) :-
	father_class_of(SuperClass, SubClass), !.

is_method_term(ClassName, method(MN, Args, RawBody)) :-
	is_list(Args),
	asserta(method_in_class(method(MN, Args, RawBody), ClassName)).

execute_method(MN, Args, Instance) :-
	inst(IN, Instance),
	execute_method(MN, Args, IN), !.

execute_method(MN, Args, IN) :-
	method_in_instance(method(MN, Args, Body), IN),
	call(Body), !.

replace((Term, OtherTerms), Keyword, Rep, (FirstTermReplaced, OtherTermsReplaced)) :-
	replace(Term, Keyword, Rep, FirstTermReplaced),
	replace(OtherTerms, Keyword, Rep, OtherTermsReplaced), !.

replace(Term, Keyword, Rep, TermReplaced) :-
    Term =.. [F | Args],
	replace_args(Args, Keyword, Rep, ReplacedArgs), !,
    TermReplaced =.. [F | ReplacedArgs].

replace_args([], _, _, []).

replace_args([Arg | Rest], Keyword, Rep, [Rep | ReplacedRest]) :-
	Arg == Keyword, !,
	replace_args(Rest, Keyword, Rep, ReplacedRest).

replace_args([Arg | Rest], Keyword, Rep, [Arg | ReplacedRest]) :-
	var(Arg), !,
	replace_args(Rest, Keyword, Rep, ReplacedRest).

replace_args([Arg | Rest], Keyword, Rep, [Arg | ReplacedRest]) :-
	Arg =.. [_ | []], !,
	replace_args(Rest, Keyword, Rep, ReplacedRest).

replace_args([Term | []], Keyword, Rep, [TermReplaced]) :- !,
	replace(Term, Keyword, Rep, TermReplaced).

replace_args([Term | OtherTerms], Keyword, Rep, [TermReplaced | OtherTermsReplaced]) :- !,
	replace(Term, Keyword, Rep, TermReplaced),
	replace_args(OtherTerms, Keyword, Rep, OtherTermsReplaced).

make(Instance, ClassName) :-
	make(Instance, ClassName, []).

make(X, ClassName, Parts) :-
	var(X), !,
	nonvar(ClassName),
	nonvar(Parts),
	make(oopinst, ClassName, Parts),
	inst(oopinst, X).

make(Instance, ClassName, Parts) :-
	instance_of(Instance, _), !,
	retractall(instance_of(Instance, _)),
	retractall(field_in_instance(_, Instance)),
	retractall(method_in_instance(_, Instance)),
	make(Instance, ClassName, Parts).

make(Instance, ClassName, Parts) :-
	is_class(ClassName),
	are_parts_in_instance(Instance, ClassName, Parts),
	assertz(instance_of(Instance, ClassName)), !.

are_parts_in_instance(Instance, CN, Parts) :-
	is_a_set(Parts),
	findall(field(FN, FV, Type), field_in_class(field(FN, FV, Type), CN), FFC),
	findall(method(MN, Args, Body), method_in_class(method(MN, Args, Body), CN), MFC),
	are_parts_in_instance(Instance, FFC, MFC, Parts).

are_parts_in_instance(_, [], [], []) :- !.

are_parts_in_instance(Instance, [field(FN,_,Type)|OtherPartsFC], MethodsFC, PartsToOverride) :-
	list_member(FN=NFV, PartsToOverride),
	is_type(NFV, Type),
	asserta(field_in_instance(field(FN, NFV, Type), Instance)),
	list_without_el(FN=NFV, PartsToOverride, OtherPartsToOverride),
	are_parts_in_instance(Instance, OtherPartsFC, MethodsFC, OtherPartsToOverride), !.

are_parts_in_instance(Instance, [field(FN, FV, Type)|OtherPartsFC], MethodsFC, PartsToOverride) :-
	asserta(field_in_instance(field(FN, FV, Type), Instance)),
	are_parts_in_instance(Instance, OtherPartsFC, MethodsFC, PartsToOverride), !.

are_parts_in_instance(Instance, PartsFC, [method(MN,Args,Body)|OtherParts], PartsToOverride) :-
	asserta(field_in_instance(method(MN, Args, Body), Instance)),
	is_instance_method(Instance, method(MN, Args, Body)),
	are_parts_in_instance(Instance, PartsFC, OtherParts, PartsToOverride), !.

is_instance_method(IN, method(MN,Args,RawBody)) :-
	is_list(Args),
	method_in_instance(method(MN,_,_), IN), !,
	retractall(method_in_instance(method(MN, _, _), IN)),
	replace(RawBody, this, IN, Body),
	asserta(method_in_instance(method(MN, Args, Body), IN)).

is_instance_method(IN, method(MN,Args,RawBody)) :-
	is_list(Args),
	replace(RawBody, this, IN, Body),
	MSig =.. [MN, Instance | Args],
	asserta(method_in_instance(method(MN, Args, Body), IN)),
	asserta((MSig :- (MSig) =.. [MN, Instance | Args], execute_method(MN, Args, Instance), !)), !.

list_member(X, [X]) :- !.

list_member(X, [X|_]) :- !.

list_member(X, [_|Xs]) :-
	list_member(X, Xs), !.

field(Instance, FN, FV) :-
	inst(IN, Instance), !,
	field_in_instance(field(FN, FV, _), IN).

field(IN, FN, FV) :-
	field_in_instance(field(FN, FV, _), IN), !.

fieldx(IN, [FN|FNs], FV) :-
	field(IN, FN, NIN),
	fieldx(NIN, FNs, FV), !.

fieldx(IN, [FN], FV) :-
	field(IN, FN, FV), !.

list_without_el(_, [], []).

list_without_el(A, [A|C], E) :-
	list_without_el(A, C, E), !.

list_without_el(A, [B|C], [B|E]) :-
	list_without_el(A, C, E), !.

my_maplist(_,[],[],[]).

my_maplist(P,[A|As],[B|Bs],[C|Cs]) :-
	call(P,A,B,C),
	my_maplist(P,As,Bs,Cs).

explode_parts([], []).

explode_parts([Part|Parts], [(F,N)|Ls]):-
	Part =.. [F,N|_],
	explode_parts(Parts, Ls).

%%%% end of file -- oop.pl
