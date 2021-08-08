:-dynamic subject/1. 
:-dynamic objectp/1.
:-dynamic accessRight/3.
:-dynamic deleteRight/3.
:-dynamic deleteSubject/1.
:-dynamic deleteObject/1.
:-dynamic entity/3, dynamic_access/3, owner/2.

:- module(add).

:- use_module(library(pldoc), []).
:- use_module(library(pengines)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(apply), [include/3,
                                exclude/3 as reject]).
:- use_module(library(apply), except([partition/4, partition/5])).

:- if(exists_source(library(http/http_dyn_workers))).
:- use_module(library(http/http_dyn_workers)).
:- else.
:- use_module(lib/plugin/http_dyn_workers, []).
:- endif.

attr(r). 
attr(w).
objectp(o3).
objectp(o5).
objectp(o7).
objectp(o9).
subject(s1).
subject(s2).
subject(s4).
subject(s6).
subject(s8).
accessRight(s2,o3,w).
accessRight(s2,o5,r).
accessRight(s2,s4,r).
accessRight(s4,o5,w).
accessRight(s4,s2,w).
accessRight(s6,s1,w).
accessRight(s6,o7,w).
accessRight(s6,s8,r).
accessRight(s8,o7,r).
accessRight(s8,o9,r).
accessRight(s8,o9,w).
accessRight(s8,s6,w).
accessRight(s1,s4,w).
accessRight(s4,s1,r).
accessRight(s1,s6,r).

insertRight(S,O,A):-
    deleteRight(S,O,A), 
    subject(S),
    S\==O,
    attr(A),
    assert(accessRight(S,O,A)),
    (objectp(O);subject(O)),
    retract(deleteRight(S,O,A)).

delRight(S,O,A):-
    S\==O,
    subject(S),
    (objectp(O);subject(O)),
    attr(A),
    accessRight(S,O,A),
    retract(accessRight(S,O,A)),
    assert(deleteRight(S,O,A)).
insertSubject(S):-
    deleteSubject(S),
    retract(deleteSubject(S)),
    assert(subject(S)).
removeSubject(S):-
    subject(S),
    retract(subject(S)),
    assert(deleteSubject(S)).
insertObject(O):-
    deleteObject(O),
    retract(deleteObject(O)),
    assert(objectp(O)).
removeObject(O):-
    objectp(O),
    assert(deleteObject(O)),
    retract(objectp(O)).
checkRight(S,O,A):-
    objectp(O),
    subject(S),
    attr(A),
    accessRight(S,O,A).
trojanAttack1(S,O,A):-
    S\==O,
    subject(S),
    (object(O);subject(O)),
    attr(A),
    assert(deleteRight(S,O,A)),
    accessRight(S,O,A),
    retract(accessRight(S,O,A)).
trojanAttack(S,O,A):-
    accessRight(S,X,A),
    (not(X=O);true),
    not(objectp(X)),
    (trojanAttack(X,O,A);true).


:-dynamic subject/3.
:-dynamic objectp/3.
:-dynamic accessRight/3.
attr(r).
attr(w).
type(admin).
type(family).
type(guest).
accessType(admin,admin).
accessType(admin,family).
accessType(admin,guest).
accessType(family,family).
accessType(family,guest).
accessType(guest,guest).
objectp(o3,guest,system).
objectp(o5,guest,system).
objectp(o7,family,system).
objectp(o9,family,system).
subjectMajor(system,admin).
subject(s1,admin,system).
subject(s2,guest,system).
subject(s4,guest,system).
subject(s6,family,system).
subject(s8,family,system).
accessRight(s2,o3,w).
accessRight(s2,o5,r).
accessRight(s2,s4,r).
accessRight(s4,o5,w).
accessRight(s4,s2,w).
accessRight(s6,s1,w).
accessRight(s6,o7,w).
accessRight(s6,s8,r).
accessRight(s8,o7,r).
accessRight(s1,s4,w).
accessRight(s4,s1,r).
accessRight(s8,o9,r).
accessRight(s8,o9,w).
accessRight(s8,s6,w).
accessRight(s1,s6,r).
insertRight(S,O,A):-
    S\==O,
    subject(S,TypeSubj,OwnSubj),
    (subject(OwnSubj,TypeOwn,_);subjectMajor(OwnSubj,_)),
    (objectp(O,TypeObj,_);subject(O,TypeObj,_)),
    attr(A),
    accessType(TypeSubj,TypeObj),
    assert(accessRight(S,O,A)).
delRight(S,O,A):-
    S\==O,
    checkRight(S,O,A),
    retract(accessRight(S,O,A)).
insertSubject(S,T):-
    not(subject(S,_,_)),
    type(T),
    assert(subject(S,T,system)).
removeSubject(S):-
    subject(S,_,_),
    retractall(subject(_,_,S)),
    retractall(objectp(_,_,S)),
    retractall(accessRight(S,_,_)),
    retractall(accessRight(_,S,_)),
    retract(subject(S,_,_)).
insertObject(O,T):-
    not(objectp(O,_,_)),
    type(T),
    assert(objectp(O,T,system)).
removeObject(O):-
    objectp(O,_,_),
    retractall(accessRight(_,O,_)),
    retract(objectp(O,_,_)).
checkRight(S,O,A):-
    (objectp(O,_,_);subject(O,_,_)),
    subject(S,_,_),
    attr(A),
    accessRight(S,O,A).
insertChildS(SC,T,SP):-
    not(subject(SC,_,_)),
    subject(SP,TypePar,_),
    type(T),
    assert(subject(SC,T,SP)),
    assert(accessRight(SC,SP,r)),
    assert(accessRight(SC,SP,w)),
    assert(accessRight(SP,SC,r)),
    assert(accessRight(SP,SC,w)).
insertChildO(O,T,SP):-
    not(objectp(O,_,_)),
    subject(SP,TypePar,_),
    type(T),
    accessType(TypePar,T),
    assert(objectp(O,T,SP)),
    assert(accessRight(SP,O,r)),
    assert(accessRight(SP,O,w)).


:-dynamic subject/3.
:-dynamic objectp/3.
:-dynamic accessRight/3.
attr(r).
attr(w).
type(admin).
type(family).
type(guest).
domain(lvl1).
domain(lvl2).
domain(lvl9).
dDT(lvl1,admin).
dDT(lvl1,family).
dDT(lvl1,guest).
dDT(lvl2,family).
dDT(lvl2,guest).
dDT(lvl9,guest).
dIT(lvl1,lvl1).
dIT(lvl1,lvl2).
dIT(lvl1,lvl9).
dIT(lvl2,lvl2).
dIT(lvl2,lvl9).
dIT(lvl9,lvl9).
objectp(o3,guest,system).
objectp(o5,guest,system).
objectp(o7,family,system).
objectp(o9,family,system).
subjectMajor(system,admin).
subject(s1,lvl1,system).
subject(s2,lvl9,system).
subject(s4,lvl9,system).
subject(s6,lvl2,system).
subject(s8,lvl2,system).
accessRight(s2,o3,w).
accessRight(s2,o5,r).
accessRight(s2,s4,r).
accessRight(s4,o5,w).
accessRight(s4,s2,w).
accessRight(s6,o7,w).
accessRight(s6,s8,r).
accessRight(s8,o7,r).
accessRight(s1,s4,w).
accessRight(s8,o9,r).
accessRight(s8,o9,w).
accessRight(s8,s6,w).
accessRight(s1,s6,r).
insertRight(S,O,A):-
    S\==O,
    subject(S,TypeSubj,OwnSubj),
    (subject(OwnSubj,TypeOwn,_);subjectMajor(OwnSubj,_)),
    (objectp(O,TypeObj,_);subject(O,TypeObj,_)),
    attr(A),
    (dDT(TypeSubj,TypeObj);dIT(TypeSubj,TypeObj)),
    assert(accessRight(S,O,A)).
delRight(S,O,A):-
    S\==O,
    checkRight(S,O,A),
    retract(accessRight(S,O,A)).
insertSubject(S):-
    not(subject(S,_,_)),
    assert(subject(S,_,system)).
insertSubjectWithDomain(S,D):-
    domain(D),
    not(subject(S,D,_)),
    assert(subject(S,D,system)).
removeSubject(S):-
    subject(S,_,_),
    retractall(subject(_,_,S)),
    retractall(objectp(_,_,S)),
    retractall(accessRight(S,_,_)),
    retractall(accessRight(_,S,_)),
    retract(subject(S,_,_)).
insertObject(O,T):-
    not(objectp(O,_,_)),
    type(T),
    assert(objectp(O,T,system)).
removeObject(O):-
    objectp(O,_,_),
    retractall(accessRight(_,O,_)),
    retract(objectp(O,_,_)).
checkRight(S,O,A):-
    (objectp(O,_,_);subject(O,_,_)),
    subject(S,_,_),
    attr(A),
    accessRight(S,O,A).
insertChildS(SC,D,SP):-
    not(subject(SC,_,_)),
    subject(SP,DPar,_),
    domain(D),
    dIT(DPar,D),
    assert(subject(SC,D,SP)),
    assert(accessRight(SC,SP,r)),
    assert(accessRight(SC,SP,w)),
    assert(accessRight(SP,SC,r)),
    assert(accessRight(SP,SC,w)).
insertChildO(O,T,SP):-
    not(objectp(O,_,_)),
    subject(SP,DPar,_),
    type(T),
    dDT(DPar,T),
    assert(objectp(O,T,SP)),
    assert(accessRight(SP,O,r)),
    assert(accessRight(SP,O,w)).
insertDomainS(S,D):-
    not(subject(S,Y,_)),
    subject(S,_,X),
    domain(D),
    assert(subject(S,D,X)).
delDomainS(S,D):-
    subject(S,D,X),
    retract(subject(S,D,_)),
    assert(subject(S,_,X)).

:-dynamic subject/1.
:-dynamic objectp/1.
:-dynamic accessRight/3.
attr(r).
attr(w).
attr(g).
attr(t).
objectp(o3).
objectp(o5).
objectp(o7).
objectp(o9).
subject(s1).
subject(s2).
subject(s4).
subject(s6).
subject(s8).
accessRight(s2,o3,w).
accessRight(s2,o5,w).
accessRight(s4,s2,r).
accessRight(s4,s2,t).
accessRight(s4,o5,r).
accessRight(s6,s1,r).
accessRight(s6,s1,t).
accessRight(s6,o7,w).
accessRight(s6,o7,r).
accessRight(s8,o7,r).
accessRight(s8,o9,w).
accessRight(s8,s6,w).
accessRight(s1,s4,w).
accessRight(s1,s4,g).
insertRight(S,O,A):-
    S\==O,
    subject(S),
    (objectp(O);subject(O)),
    attr(A),
    assert(accessRight(S,O,A)).
delRight(S,O,A):-
    S\==O,
    subject(S),
    (objectp(O);subject(O)),
    attr(A),
    accessRight(S,O,A),
    retract(accessRight(S,O,A)).
insertSubject(S):-
    not(subject(S)),
    assert(subject(S)).
removeSubject(S):-
    subject(S),
    retract(subject(S)),
    retractall(accessRight(S,_,_)),
    retractall(accessRight(_,S,_)).
insertObject(O):-
    not(objectp(O)),
    assert(objectp(O)),
    retractall(accessRight(_,O,_)).
removeObject(O):-
    objectp(O),
    retract(objectp(O)).
take(R,Sr,Ss,SO):-
    accessRight(Ss,SO,R),
    accessRight(Sr,Ss,t),
    assert(accessRight(Sr,SO,R)).
grant(R,Ss,Sr,SO):-
    accessRight(Ss,SO,R),
    accessRight(Ss,Sr,g),
    assert(accessRight(Sr,SO,R)).


:-dynamic entity/3.

entity(root, 4, 0).
entity(admin, 4, 0).
entity(family, 4, 2).
entity(anonym, 2, 3).
entity(app, 3, 4).
entity(file, 0, 4).
entity(printer, 2, 1).

owner(root, admin).
owner(root, family).
owner(admin, anonym).
owner(admin, printer).
owner(user, app).
owner(user, file).

dynamic_access(admin, family, 2).
dynamic_access(admin, anonym, 1).
dynamic_access(admin, app, 1).
dynamic_access(family, app, 1).
dynamic_access(family, printer, 1).
dynamic_access(app, printer, 1).

s_right(use, 1).
s_right(r, 2).
s_right(upd, 3).
s_right(cd, 4).

d_right(gr, 1).
d_right(da, 2).

create_entity(E):-
	not(entity(E, _, _)),
	assert(entity(E, _, _)).

create_child_entity(E1, E2):-
	entity(E1, A_r, P_r),
	not(entity(E2, _, _)),
	assert(entity(E2, A_r, P_r)),
	assert(owner(E1, E2)),
	assert(dynamic_access(E1, E2, 2)).

destroy_entity(E):-
	entity(E, _, _),
	retract(entity(E, _, _)),
	retract(owner(_, E)),
	retractall(dynamic_access(E,_,_)),
	retractall(dynamic_access(_,E,_)).

set_max_active(E, R):-
	s_right(R, New_priority),
	entity(E, A_r, P_r),
	retract(entity(E, A_r, P_r)),
	assert(entity(E, New_priority, P_r)).

set_max_passive(E, R):-
	s_right(R, New_priority),
	entity(E, A_r, P_r),
	retract(entity(E, A_r, P_r)),
	assert(entity(E, A_r, New_priority)).

check_right(E1, E2, R):-
	entity(E1, Max_a1, _),
	entity(E2, _, Max_p2),
	s_right(R, Priority),
	Max_a1 >= Priority,
	Max_p2 >= Priority.

grant(E1, E2, E3, R):-
	entity(E1, _, _),
        entity(E2, _, _),
	entity(E3, _, _),
	s_right(R, _),
	not(check_right(E2, E3, R)),
	check_right(E1, E3, R),
	(owner(E1, E2); dynamic_access(E1, E2, _)),
	set_max_active(E2, R).

revoke(E1, E2, E3, R):-
        entity(E1, _, _),
	entity(E2, _, _),
	entity(E3, _, _),
	s_right(R, Priority),
	New_priority is Priority-1,
	not(entity(E2, New_priority, _)),
	check_right(E1, E3, R),
	(owner(E1, E3); dynamic_access(E1, E3, 1)),
	(owner(E1, E2); dynamic_access(E1, E2, 1)),
	s_right(New_R, New_priority),
	set_max_active(E2, New_R).

delegate(E1, E2, E3, R):-
	entity(E1, _, _),
	entity(E2, _, _),
	entity(E3, _, _),
	d_right(R, Priority),
	Priority = 1,
	not(dynamic_access(E2, E3, Priority); owner(E2, E3)),
	(dynamic_access(E1, E3, Priority);owner(E1, E2)),
	assert(dynamic_access(E2, E3, Priority)).

abrogate(E1, E2, E3, R):-
	entity(E1, _, _),
	entity(E2, _, _),
	entity(E3, _, _),
	d_right(R, Priority),
	New_priority is Priority-1,
	not(dynamic_access(E2, E3, New_priority)),
	(dynamic_access(E1, E3, Priority);owner(E1, E2)),
	retract(dynamic_access(E2, E3, Priority)).


owner_list:-
	write("Owner : Child\n\n"), 
	!,
	f(E, f(A)).

owner_list:-
	write("Owner : Child\n\n"), 
	entity(E, _, _), 
	forall(entity(E, _, _), forall(owner(OW, E), (owner(OW), owner(A,A), owner(E), owner(O,O)))).


owner_list:-
	write("Owner : Child\n\n"), 
	forall( entity(E, _, _), (forall(owner(OW, E), (owner(OW), owner(AA), owner(E), owner(WE))))).

right_list:-
	write("\nStatic rights\n"), write("Entity : Max active : Max passive\n"), forall(entity(E, Act, Pas), (write(E), write(" : "), write(Act), nl,  write(" : "), write(Pas))),
	accessRight(Ss,Sr,g),
	write("\nDynamic rights\n"), write("Entity master: Entity slave : Dynamic priority\n"), forall(dynamic_access(E1, E2, P), (write(E1), write(" : "), write(E2), write(" : "), write(P))).

