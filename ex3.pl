% Instructions:
%   - Please submit ONLY this file up to GradeScope!
%   - Implement as many of the predicates as you can as specified in the assignment!
%   - Have lots of fun! Wahoo!
%   - Do not change the module information of this file or the name of this file (i.e., do not change line 6)!
:- module( ex3, [ myappend/3 , myreverse/2 , myflatten/2 , mymember/2 , myremove/3 , mymember2/2 , mysubstring/2 , mysublists/2 , mypermutation/2 , grandfather/2 , grandmother/2 , brother/2 , sister/2 , sibling/2 , cousin/2 , contact/3 , josephus/5 ]).


%%%% Question 1.
% From lectures
myappend([],Y,Y).
myappend([X|XS],Y,[X|Z]):-
    myappend(XS,Y,Z).

% From tutorials
myreverse(X,Y):-
    revHelper(X,[],Y, Y).

revHelper([], Y, Y, []).
revHelper([X|XS], Acc, Y, [_|Yp]):-
    revHelper(XS, [X|Acc], Y, Yp).

myflatten(_,_).

% Will pass if the A is the head of the list
mymember(A,[A|_]).
% Will check recursively if A is the head of the list
mymember(A, [B|AS]):-
    mymember(A,AS).

myremove(_,[],[]).
myremove(A,[A|AS],AS).
myremove(A, [B|BS],[B|CS]):-
    A \= B,
    myremove(A,BS,CS).

%%%% Question 2.
mymember2(_, _).

%%%% Question 3.
/*
% Base case
mysubstring([],_).
% Will recursively check if the head of the inputted left list is a member of the right list
mysubstring([X|XS],Y):-
    mymember(X,Y),
    mysubstring(XS,Y),!.
*/
%mysubstring([],_).
mysubstring([X|XS],[Y|YS]):-
    substringHelper([X|XS],[X|XS],[Y|YS]).


subtringHelper(_,[],_). %this line is never read for some reason
substringHelper(A,[X|XS],[Y|YS]):-
    (X =:= Y,
    substringHelper(A,XS,YS));
    substringHelper(A,A,YS).

%%%% Question 4
% Not working, currently works more like substring kinda
/*
mysublists([],_).
mysublists([X|XS], [X|YS]) :-
    mysublists(XS, YS).
mysublists([_|XS], [Y|YS]):-
    mysublists(XS, YS).
*/
% Base case
/*
mysublists([],[]).
mysublists([X|Tail1],[X|Tail2]):-
    mysublists(Tail1,Tail2).
mysublists([_|Tail1],[X|Tail2]):-
    mysublists(Tail1,Tail2).
*/
% Works but idk how it works
%https://stackoverflow.com/questions/4912869/subsets-in-prolog
mysublists(List,List).
mysublists(List,Rest):-
    sublistHelper(List,Rest).

sublistHelper([_|Tail],Rest):-
    mysublists(Tail,Rest).
sublistHelper([Head|Tail],[Head|Rest]):-
    sublistHelper(Tail,Rest). 

%%%% Question 5
% LEARN HOW THIS WORKS BRO
insertions(X,Y,[X|Y]).
insertions(X,[Y|T],[Y|Z]):-
    insertions(X,T,Z).

mypermutation([],[]).
mypermutation([X|XS],Z):-
    mypermutation(XS,S),
    insertions(X,S,Z).
    
%%%% Question 6

% Understand these predicates as follows.
%   son(Mom, Dad, Child)      is read as ``Child is the son of the mother Mom and the father Dad''
%   daughter(Mom, Dad, Child) is read as ``Child is the daughter of the mother Mom and the father Dad''

son(mymom, mydad, theson).
daughter(mymom, mydad, thedaughter).
% Add your own family members too!

% Understand these predicates as follows.
%   grandfather(A,B). is read as ``A is a grandfather of B''
%   grandmother(A,B). is read as ``A is a grandmother of B''
%   brother(A,B).     is read as ``A is a brother of B''
%   sister(A,B).      is read as ``A is a sister of B''
%   sibling(A,B).     is read as ``A is a sibling of B''
%   cousin(A,B).      is read as ``A is a cousin of B''

grandfather(A,B):-
    % mothers side and B is a grandson
    son(M,F,B),
    daughter(M1,A,M);
    % fathers side and B is a grandson
    son(M,F,B),
    son(M1,A,F);
    % mothers side and B is a granddaughter
    daughter(M,F,B),
    daughter(M1,A,M);
    % fathers side and B is a granddaughter
    daughter(M,F,B),
    son(M1,A,M).

grandmother(A,B):-
    % mothers side and B is a grandson
    son(M,F,B),
    daughter(A,F1,M);
    % fathers side and B is a grandson
    son(M,F,B),
    son(A,F1,F);
    % mothers side and B is a granddaughter
    daughter(M,F,B),
    daughter(A,F1,M);
    % fathers side and B is a granddaughter
    daughter(M,F,B),
    son(A,F1,M).

brother(A,B):-
    % Is a sister
    son(C,D,A),
    daughter(C,D,B);
    % Is a brother
    son(C,D,A),
    son(C,D,B).

sister(A,B):-
    % Is a sister
    daughter(C,D,A),
    daughter(C,D,B);
    % Is a brother
    daughter(C,D,A),
    son(C,D,B).

sibling(A,B):-
    brother(A,B);
    sister(A,B).

cousin(A,B):-
    % The parents of A and B are siblings
    sibling(Pa, Pb),
    % A is a child of Pa and B is a child of B
    (son(Pa,_,A); daughter(Pa,_,A); son(_,Pa,A); daughter(_,Pa,A)),
    (son(Pb,_,B); daughter(Pb,_,B); son(_,Pb,B); daughter(_,Pb,B)).

%%%% Question 7
contact(_,_,_).

%%%% Question 8
% Please see the assignment for the logic puzzle. 
% This question will just be hand graded!

%%%% Question 9
% The parameters are as follows...
%   - NumberOfSoldiers: the total number of soldiers including Josephus and his accomplice (> 2)
%   - StartingPosition: the starting position
%   - N: The selected number to count down
%   - J: Output position for Josephus (< NumberOfSoldiers)
%   - A: Output position for the accomplice (< NumberOfSoldiers)
% where all positions are 0 indexed
josephus(NumberOfSoldiers, StartingPosition, N, J, A).
