%useful helper functions
appendX([], L, L). %base case, exit out once first list is empty. i.e. once L1 matches [] we evalute this rule
appendX([A|L1], L2, [A|L3]) :- appendX(L1, L2, L3).

memberX(X, [X|_]).
memberX(X, [_|L]) :- memberX(X, L). %true if X is anywhere in S. [Y|L] cuts the list so we can get the first value and the remaining values out. if X is not Y, then we iterate on the rest of L


% appendX(X, L, [X|L]).
% appendX(X, L1, [X|L1]). %append X onto front of L

%Q1
setIntersect([], _, []). %set S3 -> [] when S1 is empty
setIntersect([A|S1], S2, [A|S3]) :- memberX(A, S2), setIntersect(S1, S2, S3), !. %we assume S1 and S2 consist of unique atoms
setIntersect([_|S1], S2, S3) :- setIntersect(S1, S2, S3). %now we assume we are NOT appending!

%Q2

swap([], []). % when L is [], R is []
swap([A, B|L], [B, A|R]) :- swap(L, R), !.
swap([A|L], [A|R]) :- swap(L, R), !.

%Q3

isList([_|_]). %true if passed a list -> assume not empty list

filter([], _, _, []). %base case
filter([A|L], OP, N, L1) :- isList(A), !, filter(A, OP, N, L2), filter(L, OP, N, L3), appendX(L2, L3, L1). %if first part of list is a list, run filter on it and second half
filter([A|L], equal, N, [A|L1]) :- A == N, filter(L, equal, N, L1), !.
filter([A|L], greaterThan, N, [A|L1]) :- A > N, filter(L, greaterThan, N, L1), !.
filter([A|L], lessThan, N, [A|L1]) :- A < N, filter(L, lessThan, N, L1), !.
filter([_|L], OP, N, L1) :- filter(L, OP, N, L1), !. %otherwise, if the condition was not satisfied, do not append the value to L1


%Q4
% X is the value to add to N1, N2 is the return object -> iterate through N1, as soon as we have a match, add it, or if we reach the end, just append it
addToList(X, [], [[X, 1]]).
addToList(X, [[X, A2]| N1], N2) :- A3 is A2 + 1, appendX([[X, A3]], N1, N2), !. %X is the current list item -> return it with + 1 along with all the others and break
addToList(X, [A|N1], [A|N2]) :- addToList(X, N1, N2). %X isnt in the current item

countAll([], []). %TODO: this needs to be sorted!
countAll([A|L], N) :- countAll(L, N1), addToList(A, N1, N). %N1 contains all the other counted values in the correct form, add A to the list in the correct form

%Q5

replace(_, _, [], []).
replace(X, E, [X|L], [E|R]) :- replace(X, E, L, R). %replace and continue
replace(X, E, [A|L], R) :- isList(A), replace(X, E, A, R1), replace(X, E, L, R2), appendX([R1], R2, R). %take an expression E and replace all X with it, for all L, put that in R -> remember that R1 is a list since A is a list and R1 is just the replaced version
replace(X, E, [A|L], [A|R]) :- replace(X, E, L, R). %A isn't a list, just keep iterating forward

sub(L, [], L). %no more expressions to replace with -> return the replaced list
sub(L, [[X, E]|S], L1) :- replace(X, E, L, L2), sub(L2, S, L1). %L2 returns S replaced with E, then call sub for next replacement value

%Q6

%for testing -> remove
edge(a,b).
% edge(b,c).
% edge(c,a).
node(a).
node(b).
node(c).

productX(_, [], []).
productX(X, [Y|L], [[X|Y]|R]) :- productX(X, L, R). %takes the product of a variable against all other items and returns that list -> put X onto the front of every list in L and return it in R

getSubsets([], [[]]). %return the empty set for base case
getSubsets([X|S], R) :- getSubsets(S, R1), productX(X, R1, R2), appendX(R2, R1, R). %get subsets of the rest of S, take the product of X against R1 to get R2, append R2 onto R1

connectionsExist(_, []).
connectionsExist(X, [Y|C]) :- edge(X, Y), connectionsExist(X, C). %edges are undirected, so we need to check both ways
connectionsExist(X, [Y|C]) :- edge(Y, X), connectionsExist(X, C).

isClique([]).
isClique([X|C]) :- node(X), connectionsExist(X, C), isClique(C). %checks if C is a clique -> goes through every element in a flat list and checks if it has an edge to every other element

clique(L) :- nonvar(L), isClique(L). %if we are just given a list, run isClique on it.
clique(L) :- var(L), findall(X, node(X), N), getSubsets(N, N1), !, clique(N1, L). %only get N once, then call clique with the list we got -> pass list of subsets here
clique([[]|_], []) :- !. %stop once we reach the empty set.
clique([X|_], L) :- isClique(X), L = X.
clique([_|N], L) :- clique(N, L). %when ; is entered, fall into this rule and continue with the rest of the list

%Q7

splitQ(L, L1, L2) :- appendX(L1,[q|L2], L). 

replaceEW([], []).
replaceEW([e|X], R) :- replaceEW(X, R).
replaceEW([q|X], [q|R]) :- replaceEW(X, R). %might not need this, just in case
replaceEW([_|X], [w|R]) :- replaceEW(X, R). %given a list, replace with w's and remove e's

%depending what level we're on, we call one of two functions
%one goes through and acts like we are outside a set of qs
%one goes through and acts like we are inside a set of qs
%as soon as we hit a q, take the given string and replace the body with whatever we iterated through
%if no q is hit, then just continue like we are not inside a set

%not sure i love the 0 and 1 flip but it works...
convert([], []) :- !.
convert(Term, Result) :- convert(Term, 1, Result), !. %top case, call convert on the list and return what it gives you
convert(Term, Result) :- replaceEW(Term, Result), !. %if splitQ is false, then there are no occurrences of q in the rest of the list, so replace all values
convert([], _, []).
convert(Term, 0, Result) :- splitQ(Term, L1, L2), convert(L2, 1, R1), appendX(L1, [q|R1], Result). %no replace -> return the constructed list of L1 with the rest of converted L2 -> put q back on where we took it out
convert(Term, 1, Result) :- splitQ(Term, L1, L2), replaceEW(L1, R1), convert(L2, 0, R2), appendX(R1, [q|R2], Result). %replace
convert(Term, _, Result) :- replaceEW(Term, Result). %if splitQ fails, it returns false and falls through to here where we iterate through the rest of the list and return