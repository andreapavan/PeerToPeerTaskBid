-module(test).
-export([fac/1, average/1, double/1, member/2, averageAcc/1, allsame/1]).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

% Common functional patterns

average(X) -> sum(X) / len(X).
	
sum([H|T]) -> H + sum(T);
sum([]) -> 0.
	
len([_|T]) -> 1 + len(T);
len([]) -> 0.

double([H|T]) -> [2*H|double(T)];
double([]) -> [].
	
member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_, []) -> false.

% Average with accumulators

averageAcc(X) -> average(X, 0, 0).
average([H|T], Length, Sum) -> average(T, Length + 1, Sum + H);
average([], Length, Sum) -> Sum / Length.

% Other examples

allsame([]) -> true;
allsame([_]) -> true;
allsame([H,H|T]) -> allsame([H|T]).