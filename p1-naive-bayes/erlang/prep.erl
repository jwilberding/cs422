-module(prep).
-export([fix/1]).

fix(Data) ->
  %% Get Class 1 data <=50K. and Class2 data >50K. 
  {Class1, Class2} = lists:partition(fun(X) -> element(15,X) == "<=50K." end, Data),
  
  io:format("Find averages~n"),
  AvgAge1 = find_avg(1, Class1),
  AvgAge2 = find_avg(1, Class2),
  
  io:format("Avg Age 1: ~w, Avg Age 2: ~w~n", [AvgAge1, AvgAge2]).

find_avg(N, L) ->
  {S,C} = lists:foldl(fun(X, {Sum, Count}) -> E = element(N,X),
                                      if
                                        E > -1 -> {E + Sum, Count+1};
                                        true -> {Sum, Count}
                                      end
				      end, 
			              {0,0}, L),
  S/C.

