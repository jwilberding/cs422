-module(p1).
-export([go/1]).

go(File) ->
  %% Load Data
  io:format("Loading data~n"),
  Data = csv_parser:load_csv(File),
  
  %% Replace Missing Values
  io:format("Fixing data~n"),
  DataFixed = prep:fix(Data),
  
  io:format("Done!~n").
