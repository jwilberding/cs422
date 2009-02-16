-module(csv_parser).
-export([load_csv/1]).

load_csv(F) ->
  io:format("Loading: ~s~n", [F]).

%% Main - {Age, WorkClass, FNLWGT, Education, EducationNum, MaritalStatus, Occupation, Relationship, Race, Sex, CapitalGain, CapitalLoss, HoursPerWeek, NativeCountry, Income}

%% Age - Int
%% WorkClass - 
