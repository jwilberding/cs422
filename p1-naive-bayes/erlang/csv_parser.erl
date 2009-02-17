-module(csv_parser).
-export([load_csv/1,check_undef_int/1]).


%% Main - {Age, WorkClass, FNLWGT, Education, EducationNum, MaritalStatus, Occupation, Relationship, Race, Sex, CapitalGain, CapitalLoss, HoursPerWeek, NativeCountry, Income}

%% Age - Int
%% WorkClass - {Local-gov,Private,Self-emp-not-inc,Federal-gov,State-gov,Self-emp-inc,Never-worked,Without-pay} 
%% FNLWGT - Int
%% Education - {11th,HS-grad,Assoc-acdm,Some-college,10th,Prof-school,7th-8th,Bachelors,Masters,Doctorate,5th-6th,Assoc-voc,9th,12th,1st-4th,Preschool}
%% EducationNum - Int
%% MaritalStatus - {Never-married,Married-civ-spouse,Widowed,Divorced,Separated,Married-spouse-absent,Married-AF-spouse}
%% Occupation - {Machine-op-inspct,Farming-fishing,Protective-serv,?,Other-service,Prof-specialty,Craft-repair,Adm-clerical,Exec-managerial,Tech-support,Sales,Priv-house-serv,Transport-moving,Handlers-cleaners,Armed-Forces}
%% Relationship - {Own-child,Husband,Not-in-family,Unmarried,Wife,Other-relative}
%% Race - {Black,White,Asian-Pac-Islander,Other,Amer-Indian-Eskimo}
%% Sex - {Male,Female}
%% CapitalGain - Int
%% CapitalLoss - Int
%% HoursPerWeek - Int
%% NativeCountry - {United-States,?,Peru,Guatemala,Mexico,Dominican-Republic,Ireland,Germany,Philippines,Thailand,Haiti,El-Salvador,Puerto-Rico,Vietnam,South,Columbia,Japan,India,Cambodia,Poland,Laos,England,Cuba,Taiwan,Italy,Canada,Portugal,China,Nicaragua,Honduras,Iran,Scotland,Jamaica,Ecuador,Yugoslavia,Hungary,Hong,Greece,Trinadad&Tobago,Outlying-US(Guam-USVI-etc),France}
%% Income - {<=50K.,>50K.}


%% TODO: Parse 1000 lines at a time, and use pmap to process into tuples


%% Loads CSV file in arff format and returns list of tuples, 1 for each entry
load_csv(F) ->
  io:format("Loading: ~s~n", [F]),
  load_file(F).

 
load_file(Name) ->
  {ok, Device} = file:open(Name, [read]),
  for_each_line(Device, [],  1).


for_each_line(Device, Accum, Count) ->
  case io:get_line(Device, "") of
    eof  -> file:close(Device), Accum;
    Line ->
      TestStr = hd(Line),
      if 
        TestStr == $@ ->
	  %io:format("Found1: ~w~n", [TestStr]),
          for_each_line(Device, Accum, Count+1);
	TestStr == $\n ->
	  %io:format("Found2: ~w~n", [TestStr]),
          for_each_line(Device, Accum, Count+1);
        true ->
	  %io:format("Parse This: ~w~n", [TestStr]),
          for_each_line(Device, [parse_line(Line) | Accum], Count)
      end
  end.

  
parse_line(L) ->
  Tokens = string:tokens(L, ", "),
  %io:format("Tokens: ~w~n", [Tokens]),
  %io:format("Age: ~w~n", [list_to_integer(lists:nth(1,Tokens))]),
  Age = check_undef_int(lists:nth(1,Tokens)),
  WorkClass = lists:nth(2,Tokens),
  FNLWGT = check_undef_int(lists:nth(3,Tokens)),
  Education = lists:nth(4,Tokens),
  EducationNum = check_undef_int(lists:nth(5,Tokens)),
  MaritalStatus = lists:nth(6,Tokens),
  Occupation = lists:nth(7,Tokens),
  Relationship = lists:nth(8,Tokens),
  Race = lists:nth(9,Tokens),
  Sex = lists:nth(10,Tokens),
  CapitalGain = check_undef_int(lists:nth(11,Tokens)),
  CapitalLoss = check_undef_int(lists:nth(12,Tokens)),
  HoursPerWeek = check_undef_int(lists:nth(13,Tokens)),
  NativeCountry = lists:nth(14,Tokens),
  Income = lists:nth(15,Tokens),
  {Age, WorkClass, FNLWGT, Education, EducationNum, MaritalStatus, Occupation, Relationship, Race, Sex, CapitalGain, CapitalLoss, HoursPerWeek, NativeCountry, Income}.


check_undef_int(S) ->
  if 
    S == "?" -> -1;	
    true -> list_to_integer(S)
  end.
