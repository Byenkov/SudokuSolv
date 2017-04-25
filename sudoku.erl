-module(sudoku).
-include_lib("wx/include/wx.hrl").
-export([start/0]).

%%Przykladowe plansze

fillableExample1() ->
  [ [ 9, 5, 0, 0, 0, 6, 4, 7, 0 ],
    [ 4, 0, 8, 7, 0, 2, 0, 0, 0 ],
    [ 6, 2, 0, 4, 0, 0, 0, 5, 0 ],
    [ 5, 0, 2, 0, 6, 0, 3, 0, 0 ],
    [ 0, 0, 0, 2, 0, 7, 0, 0, 0 ],
    [ 0, 0, 4, 0, 1, 0, 2, 0, 8 ],
    [ 0, 7, 0, 0, 0, 9, 0, 3, 4 ],
    [ 0, 0, 0, 1, 0, 3, 7, 0, 5 ],
    [ 0, 4, 3, 5, 0, 0, 0, 2, 9 ]
  ].

fillableExample2() ->
  [ [ 0, 9, 0, 0, 6, 7, 1, 0, 0 ],
    [ 0, 8, 0, 9, 0, 0, 3, 2, 0 ],
    [ 1, 0, 0, 0, 0, 0, 9, 0, 0 ],
    [ 0, 0, 0, 3, 0, 0, 6, 0, 2 ],
    [ 0, 0, 8, 0, 0, 0, 7, 0, 0 ],
    [ 3, 0, 6, 0, 0, 2, 0, 0, 0 ],
    [ 0, 0, 5, 0, 0, 0, 0, 0, 9 ],
    [ 0, 6, 2, 0, 0, 5, 0, 8, 0 ],
    [ 0, 0, 9, 2, 8, 0, 0, 1, 0 ]
  ].

fillableExample3() ->
  [ [ 0, 0, 0, 3, 0, 0, 4, 0, 0 ],
    [ 9, 0, 0, 0, 1, 0, 0, 0, 0 ],
    [ 4, 0, 8, 0, 7, 0, 5, 0, 3 ],
    [ 0, 0, 0, 6, 0, 0, 9, 0, 0 ],
    [ 0, 0, 0, 1, 5, 7, 0, 0, 0 ],
    [ 0, 0, 5, 0, 0, 2, 0, 0, 0 ],
    [ 2, 0, 4, 0, 0, 0, 6, 0, 5 ],
    [ 0, 0, 0, 0, 2, 0, 0, 0, 8 ],
    [ 0, 0, 6, 7, 0, 8, 0, 0, 0 ]
  ].

getBoard1() ->  toCordsValue(fillableExample1()).
getBoard2() ->  toCordsValue(fillableExample2()).
getBoard3() ->  toCordsValue(fillableExample3()).

toCordsValue(Matrix)->
  lists:zip([ {X,Y} || X<-lists:seq(1,9), Y<-lists:seq(1,9) ], lists:flatten(Matrix)).

getVal(Matrix, {X,Y}) ->
  element(2, lists:keyfind({X,Y}, 1, Matrix)).


getRow( Matrix, {Row,_C} ) ->
  [X || {{R,_},X} <- Matrix, R==Row].

getColumn( Matrix, {_,Y})->
  [X || {{_,C},X} <- Matrix, C == Y].

getSubmatrix(Matrix, {X,Y})->
  NX = ( (X-1) div 3)*3 + 1,
  NY = ( (Y-1) div 3)*3 + 1,
  [E || {{R,C},E} <- Matrix, (R - NX < 3), (C - NY <3), (R - NX >= 0), (C - NY >= 0) ].

checkNumber(X) when (X >= 0) and (X < 10) -> true;
checkNumber(_) -> false.

checkForDuplicates(List) ->
  erlang:length(List) == sets:size(sets:from_list(List)).

deleteAllZeros(List) -> [E || E <- List, E /= 0].

checkForAllDuplicates(Matrix) ->
  lists:foldl(fun(X, A) -> X and A end, true,[checkForDuplicates(deleteAllZeros(getRow( Matrix, {X,Y} )))|| X <- lists:seq(1,9), Y <- [1]]) and
  lists:foldl(fun(X, A) -> X and A end, true,[checkForDuplicates(deleteAllZeros(getColumn( Matrix, {Y,X} )))|| X <- lists:seq(1,9), Y <- [1]]) and
  lists:foldl(fun(X, A) -> X and A end, true,[checkForDuplicates(deleteAllZeros(getSubmatrix( Matrix, {X,Y} )))|| X <- [2,4,6], Y <- [2,4,6]]).


checkIfFillable(_, {1, 10}) -> true;
checkIfFillable(Matrix, {10, Y}) -> checkIfFillable(Matrix, {1, Y+1});
checkIfFillable(Matrix, {X,Y}) ->
  lists:foldl(fun(X, A) -> A and checkNumber(X) end, true, getRow(Matrix, {X,Y})) and
  lists:foldl(fun(X, A) -> A and checkNumber(X) end, true, getColumn(Matrix, {X,Y})) and
  lists:foldl(fun(X, A) -> A and checkNumber(X) end, true, getSubmatrix(Matrix, {X,Y})) and
    checkIfFillable(Matrix, {X, Y+1}).

checkCorrectness(Checked)->
  lists:sort(Checked) =:= lists:seq(1,9).

simplePointChecker(Matrix, {X,Y}) ->
  checkCorrectness(getRow(Matrix, {X,Y})),
  checkCorrectness(getColumn(Matrix, {X,Y})),
  checkCorrectness(getSubmatrix(Matrix, {X,Y})).

simpleChecker(_, {10, 1}) -> true;
simpleChecker(Matrix, {X, 10}) -> simpleChecker(Matrix, {X+1, 1});
simpleChecker(Matrix, {X,Y}) ->
  simplePointChecker(Matrix, {X,Y}) and simpleChecker(Matrix, {X,Y+1}).

simpleMatrixChecker(Matrix) -> simpleChecker(Matrix, {1,1}) and not(lists:keymember(0, 2, Matrix)).

available(Matrix, {X,Y})->
  case getVal(Matrix, {X,Y}) of
    0 ->
      lists:subtract(lists:subtract(lists:subtract(lists:seq(1,9), getRow( Matrix, {X,Y} )), getColumn( Matrix, {X,Y})), getSubmatrix(Matrix, {X,Y}));
    _Else ->
      {}
  end.

changeValue(Matrix, {X,10}, V) -> changeValue(Matrix, {X+1,1}, V);
changeValue(Matrix, {X,Y}, V) ->
  lists:keyreplace({X,Y}, 1, Matrix, {{X,Y}, V}).

check(Matrix) -> checkIfFillable(Matrix, {1,1}) and checkForAllDuplicates(Matrix).

solve(Matrix) ->
  Server = self(),
  solution(Server, Matrix, {1,1}).

solution(Server, Matrix, {9, 10}) -> Server ! Matrix;
solution(Server, Matrix, {X, 10}) -> solution(Server, Matrix, {X+1, 1});
solution(Server, Matrix, {X,Y}) ->
  case getVal(Matrix, {X,Y}) of
    0 ->
      List = available(Matrix, {X,Y}),
      lists:map(fun(L) -> spawn(fun() -> solver(Server, changeValue(Matrix, {X, Y}, L), {X, Y+1}) end) end, List),
      loop(notfound);
    _Else ->
      solution(Server, Matrix, {X,Y+1})
  end.

solver(Server, Matrix, {9,10}) ->
  case simpleMatrixChecker(Matrix) of
    true ->
      Server ! Matrix;
    _ ->
      Server ! notfound
  end;
solver(Server, Matrix, {X, 10}) -> solver(Server, Matrix, {X+1, 1});
solver(Server, Matrix, {X,Y}) ->
  case simpleMatrixChecker(Matrix) of
    true ->
      Server ! Matrix;
    _ ->
      next(Server, Matrix, {X,Y})
  end.

next(Server, Matrix, {X,Y}) ->
  case getVal(Matrix, {X,Y}) of
    0 ->
      List = available(Matrix, {X,Y}),
      lists:map(fun(L) -> spawn(fun() -> solver(Server, changeValue(Matrix, {X, Y}, L), {X, Y+1}) end) end, List);
    _Else ->
      solver(Server, Matrix, {X,Y+1})
  end.

loop(Ret) ->
  receive
    notfound ->
      loop(Ret);
    Matrix ->
      Matrix
  end.

start() ->
  State = make_window(),
  gloop (State).

make_window() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, "Sudoku", [{size,{350, 300}}]),
  Panel = wxPanel:new(Frame),

  B101 = wxButton:new(Panel, 101, [{label, "Reset"}]),
  wxButton:setToolTip(B101, "All values will be set to 0"),
  B102 = wxButton:new(Panel, 102, [{label, "Solve"}]),
  wxButton:setToolTip(B102, "Current board will be solved"),
  B103 = wxButton:new(Panel, 103, [{label, "Example 1"}]),
  wxButton:setToolTip(B103, "Easy Difficulty"),
  B104 = wxButton:new(Panel, 104, [{label, "Example 2"}]),
  wxButton:setToolTip(B104, "Medium Difficulty"),
  B105 = wxButton:new(Panel, 105, [{label, "Example 3"}]),
  wxButton:setToolTip(B105, "Hard Difficulty"),

  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Board"}]),

  Grid = create_grid(Panel),
  ButtonSizer = wxBoxSizer:new(?wxVERTICAL),

  Options = [{flag, ?wxEXPAND}, {proportion, 0}],
  wxSizer:addSpacer(ButtonSizer, 5),
  wxSizer:add(Sizer, Grid, Options),
  wxSizer:addSpacer(ButtonSizer, 20),
  wxSizer:add(ButtonSizer, B103,  []),
  wxSizer:addSpacer(ButtonSizer, 10),
  wxSizer:add(ButtonSizer, B104,  []),
  wxSizer:addSpacer(ButtonSizer, 10),
  wxSizer:add(ButtonSizer, B105,  []),
  wxSizer:addSpacer(ButtonSizer, 40),
  wxSizer:add(MainSizer, Sizer, Options),
  wxSizer:add(ButtonSizer, B101,  []),
  wxSizer:addSpacer(ButtonSizer, 10),
  wxSizer:add(ButtonSizer, B102,  []),
  wxSizer:addSpacer(ButtonSizer, 20),
  wxSizer:add(MainSizer, ButtonSizer, []),

  wxPanel:setSizer(Panel, MainSizer),
  wxFrame:show(Frame),

  wxFrame:connect( Frame, close_window),
  wxPanel:connect(Panel, command_button_clicked),

  {Frame, Grid}.

create_grid(Panel) ->
  Grid = wxGrid:new(Panel, 2, []),
  wxGrid:createGrid(Grid, 9, 9),
  [wxGrid:setColLabelValue(Grid, X, integer_to_list(X+1)) || X <- lists:seq(0,8)],
  Fun =
    fun(Row) ->
      wxGrid:setCellValue(Grid, Row, 0, "0"),
      wxGrid:setCellValue(Grid, Row, 1, "0"),
      wxGrid:setCellValue(Grid, Row, 2, "0"),
      wxGrid:setCellValue(Grid, Row, 3, "0"),
      wxGrid:setCellValue(Grid, Row, 4, "0"),
      wxGrid:setCellValue(Grid, Row, 5, "0"),
      wxGrid:setCellValue(Grid, Row, 6, "0"),
      wxGrid:setCellValue(Grid, Row, 7, "0"),
      wxGrid:setCellValue(Grid, Row, 8, "0")
    end,
  wx:foreach(Fun, lists:seq(0,8)),
  wxGrid:autoSize(Grid),
  Grid.

gloop(State) ->
  {Frame, Grid}  = State,
  Fun =
    fun(Row) ->
      wxGrid:setCellValue(Grid, Row, 0, "0"),
      wxGrid:setCellValue(Grid, Row, 1, "0"),
      wxGrid:setCellValue(Grid, Row, 2, "0"),
      wxGrid:setCellValue(Grid, Row, 3, "0"),
      wxGrid:setCellValue(Grid, Row, 4, "0"),
      wxGrid:setCellValue(Grid, Row, 5, "0"),
      wxGrid:setCellValue(Grid, Row, 6, "0"),
      wxGrid:setCellValue(Grid, Row, 7, "0"),
      wxGrid:setCellValue(Grid, Row, 8, "0")
    end,
  receive
    #wx{event=#wxClose{}} ->
      wxWindow:destroy(Frame),
      ok;
    #wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
      io:format("Resetting... ~n"),
      wx:foreach(Fun, lists:seq(0,8)),
      gloop(State);
    #wx{id = 102, event=#wxCommand{type = command_button_clicked}} ->
      io:format("Solving... ~n"),
      Vals = [list_to_integer(wxGrid:getCellValue(Grid, X, Y)) || X <- lists:seq(0,8), Y <- lists:seq(0,8)],
      Matrix = lists:zip( [ {X,Y} || X<-lists:seq(1,9), Y<-lists:seq(1,9) ], Vals),
      case check(Matrix) of
        true ->
          Solved = solve(Matrix),
          [wxGrid:setCellValue(Grid, X, Y, integer_to_list(getVal(Solved, {X+1, Y+1}))) || X <- lists:seq(0,8), Y <- lists:seq(0,8)];
        _Else ->
          io:format("Wrong Format! ~n")
      end,
      gloop(State);
    #wx{id = 103, event=#wxCommand{type = command_button_clicked}} ->
      io:format("Loading example board 1... ~n"),
      Board = getBoard1(),
      [wxGrid:setCellValue(Grid, X, Y, integer_to_list(getVal(Board, {X+1, Y+1}))) || X <- lists:seq(0,8), Y <- lists:seq(0,8)],
      gloop(State);
    #wx{id = 104, event=#wxCommand{type = command_button_clicked}} ->
      io:format("Loading example board 2... ~n"),
      Board = getBoard2(),
      [wxGrid:setCellValue(Grid, X, Y, integer_to_list(getVal(Board, {X+1, Y+1}))) || X <- lists:seq(0,8), Y <- lists:seq(0,8)],
      gloop(State);
    #wx{id = 105, event=#wxCommand{type = command_button_clicked}} ->
      io:format("Loading example board 3... ~n"),
      Board = getBoard3(),
      [wxGrid:setCellValue(Grid, X, Y, integer_to_list(getVal(Board, {X+1, Y+1}))) || X <- lists:seq(0,8), Y <- lists:seq(0,8)],
      gloop(State);
    Msg ->
      io:format("Default triggered: Got ~n ~p ~n", [Msg]),
      gloop(State)
  end.
