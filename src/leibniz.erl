-module(leibniz).
-export([main/0]).

main() ->
    {ok, Binary} = file:read_file("rounds.txt"),
    Rounds = list_to_integer(string:trim(binary_to_list(Binary))),
    Stop = Rounds + 2,
    Pi = leibniz(Stop, 2, 1.0, 1.0),
    io:format("~.16f~n", [Pi * 4.0]),
    halt(0).

leibniz(Stop, I, _X, Pi) when I > Stop ->
    Pi;
leibniz(Stop, I, X, Pi) ->
    NewX = -X,
    NewPi = Pi + NewX / (2 * I - 1),
    leibniz(Stop, I + 1, NewX, NewPi).
