-module(todo_client).
-export([start/2]).

start(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    spawn(fun() -> listen(Socket) end),
    send_loop(Socket).

send_loop(Socket) ->
    Input = io:get_line(""),
    case string:trim(Input) of
        "quit" ->
            gen_tcp:send(Socket, "quit\n"),
            gen_tcp:close(Socket),
            io:format("Déconnecté.~n");
        Text ->
            gen_tcp:send(Socket, Text ++ "\n"),
            send_loop(Socket)
    end.

listen(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("~s", [Data]),
            listen(Socket);
        {error, closed} ->
            io:format("Connexion fermée~n")
    end.
