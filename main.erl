-module(main).
-export([start/0]).

start() ->
    Mode = string:trim(io:get_line("Mode (init/join) ? ")),
    case Mode of
        "init" ->
            db_manager:start(init),
            io:format("Base créée sur ~p~n", [node()]),
            scheduler:start();
        "join" ->
            db_manager:start(join),
            Remote = string:trim(io:get_line("Nom du nœud principal (ex: server@machine) : ")),
            RemoteNode = list_to_atom(Remote),
            case db_manager:connect_to(RemoteNode) of
                ok -> scheduler:start();
                error -> io:format("Connexion échouée. Relancez avec le bon nom de nœud.~n")
            end;
        _ ->
            io:format("Mode inconnu. Tapez init ou join.~n"),
            start()
    end.
