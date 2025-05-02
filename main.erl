-module(main).
-export([start/0]).

start() ->
    Mode = string:trim(io:get_line("Mode (init/join) ? ")),
    case Mode of
        "init" ->
            db_manager:start(init),
            io:format("Base créée sur ~p~n", [node()]),
            scheduler:menu();  % lance le menu après création

        "join" ->
            db_manager:start(join),
            Remote = string:trim(io:get_line("Nom du nœud principal (ex: server@nom-pc) : ")),
            RemoteNode = list_to_atom(Remote),
            db_manager:connect_to(RemoteNode),
            io:format("Connecté à la base sur ~p~n", [RemoteNode]),
            scheduler:menu();

        _ ->
            io:format("Mode inconnu. Tapez init ou join.~n"),
            start()
    end.
