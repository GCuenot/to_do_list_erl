
-module(todo_server).
-export([start/1, accept/1, handle_client/1]).

start(Port) ->
    todo_db:start(),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Todo Server running on port ~p~n", [Port]),
    accept(LSock).

accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> handle_client(Sock) end),
    accept(LSock).

handle_client(Sock) ->
    gen_tcp:send(Sock, "Bienvenue sur le Todo Server !
1 - Connexion
2 - Créer un nouvel utilisateur
> "),
    {ok, ChoiceBin} = gen_tcp:recv(Sock, 0),
    Choice = string:trim(binary_to_list(ChoiceBin)),
    gen_tcp:send(Sock, "Nom d'utilisateur: "),
    {ok, NameBin} = gen_tcp:recv(Sock, 0),
    Name = string:trim(binary_to_list(NameBin)),
    gen_tcp:send(Sock, "Mot de passe: "),
    {ok, PassBin} = gen_tcp:recv(Sock, 0),
    Pass = string:trim(binary_to_list(PassBin)),
    case Choice of
        "1" ->
            case todo_db:check_user(Name, Pass) of
                {atomic, ok} ->
                    gen_tcp:send(Sock, "Connecté avec succès.
"),
                    user_loop(Sock, Name);
                {atomic, {error, wrong_password}} ->
                    gen_tcp:send(Sock, "Mot de passe incorrect. Déconnexion.
"),
                    gen_tcp:close(Sock);
                {atomic, {error, not_found}} ->
                    gen_tcp:send(Sock, "Utilisateur introuvable. Déconnexion.
"),
                    gen_tcp:close(Sock);
                Other ->
                    io:format("Erreur: ~p~n", [Other]),
                    gen_tcp:close(Sock)
            end;
        "2" ->
            case todo_db:create_user(Name, Pass) of
                {atomic, ok} ->
                    gen_tcp:send(Sock, "Utilisateur créé avec succès.
"),
                    user_loop(Sock, Name);
                {atomic, {error, exists}} ->
                    gen_tcp:send(Sock, "Utilisateur déjà existant. Déconnexion.
"),
                    gen_tcp:close(Sock);
                Other ->
                    io:format("Erreur: ~p~n", [Other]),
                    gen_tcp:close(Sock)
            end;
        _ ->
            gen_tcp:send(Sock, "Choix invalide. Déconnexion.
"),
            gen_tcp:close(Sock)
    end.

user_loop(Sock, Name) ->
    gen_tcp:send(Sock, "Commandes: add <jour> <tâche> | done <jour> <tâche> | show <jour> | showall | quit
> "),
    case gen_tcp:recv(Sock, 0) of
        {ok, Bin} ->
            Line = string:trim(binary_to_list(Bin)),
            case string:tokens(Line, " ") of
                ["add", Day | Toks] ->
                    Task = string:join(Toks, " "),
                    todo_db:add_task(Name, Day, Task),
                    gen_tcp:send(Sock, "Tâche ajoutée.
"),
                    user_loop(Sock, Name);
                ["done", Day | Toks] ->
                    Task = string:join(Toks, " "),
                    todo_db:set_done(Name, Day, Task),
                    gen_tcp:send(Sock, "Tâche marquée comme faite.
"),
                    user_loop(Sock, Name);
                ["show", Day] ->
                    {atomic, Tasks} = todo_db:get_day_tasks(Name, Day),
                    lists:foreach(fun(T) ->
                        gen_tcp:send(Sock, io_lib:format("~p
", [T]))
                    end, Tasks),
                    user_loop(Sock, Name);
                ["showall"] ->
                    {atomic, Tasks} = todo_db:get_all_tasks(Name),
                    lists:foreach(fun(T) ->
                        gen_tcp:send(Sock, io_lib:format("~p
", [T]))
                    end, Tasks),
                    user_loop(Sock, Name);
                ["quit"] ->
                    gen_tcp:send(Sock, "Au revoir !
"),
                    gen_tcp:close(Sock);
                _ ->
                    gen_tcp:send(Sock, "Commande invalide.
"),
                    user_loop(Sock, Name)
            end;
        {error, closed} ->
            io:format("Déconnexion de ~s~n", [Name])
    end.
