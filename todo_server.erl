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
    catch gen_tcp:send(Sock, "Bienvenue sur le Todo Server !\n1 - Connexion\n2 - Créer un nouvel utilisateur\n> "),
    case gen_tcp:recv(Sock, 0) of
        {ok, ChoiceBin} ->
            Choice = string:trim(binary_to_list(ChoiceBin)),
            gen_tcp:send(Sock, "Nom d'utilisateur: "),
            {ok, NameBin} = gen_tcp:recv(Sock, 0),
            Name = string:trim(binary_to_list(NameBin)),
            gen_tcp:send(Sock, "Mot de passe: "),
            {ok, PassBin} = gen_tcp:recv(Sock, 0),
            Pass = string:trim(binary_to_list(PassBin)),
            handle_login_choice(Choice, Name, Pass, Sock);
        {error, closed} ->
            io:format("Connexion fermée brusquement~n")
    end.

handle_login_choice("1", Name, Pass, Sock) ->
    case todo_db:check_user(Name, Pass) of
        {atomic, ok} ->
            gen_tcp:send(Sock, "Connecté avec succès.\n"),
            user_loop(Sock, Name);
        {atomic, {error, wrong_password}} ->
            gen_tcp:send(Sock, "Mot de passe incorrect. Déconnexion.\n"),
            gen_tcp:close(Sock);
        {atomic, {error, not_found}} ->
            gen_tcp:send(Sock, "Utilisateur introuvable. Déconnexion.\n"),
            gen_tcp:close(Sock);
        Other ->
            io:format("Erreur: ~p~n", [Other]),
            gen_tcp:close(Sock)
    end;
handle_login_choice("2", Name, Pass, Sock) ->
    case todo_db:create_user(Name, Pass) of
        {atomic, ok} ->
            gen_tcp:send(Sock, "Utilisateur créé avec succès.\n"),
            user_loop(Sock, Name);
        {atomic, {error, exists}} ->
            gen_tcp:send(Sock, "Utilisateur déjà existant. Déconnexion.\n"),
            gen_tcp:close(Sock);
        Other ->
            io:format("Erreur: ~p~n", [Other]),
            gen_tcp:close(Sock)
    end;
handle_login_choice(_, _, _, Sock) ->
    gen_tcp:send(Sock, "Choix invalide. Déconnexion.\n"),
    gen_tcp:close(Sock).

user_loop(Sock, Name) ->
    gen_tcp:send(Sock, "Commandes: add | done <date jj/mm/aaaa> <tâche> | show <date jj/mm/aaaa> | showall | quit\n> "),
    case gen_tcp:recv(Sock, 0) of
        {ok, Bin} ->
            Line = string:trim(binary_to_list(Bin)),
            case string:tokens(Line, " ") of
                ["add"] ->
                    gen_tcp:send(Sock, "Jour (1-31): "),
                    {ok, DayBin} = gen_tcp:recv(Sock, 0),
                    Day = list_to_integer(string:trim(binary_to_list(DayBin))),
                    gen_tcp:send(Sock, "Mois (1-12): "),
                    {ok, MonthBin} = gen_tcp:recv(Sock, 0),
                    Month = list_to_integer(string:trim(binary_to_list(MonthBin))),
                    gen_tcp:send(Sock, "Année (>=2025): "),
                    {ok, YearBin} = gen_tcp:recv(Sock, 0),
                    Year = list_to_integer(string:trim(binary_to_list(YearBin))),
                    gen_tcp:send(Sock, "Tâche: "),
                    {ok, TaskBin} = gen_tcp:recv(Sock, 0),
                    Task = string:trim(binary_to_list(TaskBin)),
                    case todo_db:add_task(Name, Day, Month, Year, Task) of
                        {atomic, ok} ->
                            gen_tcp:send(Sock, "Tâche ajoutée.\n");
                        {error, invalid_date} ->
                            gen_tcp:send(Sock, "Date invalide.\n");
                        {error, past_date} ->
                            gen_tcp:send(Sock, "Vous ne pouvez pas ajouter une tâche dans le passé.\n")
                    end,

                    user_loop(Sock, Name);

                ["done", DateStr | Toks] ->
                    Task = string:join(Toks, " "),
                    case todo_db:set_done(Name, DateStr, Task) of
                        {atomic, ok} ->
                            gen_tcp:send(Sock, "Tâche marquée comme faite.\n");
                        {atomic, {error, not_found}} ->
                            gen_tcp:send(Sock, "Tâche non trouvée.\n")
                    end,
                    user_loop(Sock, Name);

                ["show", DateStr] ->
                    case todo_db:get_day_tasks(Name, DateStr) of
                        {atomic, Tasks} ->
                            lists:foreach(fun(T) ->
                                gen_tcp:send(Sock, io_lib:format("~p\n", [T]))
                            end, Tasks);
                        _ ->
                            gen_tcp:send(Sock, "Erreur lors de la récupération.\n")
                    end,
                    user_loop(Sock, Name);

                ["showall"] ->
                    case todo_db:get_all_tasks(Name) of
                        {atomic, Tasks} ->
                            lists:foreach(fun(T) ->
                                gen_tcp:send(Sock, io_lib:format("~p\n", [T]))
                            end, Tasks);
                        _ ->
                            gen_tcp:send(Sock, "Erreur lors de la récupération.\n")
                    end,
                    user_loop(Sock, Name);

                ["quit"] ->
                    gen_tcp:send(Sock, "Au revoir !\n"),
                    gen_tcp:close(Sock);

                _ ->
                    gen_tcp:send(Sock, "Commande invalide.\n"),
                    user_loop(Sock, Name)
            end;
        {error, closed} ->
            io:format("Déconnexion de ~s~n", [Name])
    end.

