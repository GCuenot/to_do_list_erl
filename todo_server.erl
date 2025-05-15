-module(todo_server).
-export([start/1, accept/1, handle_client/1]).

-include("todo_db.hrl").

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
            case Choice of
                "1" ->
                    gen_tcp:send(Sock, "Nom d'utilisateur: "),
                    {ok, NameBin} = gen_tcp:recv(Sock, 0),
                    Name = string:trim(binary_to_list(NameBin)),
                    gen_tcp:send(Sock, "Mot de passe: "),
                    {ok, PassBin} = gen_tcp:recv(Sock, 0),
                    Pass = string:trim(binary_to_list(PassBin)),
                    handle_login_choice("1", Name, Pass, Sock);

                "2" ->
                    gen_tcp:send(Sock, "Nom d'utilisateur: "),
                    {ok, NameBin} = gen_tcp:recv(Sock, 0),
                    Name = string:trim(binary_to_list(NameBin)),
                    gen_tcp:send(Sock, "Mot de passe: "),
                    {ok, PassBin} = gen_tcp:recv(Sock, 0),
                    Pass = string:trim(binary_to_list(PassBin)),
                    handle_login_choice("2", Name, Pass, Sock);

                _ ->
                    gen_tcp:send(Sock, "Choix invalide.\n"),
                    handle_client(Sock)
            end;
        {error, closed} ->
            io:format("Connexion fermée brusquement~n")
    end.

handle_login_choice("1", Name, Pass, Sock) ->
    case todo_db:check_user(Name, Pass) of
        {atomic, ok} ->
            gen_tcp:send(Sock, "Connecté avec succès.\n"),
            user_loop(Sock, Name);
        {atomic, {error, wrong_password}} ->
            gen_tcp:send(Sock, "Mot de passe incorrect. Retour au menu.\n"),
            handle_client(Sock);
        {atomic, {error, not_found}} ->
            gen_tcp:send(Sock, "Utilisateur introuvable. Retour au menu.\n"),
            handle_client(Sock);
        _ ->
            gen_tcp:send(Sock, "Erreur système. Retour au menu.\n"),
            handle_client(Sock)
    end;
handle_login_choice("2", Name, Pass, Sock) ->
    case todo_db:create_user(Name, Pass) of
        {atomic, ok} ->
            gen_tcp:send(Sock, "Utilisateur créé avec succès.\n"),
            user_loop(Sock, Name);
        {atomic, {error, exists}} ->
            gen_tcp:send(Sock, "Utilisateur déjà existant.\n"),
            handle_client(Sock);
        _ ->
            gen_tcp:send(Sock, "Erreur système. Retour au menu.\n"),
            handle_client(Sock)
    end.

user_loop(Sock, Name) ->
    gen_tcp:send(Sock, 
"========================================\r\n"
"         GESTIONNAIRE DE TACHES         \r\n"
"========================================\r\n"
"\r\n"
" Que souhaitez-vous faire ?\r\n"
"\r\n"
" 1. Ajouter une tâche\r\n"
" 2. Marquer une tâche comme terminée\r\n"
" 3. Afficher les tâches d'une date\r\n"
" 4. Afficher toutes les tâches\r\n"
" 5. Se déconnecter\r\n"
" 6. Quitter le programme\r\n"
"\r\n"
" Entrez un numéro (1-6) :\r\n> "
),
    case gen_tcp:recv(Sock, 0) of
        {ok, Bin} ->
            Line = string:trim(binary_to_list(Bin)),
            case string:tokens(Line, " ") of
                ["1"] -> %add
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

                ["2", DateStr | Toks] ->%done
                    Task = string:join(Toks, " "),
                    case todo_db:set_done(DateStr, Task) of
                        {atomic, ok} ->
                            gen_tcp:send(Sock, "Tâche marquée comme faite.\n");
                        {atomic, {error, not_found}} ->
                            gen_tcp:send(Sock, "Tâche non trouvée.\n")
                    end,
                    user_loop(Sock, Name);

                ["3", DateStr] -> %show
                    case todo_db:get_day_tasks(DateStr) of
                        {atomic, Tasks} ->
                            lists:foreach(fun(#todo{name=N, day=D, task=T, status=S}) ->
                                gen_tcp:send(Sock, io_lib:format("~s - ~s : ~s [~p]\n", [D, N, T, S]))
                            end, Tasks);
                        _ ->
                            gen_tcp:send(Sock, "Erreur lors de la récupération.\n")
                    end,
                    user_loop(Sock, Name);

                ["4"] -> %showall
                    case todo_db:get_all_tasks() of
                        {atomic, Tasks} ->
                            lists:foreach(fun(#todo{name=N, day=D, task=T, status=S}) ->
                                gen_tcp:send(Sock, io_lib:format("~s - ~s : ~s [~p]\n", [D, N, T, S]))
                            end, Tasks);
                        _ ->
                            gen_tcp:send(Sock, "Erreur lors de la récupération.\n")
                    end,
                    user_loop(Sock, Name);

                ["6"] -> %quit
                    gen_tcp:send(Sock, "Au revoir !\n"),
                    gen_tcp:close(Sock);

                ["5"] -> %logout
                    gen_tcp:send(Sock, "Retour a la page de connexion!\n"),
                    handle_client(Sock);

                _ ->
                    gen_tcp:send(Sock, "Commande invalide.\n"),
                    user_loop(Sock, Name)
            end;
        {error, closed} ->
            io:format("Déconnexion de ~s~n", [Name])
    end.
