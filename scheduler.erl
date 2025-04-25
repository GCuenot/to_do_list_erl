-module(scheduler).
-export([menu/0]).

menu() ->
    io:format("~n--- MENU TO-DO LIST ---~n"),
    io:format("1. Ajouter un événement~n"),
    io:format("2. Modifier un événement~n"),
    io:format("3. Supprimer un événement~n"),
    io:format("4. Afficher tous les événements~n"),
    io:format("0. Quitter~n"),
    Input = io:get_line("Choix : "),
    case string:trim(Input) of
        "1" -> add_event(), menu();
        "2" -> update_event(), menu();
        "3" -> delete_event(), menu();
        "4" -> display_all(), menu();
        "0" -> io:format("Au revoir !~n");
        _   -> io:format("Choix invalide.~n"), menu()
    end.

add_event() ->
    Id = string:trim(io:get_line("Id : ")),
    Jour = string:trim(io:get_line("Jour : ")),
    Heure = string:trim(io:get_line("Heure : ")),
    Titre = string:trim(io:get_line("Titre : ")),
    Utilisateur = string:trim(io:get_line("Utilisateur : ")),
    db_manager:add_event({Id, Jour, Heure, Titre, Utilisateur}).

update_event() ->
    Id = string:trim(io:get_line("Id de l'événement à modifier : ")),
    Jour = string:trim(io:get_line("Nouveau jour : ")),
    Heure = string:trim(io:get_line("Nouvelle heure : ")),
    Titre = string:trim(io:get_line("Nouveau titre : ")),
    Utilisateur = string:trim(io:get_line("Nouvel utilisateur : ")),
    db_manager:update_event(Id, {Jour, Heure, Titre, Utilisateur}).

delete_event() ->
    Id = string:trim(io:get_line("Id de l'événement à supprimer : ")),
    db_manager:delete_event(Id).

display_all() ->
    Events = db_manager:get_all_events(),
    lists:foreach(fun(E) -> io:format("~p~n", [E]) end, Events).
