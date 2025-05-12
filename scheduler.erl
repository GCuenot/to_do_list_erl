-module(scheduler).
-export([start/0, menu/1]).

-include("event.hrl").

start() ->
    %%db_manager:start().
    auth_loop().

auth_loop() ->
    io:format("~n--- BIENVENUE SUR LA TODO LIST PARTAGÉE ---~n"),
    io:format("1. Se connecter~n"),
    io:format("2. S'inscrire~n"),
    io:format("0. Quitter~n"),
    Choice = string:trim(io:get_line("Choix : ")),
    case Choice of
        "1" -> login();
        "2" -> register();
        "0" -> io:format("À bientôt !~n"), db_manager:stop();
        _   -> io:format("Choix invalide.~n"), auth_loop()
    end.

login() ->
    Username = string:trim(io:get_line("Nom d'utilisateur : ")),
    Password = string:trim(io:get_line("Mot de passe : ")),
    case db_manager:login_user(Username, Password) of
        {atomic, {ok, Username}} ->
            io:format("Connexion réussie.~n"),
            menu(Username);
        {atomic, {error, Msg}} ->
            io:format("Erreur : ~s~n", [Msg]),
            auth_loop();
        {aborted, Reason} -> % Cas d'une erreur dans la transaction
            io:format("Échec de la connexion : ~s~n", [Reason]),
            auth_loop()
    end.

register() ->
    Username = string:trim(io:get_line("Choisissez un nom d'utilisateur : ")),
    Password = string:trim(io:get_line("Choisissez un mot de passe : ")),
    case db_manager:register_user(Username, Password) of
        {atomic, {ok, Msg}} ->
            io:format("~s~n", [Msg]),
            auth_loop();
        {atomic, {error, Msg}} ->
            io:format("Erreur : ~s~n", [Msg]),
            auth_loop();
        {aborted, Reason} -> % Cas d'une erreur dans la transaction
            io:format("Échec de l'inscription : ~s~n", [Reason]),
            auth_loop()
    end.


menu(Utilisateur) ->
    io:format("~n--- MENU TO-DO LIST ---~n"),
    io:format("Utilisateur : ~s~n", [Utilisateur]),
    io:format("1. Ajouter un événement~n"),
    io:format("2. Modifier un événement~n"),
    io:format("3. Supprimer un événement~n"),
    io:format("4. Afficher tous les événements~n"),
    io:format("0. Déconnexion~n"),
    Input = string:trim(io:get_line("Choix : ")),
    case Input of
        "1" -> add_event(Utilisateur), menu(Utilisateur);
        "2" -> update_event(Utilisateur), menu(Utilisateur);
        "3" -> delete_event(), menu(Utilisateur);
        "4" -> display_all(), menu(Utilisateur);
        "0" -> io:format("Déconnexion...~n"), auth_loop();
        _   -> io:format("Choix invalide.~n"), menu(Utilisateur)
    end.

add_event(Utilisateur) ->
    Id = string:trim(io:get_line("Id : ")),
    Jour = string:trim(io:get_line("Jour : ")),
    Heure = string:trim(io:get_line("Heure : ")),
    Titre = string:trim(io:get_line("Titre : ")),
    db_manager:add_event({Id, Jour, Heure, Titre, Utilisateur}).

update_event(Utilisateur) ->
    Id = string:trim(io:get_line("Id de l'événement à modifier : ")),
    Jour = string:trim(io:get_line("Nouveau jour : ")),
    Heure = string:trim(io:get_line("Nouvelle heure : ")),
    Titre = string:trim(io:get_line("Nouveau titre : ")),
    db_manager:update_event(Id, {Jour, Heure, Titre, Utilisateur}).

delete_event() ->
    Id = string:trim(io:get_line("Id de l'événement à supprimer : ")),
    db_manager:delete_event(Id).

display_all() ->
    Events = db_manager:get_all_events(),
    lists:foreach(fun(E) -> io:format("~p~n", [E]) end, Events).
