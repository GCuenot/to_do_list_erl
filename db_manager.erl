-module(db_manager).
-compile([export_all]).
-include("event.hrl").
-include("user.hrl").

-export([
    start/1, stop/0, add_event/1, update_event/2, delete_event/1, get_all_events/0,
    register_user/2, login_user/2, connect_to/1
]).

start(Mode) ->
    case Mode of
        init ->
            mnesia:create_schema([node()]),
            mnesia:start(),
            mnesia:create_table(user, [
                {attributes, record_info(fields, user)},
                {disc_copies, [node()]}
            ]),
            mnesia:create_table(event, [
                {attributes, record_info(fields, event)},
                {disc_copies, [node()]}
            ]),
            ok;
        join ->
            mnesia:start(),
            ok
    end.

stop() ->
    mnesia:stop().

connect_to(RemoteNode) ->
    case net_adm:ping(RemoteNode) of
        pong ->
            io:format("Connexion réussie à ~p~n", [RemoteNode]),
            mnesia:change_config(extra_db_nodes, [RemoteNode]),
            [ensure_copy(Tab, RemoteNode) || Tab <- [user, event]],
            ok;
        pang ->
            io:format("Échec de connexion à ~p~n", [RemoteNode]),
            error
    end.

ensure_copy(Tab, RemoteNode) ->
    case mnesia:table_info(Tab, where_to_read) of
        undefined ->
            mnesia:add_table_copy(Tab, node(), disc_copies);
        Nodes when is_list(Nodes) ->
            case lists:member(node(), Nodes) of
                true -> ok;
                false -> mnesia:add_table_copy(Tab, node(), disc_copies)
            end
    end.

%% Utilisateurs
register_user(Username, Password) ->
    User = #user{username = Username, password = Password},
    Fun = fun() ->
        case mnesia:read({user, Username}) of
            [] -> mnesia:write(User), {ok, "Inscription réussie."};
            _ -> {error, "Nom d'utilisateur déjà utilisé."}
        end
    end,
    mnesia:transaction(Fun).

login_user(Username, Password) ->
    Fun = fun() ->
        case mnesia:read({user, Username}) of
            [#user{password = Password}] -> {ok, Username};
            [#user{}] -> {error, "Mot de passe incorrect."};
            [] -> {error, "Utilisateur introuvable."}
        end
    end,
    mnesia:transaction(Fun).

%% Événements
add_event({Id, Jour, Heure, Titre, Utilisateur}) ->
    Event = #event{id=Id, jour=Jour, heure=Heure, titre=Titre, utilisateur=Utilisateur},
    mnesia:transaction(fun() -> mnesia:write(Event) end).

update_event(Id, {Jour, Heure, Titre, Utilisateur}) ->
    Fun = fun() ->
        mnesia:write(#event{id=Id, jour=Jour, heure=Heure, titre=Titre, utilisateur=Utilisateur})
    end,
    mnesia:transaction(Fun).

delete_event(Id) ->
    mnesia:transaction(fun() -> mnesia:delete({event, Id}) end).

get_all_events() ->
    Fun = fun() ->
        mnesia:match_object(#event{_='_'})
    end,
    {atomic, Events} = mnesia:transaction(Fun),
    Events.
