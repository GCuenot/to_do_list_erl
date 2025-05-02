-module(db_manager).
-compile([export_all]).
-include("event.hrl").
-include("user.hrl").

-export([start/1, stop/0, add_event/1, update_event/2, delete_event/1, get_all_events/0,
         register_user/2, login_user/2, connect_to/1]).

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
            ]);
        
        join ->
            mnesia:start(),
            ok
    end.

stop() ->
    mnesia:stop().

connect_to(RemoteNode) ->
    net_adm:ping(RemoteNode),
    mnesia:change_config(extra_db_nodes, [RemoteNode]),
    [mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- [event, user]],
    ok.

%% Utilisateurs
register_user(Username, Password) ->
    User = #user{username = Username, password = Password},
    mnesia:transaction(fun() -> mnesia:write(User) end).

login_user(Username, Password) ->
    Fun = fun() ->
        case mnesia:read({user, Username}) of
            [#user{password = Password}] -> ok;
            _ -> error
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        _ -> error
    end.

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
