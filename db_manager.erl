-module(db_manager).
-export([start/0, stop/0, add_event/1, update_event/2, delete_event/1, get_all_events/0]).

-record(event, {id, jour, heure, titre, utilisateur}).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(event, [
        {attributes, record_info(fields, event)},
        {disc_copies, [node()]}
    ]).

stop() ->
    mnesia:stop().

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
