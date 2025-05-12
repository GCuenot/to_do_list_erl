
-module(todo_db).
-export([start/0, create_user/2, check_user/2, add_task/3, set_done/3, get_day_tasks/2, get_all_tasks/1]).
-record(user, {name, password}).
-record(todo, {name, day, task, status}). % status = done | not_done

start() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok -> ok;
        {error, {already_exists, _}} -> ok;
        _ -> ok
    end,
    case application:start(mnesia) of
        ok -> ok;
        {error, {already_started, mnesia}} -> ok
    end,
    mnesia:wait_for_tables([schema], 5000),
    ensure_table(user, [name, password]),
    ensure_table(todo, [name, day, task, status]),
    mnesia:wait_for_tables([user, todo], 5000).


ensure_table(Table, Attributes) ->
    Node = node(),
    case mnesia:create_table(Table, [
        {attributes, Attributes},
        {disc_copies, [Node]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Other -> io:format("Erreur création table ~p: ~p~n", [Table, Other]), exit(Other)
    end.


create_user(Name, Password) ->
    mnesia:transaction(fun() ->
        case mnesia:read(user, Name) of
            [] -> mnesia:write(#user{name = Name, password = Password}), ok;
            _ -> {error, exists}
        end
    end).

check_user(Name, Password) ->
    mnesia:transaction(fun() ->
        case mnesia:read(user, Name) of
            [#user{password = Password}] -> ok;
            [_] -> {error, wrong_password};
            [] -> {error, not_found}
        end
    end).

add_task(Name, Day, Task) ->
    mnesia:transaction(fun() ->
        mnesia:write(#todo{name = Name, day = Day, task = Task, status = not_done})
    end).

set_done(Name, Day, Task) ->
    mnesia:transaction(fun() ->
        case mnesia:match_object(#todo{name = Name, day = Day, task = Task, status = '_'}) of
            [T] ->
                mnesia:write(T#todo{status = done}),
                ok;
            [] -> {error, not_found}
        end
    end).

get_day_tasks(Name, Day) ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#todo{name = Name, day = Day, task = '_', status = '_'})
    end).

get_all_tasks(Name) ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#todo{name = Name, day = '_', task = '_', status = '_'})
    end).
