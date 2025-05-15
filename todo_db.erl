-module(todo_db).
-export([
    start/0, create_user/2, check_user/2,
    add_task/5, set_done/2, get_day_tasks/1,
    get_all_tasks/0
]).

-include("todo_db.hrl").

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
    ensure_table(user, record_info(fields, user)),
    ensure_table(todo, record_info(fields, todo)),
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

add_task(Name, Day, Month, Year, Task) ->
    case is_valid_date(Day, Month, Year) of
        true ->
            case is_past_date(Day, Month, Year) of
                true ->
                    {error, past_date};
                false ->
                    DateStr = format_date(Day, Month, Year),
                    ID = erlang:unique_integer([monotonic, positive]),
                    mnesia:transaction(fun() ->
                        mnesia:write(#todo{id = ID, name = Name, day = DateStr, task = Task, status = not_done})
                    end)
            end;
        false ->
            {error, invalid_date}
    end.

set_done(DayStr, Task) ->
    mnesia:transaction(fun() ->
        case mnesia:match_object(#todo{id = '_', name = '_', day = DayStr, task = Task, status = '_'}) of
            [T] ->
                mnesia:write(T#todo{status = done}),
                ok;
            [] -> {error, not_found}
        end
    end).

get_day_tasks(DayStr) ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#todo{id = '_', name = '_', day = DayStr, task = '_', status = '_'})
    end).

get_all_tasks() ->
    mnesia:transaction(fun() ->
        mnesia:match_object(#todo{id = '_', name = '_', day = '_', task = '_', status = '_'})
    end).

is_valid_date(Day, Month, Year) when
    Day >= 1, Day =< 31,
    Month >= 1, Month =< 12,
    Year >= 2025 ->
    true;
is_valid_date(_, _, _) ->
    false.

is_past_date(Day, Month, Year) ->
    {{CurrentYear, CurrentMonth, CurrentDay}, _} = calendar:local_time(),
    {Year, Month, Day} < {CurrentYear, CurrentMonth, CurrentDay}.

format_date(Day, Month, Year) ->
    lists:flatten(io_lib:format("~2..0B/~2..0B/~4..0B", [Day, Month, Year])).
