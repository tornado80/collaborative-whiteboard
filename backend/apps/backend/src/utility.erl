-module(utility).

-export([new_uuid/0, is_valid_uuid/1]).

-spec new_uuid() -> binary().
new_uuid() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-spec is_valid_uuid(binary()) -> boolean().
is_valid_uuid(Uuid) when is_binary(Uuid) ->
    try
        uuid:is_v4(binary_to_list(Uuid))
    catch
        _Class:_Error -> false
    end.

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

is_valid_uuid_test_() ->
    [
        ?_assertEqual(true, is_valid_uuid(new_uuid())),
        ?_assertEqual(false, is_valid_uuid(<<"not a uuid">>))
    ].

-endif.