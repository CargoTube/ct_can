%%
%% Copyright (c) 2014-2017 Bas Wegh
%%
-module(ct_can_cargo_check).
-author("Bas Wegh").

%% API
-export([is_safe_cargo/1]).


-spec is_safe_cargo(map()) -> true | false.
is_safe_cargo(Msg) ->
    EntryList = maps:to_list(Msg),
    ValidFields = contains_valid_fields(Msg),
    Validate = fun(Entry, Boolean) ->
                       case Boolean of
                           false -> false;
                           true -> is_valid_entry(Entry)
                       end
               end,
    lists:foldl(Validate, ValidFields, EntryList).

is_valid_entry({type, Type}) ->
    is_valid_type(Type);
is_valid_entry({realm, Realm}) ->
    is_valid_uri(Realm);
is_valid_entry({topic, Topic}) ->
    is_valid_uri(Topic);
is_valid_entry({procedure, Procedure}) ->
    is_valid_uri(Procedure);
is_valid_entry({session_id, Id}) ->
    is_valid_id(Id);
is_valid_entry({request_id, Id}) ->
    is_valid_id(Id);
is_valid_entry({publication_id, Id}) ->
    is_valid_id(Id);
is_valid_entry({subscription_id, Id}) ->
    is_valid_id(Id);
is_valid_entry({registration_id, Id}) ->
    is_valid_id(Id);
is_valid_entry({request_type, RequestType}) ->
    is_valid_request_type(RequestType);
is_valid_entry({reason, Reason}) ->
    is_valid_uri(Reason);
is_valid_entry({error, Reason}) ->
    is_valid_uri(Reason);
is_valid_entry({details, Details}) ->
    is_valid_dict(Details);
is_valid_entry({options, Options}) ->
    is_valid_dict(Options);
is_valid_entry({auth_method, AuthMethod}) ->
    is_binary(AuthMethod) or is_atom(AuthMethod);
is_valid_entry({signature, Signature}) ->
    is_binary(Signature);
is_valid_entry({extra, Extra}) ->
    is_valid_dict(Extra);
is_valid_entry({payload, Payload}) ->
    is_binary(Payload);
is_valid_entry({arguments, Args}) ->
    is_valid_arguments(Args);
is_valid_entry({arguments_kw, ArgsKw}) ->
    is_valid_argumentskw(ArgsKw).


is_valid_type(Type) ->
    ValidTypes = [hello, welcome, abort, goodbye, error, publish, published,
                 subscribe, subscribed, unsubscribe, unsubscribed, event, call,
                 result, register, registered, unregister, unregistered,
                 invocation, yield, challenge, authenticate, cancel, interrupt,
                 ping, pong],
    lists:member(Type, ValidTypes).

is_valid_request_type(Type) ->
    ValidTypes = [publish, subscribe, unsubscribe, call, register, unregister,
                  invocation],
    lists:member(Type, ValidTypes).

is_valid_uri(Uri) when is_binary(Uri) ->
    UriParts = binary:split(Uri, <<".">>, [global]),
    CheckPart = fun(_Char, false) ->
                        false;
                   (Char, true) ->
                        is_valid_uri_part_character(Char)
                end,
    CheckParts = fun(_Part, false) ->
                         false;
                    (<<"">>, true) ->
                         false;
                    (Part, true) ->
                         Chars = binary_to_list(Part),
                         lists:foldl(CheckPart, true, Chars)
                 end,
    lists:foldl(CheckParts, true, UriParts);
is_valid_uri(_Uri) ->
    false.

is_valid_uri_part_character($a) ->
    true;
is_valid_uri_part_character($b) ->
    true;
is_valid_uri_part_character($c) ->
    true;
is_valid_uri_part_character($d) ->
    true;
is_valid_uri_part_character($e) ->
    true;
is_valid_uri_part_character($f) ->
    true;
is_valid_uri_part_character($g) ->
    true;
is_valid_uri_part_character($h) ->
    true;
is_valid_uri_part_character($i) ->
    true;
is_valid_uri_part_character($j) ->
    true;
is_valid_uri_part_character($k) ->
    true;
is_valid_uri_part_character($l) ->
    true;
is_valid_uri_part_character($m) ->
    true;
is_valid_uri_part_character($n) ->
    true;
is_valid_uri_part_character($o) ->
    true;
is_valid_uri_part_character($p) ->
    true;
is_valid_uri_part_character($q) ->
    true;
is_valid_uri_part_character($r) ->
    true;
is_valid_uri_part_character($s) ->
    true;
is_valid_uri_part_character($t) ->
    true;
is_valid_uri_part_character($u) ->
    true;
is_valid_uri_part_character($v) ->
    true;
is_valid_uri_part_character($w) ->
    true;
is_valid_uri_part_character($x) ->
    true;
is_valid_uri_part_character($y) ->
    true;
is_valid_uri_part_character($z) ->
    true;
is_valid_uri_part_character($0) ->
    true;
is_valid_uri_part_character($1) ->
    true;
is_valid_uri_part_character($2) ->
    true;
is_valid_uri_part_character($3) ->
    true;
is_valid_uri_part_character($4) ->
    true;
is_valid_uri_part_character($5) ->
    true;
is_valid_uri_part_character($6) ->
    true;
is_valid_uri_part_character($7) ->
    true;
is_valid_uri_part_character($8) ->
    true;
is_valid_uri_part_character($9) ->
    true;
is_valid_uri_part_character($_) ->
    true;
is_valid_uri_part_character(_) ->
    false.



is_valid_id(Id) when is_integer(Id), Id >= 0, Id < 9007199254740992 -> true;
is_valid_id(_) -> false.

is_valid_dict(Dict) when is_map(Dict) ->
    true;
is_valid_dict(_Dict) ->
    false.

is_valid_arguments(Arguments) when is_list(Arguments) -> true;
is_valid_arguments(_) -> false.

is_valid_argumentskw(ArgumentsKw) when is_map(ArgumentsKw) -> true;
is_valid_argumentskw(_) -> false.

-define(FIELD_MAPPING, [
                       {hello, [realm, details], []},
                       {welcome, [session_id, details], []},
                       {abort, [details, reason], []},
                       {goodbye, [details, reason], []},
                       {error, [request_type, request_id, details, error],
                        [arguments, arguments_kw]},
                       {publish, [request_id, options, topic],
                        [arguments, arguments_kw]},
                       {published, [request_id, publication_id], []},
                       {subscribe, [request_id, options, topic], []},
                       {subscribed, [request_id, subscription_id], []},
                       {unsubscribe, [request_id, subscription_id], []},
                       {unsubscribed, [request_id], []},
                       {event, [subscription_id, publication_id, details],
                        [arguments, arguments_kw]},
                       {call, [request_id, options, procedure],
                        [arguments, arguments_kw]},
                       {result, [request_id, details],
                        [arguments, arguments_kw]},
                       {register, [request_id, options, procedure], []},
                       {registered, [request_id, registration_id], []},
                       {unregister, [request_id, registration_id], []},
                       {unregistered, [request_id], []},
                       {invocation, [request_id, registration_id, details],
                        [arguments, arguments_kw]},
                       {yield, [request_id, options],
                        [arguments, arguments_kw]},
                       %% PING, PONG
                       {ping, [payload], []},
                       {pong, [payload], []},
                       %% ADVANCED MESSAGES
                       {challenge, [auth_method, extra], []},
                       {authenticate, [signature, extra], []},
                       {cancel, [request_id, options], []},
                       {interrupt, [request_id, options], []}
                      ]).



contains_valid_fields(#{type := Type} = Map) ->
    case lists:keyfind(Type, 1, ?FIELD_MAPPING) of
        {Type, MustKeys, MayKeys} ->
            validate_keys(Map, MustKeys, MayKeys);
        _ -> false
    end.


validate_keys(Map, MustKeys, MayKeys) ->
    KeyList = lists:subtract(maps:keys(Map), [type | MustKeys]),
    IsKey = fun(Key, Bool) ->
                case Bool of
                    false -> false;
                    true -> maps:is_key(Key, Map)
                end
            end,
    MustResult = lists:foldl(IsKey, true, MustKeys),
    DropKey = fun(Key, List) ->
                    lists:delete(Key, List)
              end,
    KeysLeft = lists:foldl(DropKey, KeyList, MayKeys),
    MustResult and (KeysLeft == []).
