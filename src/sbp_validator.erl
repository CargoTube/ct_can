%%
%% Copyright (c) 2014-2016 Bas Wegh
%%
-module(sbp_validator).
-author("Bas Wegh, bwegh@github.com").

%% API
-export([is_valid_message/1]).

-spec is_valid_message(map()) -> true | false.
is_valid_message(Msg) ->
    EntryList = maps:to_list(Msg),
    Validate = fun(Entry, Boolean) ->
                       case Boolean of
                           false -> false;
                           true -> is_valid_entry(Entry)
                       end
               end,
    lists:foldl(Validate, true, EntryList).

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
is_valid_entry({arguments, Args}) ->
    is_valid_arguments(Args);
is_valid_entry({arguments_kw, ArgsKw}) ->
    is_valid_argumentskw(ArgsKw).


is_valid_type(Type) ->
    ValidTypes = [hello, welcome, abort, goodbye, error, publish, published,
                 subscribe, subscribed, unsubscribe, unsubscribed, event, call,
                 result, register, registered, unregister, unregistered,
                 invocation, yield],
    lists:member(Type, ValidTypes).

is_valid_request_type(Type) ->
    ValidTypes = [publish, subscribe, unsubscribe, call, register, unregister,
                  invocation],
    lists:member(Type, ValidTypes).
is_valid_uri(_Uri) ->
    true.

is_valid_id(Id) when is_integer(Id), Id >= 0, Id < 9007199254740992 -> true;
is_valid_id(_) -> false.

is_valid_dict(_Dict) ->
    true.

is_valid_arguments(Arguments) when is_list(Arguments) -> true;
is_valid_arguments(undefined) -> true;
is_valid_arguments(_) -> false.

is_valid_argumentskw(ArgumentsKw) when is_map(ArgumentsKw) -> true;
is_valid_argumentskw(undefined) -> true;
is_valid_argumentskw(_) -> false.

