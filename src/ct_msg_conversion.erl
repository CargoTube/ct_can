%%
%% Copyright (c) 2014-2017 Bas Wegh
%%
-module(ct_msg_conversion).
-author("Bas Wegh, bwegh@github.com").

-include("ct_msg_types.hrl").
-include("ct_msg_mapping.hrl").
-include("ct_msg_codes.hrl").


%% API
-export([to_wamp/1, to_internal/1]).


%% TODO:  heartbeat


-spec to_wamp(ErlWamp) -> list() when
      ErlWamp :: ct_msg().
to_wamp(Msg) ->
    true = ct_msg_validation:is_valid(Msg),
    msg_to_wamp(Msg).

-spec to_internal(WampMsg) -> ct_msg() when
      WampMsg :: list().
to_internal(WampMsg) ->
    ErlMsg = msg_to_internal(WampMsg),
    case ct_msg_validation:get_bad_fields(ErlMsg) of
        [] -> ErlMsg;
        BadFields ->
            {bad_fields, BadFields, ErlMsg}
    end.


-spec msg_to_wamp(Msg) -> list() when
      Msg :: ct_msg().
msg_to_wamp({hello, Realm, Details}) ->
    [?HELLO, Realm, Details];
msg_to_wamp({welcome, SessionId, Details}) ->
    [?WELCOME, SessionId, Details];
msg_to_wamp({abort, Details, Reason}) ->
    [?ABORT, Details, error_serialize(Reason)];
msg_to_wamp({goodbye, Details, Reason}) ->
    [?GOODBYE, Details, error_serialize(Reason)];
msg_to_wamp({error, AtomType, RequestId, Details, Error, Arguments,
             ArgumentsKw})
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_serialize(Error), Arguments,
     ArgumentsKw];
msg_to_wamp({error, AtomType, RequestId, Details, Error, Arguments, undefined})
  when is_list(Arguments), length(Arguments) > 0 ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_serialize(Error), Arguments];
msg_to_wamp({error, AtomType, RequestId, Details, Error, undefined,
             undefined}) ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_serialize(Error)];
msg_to_wamp({publish, RequestId, Options, Topic, Arguments, ArgumentsKw})
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    [?PUBLISH, RequestId, Options, Topic, Arguments, ArgumentsKw];
msg_to_wamp({publish, RequestId, Options, Topic, Arguments, undefined})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?PUBLISH, RequestId, Options, Topic, Arguments];
msg_to_wamp({publish, RequestId, Options, Topic, undefined, undefined}) ->
    [?PUBLISH, RequestId, Options, Topic];
msg_to_wamp({published, RequestId, PublicationId}) ->
    [?PUBLISHED, RequestId, PublicationId];
msg_to_wamp({subscribe, RequestId, Options, Topic}) ->
    [?SUBSCRIBE, RequestId, Options, Topic];
msg_to_wamp({subscribed, RequestId, SubscriptionId}) ->
    [?SUBSCRIBED, RequestId, SubscriptionId];
msg_to_wamp({unsubscribe, RequestId, SubscriptionId}) ->
    [?UNSUBSCRIBE, RequestId, SubscriptionId];
msg_to_wamp({unsubscribed, RequestId}) ->
    [?UNSUBSCRIBED, RequestId];
msg_to_wamp({event, SubscriptionId, PublicationId, Details, Arguments,
             ArgumentsKw})
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    [?EVENT, SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw];
msg_to_wamp({event, SubscriptionId, PublicationId, Details, Arguments,
             undefined}) when is_list(Arguments), length(Arguments) > 0 ->
    [?EVENT, SubscriptionId, PublicationId, Details, Arguments];
msg_to_wamp({event, SubscriptionId, PublicationId, Details, undefined,
             undefined}) ->
    [?EVENT, SubscriptionId, PublicationId, Details];
msg_to_wamp({call, RequestId, Options, Procedure, Arguments, ArgumentsKw} )
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    [?CALL, RequestId, Options, Procedure, Arguments, ArgumentsKw];
msg_to_wamp({call, RequestId, Options, Procedure, Arguments, undefined})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?CALL, RequestId, Options, Procedure, Arguments];
msg_to_wamp({call, RequestId, Options, Procedure, undefined, undefined}) ->
    [?CALL, RequestId, Options, Procedure];
msg_to_wamp({result, RequestId, Details, Arguments, ArgumentsKw})
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    [?RESULT, RequestId, Details, Arguments, ArgumentsKw];
msg_to_wamp({result, RequestId, Details, Arguments, undefined})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?RESULT, RequestId, Details, Arguments];
msg_to_wamp({result, RequestId, Details, undefined, undefined}) ->
    [?RESULT, RequestId, Details];
msg_to_wamp({register, RequestId, Options, Procedure}) ->
    [?REGISTER, RequestId, Options, Procedure];
msg_to_wamp({registered, RequestId, RegistrationId}) ->
    [?REGISTERED, RequestId, RegistrationId];
msg_to_wamp({unregister, RequestId, RegistrationId}) ->
    [?UNREGISTER, RequestId, RegistrationId];
msg_to_wamp({unregistered, RequestId}) ->
    [?UNREGISTERED, RequestId];
msg_to_wamp({invocation, RequestId, RegistrationId, Details, Arguments,
             ArgumentsKw})
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    [?INVOCATION, RequestId, RegistrationId, Details, Arguments, ArgumentsKw];
msg_to_wamp({invocation, RequestId, RegistrationId, Details, Arguments,
             undefined})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?INVOCATION, RequestId, RegistrationId, Details, Arguments];
msg_to_wamp({invocation, RequestId, RegistrationId, Details, undefined,
             undefined}) ->
    [?INVOCATION, RequestId, RegistrationId, Details];
msg_to_wamp({yield, RequestId, Options, Arguments, ArgumentsKw})
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    [?YIELD, RequestId, Options, Arguments, ArgumentsKw];
msg_to_wamp({yield, RequestId, Options, Arguments, undefined})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?YIELD, RequestId, Options, Arguments];
msg_to_wamp({yield, RequestId, Options, undefined, undefined}) ->
    [?YIELD, RequestId, Options];
%% ADVANCED MESSAGES
msg_to_wamp({challenge, AuthMethod, Extra}) ->
    [?CHALLENGE, authmethod_serialize(AuthMethod), Extra];
msg_to_wamp({authenticate, Signature, Extra}) ->
    [?AUTHENTICATE, Signature, Extra];
msg_to_wamp({cancel, RequestId, Options}) ->
    [?CANCEL, RequestId, Options];
msg_to_wamp({interrupt, RequestId, Options}) ->
    [?INTERRUPT, RequestId, Options].
%% msg_to_wamp({type := heartbeat, IncomingSeq, OutgoingSeq}) ->
%%     [?HEARTBEAT, IncomingSeq, OutgoingSeq];



-spec msg_to_internal(Msg) -> ct_msg() when
      Msg :: list().
msg_to_internal([?HELLO, Realm, Details]) ->
    {hello, Realm, dict_deserialize(Details)};
msg_to_internal([?WELCOME, SessionId, Details]) ->
    {welcome, SessionId, dict_deserialize(Details)};
msg_to_internal([?ABORT, Details, Reason]) ->
    {abort, dict_deserialize(Details), try_error_deserialize(Reason)};
msg_to_internal([?GOODBYE, Details, Reason]) ->
    {goodbye, dict_deserialize(Details), try_error_deserialize(Reason)};
msg_to_internal([?ERROR, RequestType, RequestId, Details, Error, Arguments,
            ArgumentsKw]) ->
    ErlType = request_type_to_atom(RequestType),
    {error, ErlType, RequestId, Details, try_error_deserialize(Error),
     Arguments, ArgumentsKw};
msg_to_internal([?ERROR, RequestType, RequestId, Details, Error, Arguments]) ->
    ErlType = request_type_to_atom(RequestType),
    {error, ErlType, RequestId, Details, try_error_deserialize(Error),
     Arguments, undefined};
msg_to_internal([?ERROR, RequestType, RequestId, Details, Error]) ->
    ErlType = request_type_to_atom(RequestType),
    {error, ErlType, RequestId, Details, try_error_deserialize(Error),
     undefined, undefined};
msg_to_internal([?PUBLISH, RequestId, Options, Topic]) ->
    {publish,  RequestId, dict_deserialize(Options), Topic, undefined,
     undefined};
msg_to_internal([?PUBLISH, RequestId, Options, Topic, Arguments]) ->
    {publish,  RequestId, dict_deserialize(Options), Topic, Arguments,
     undefined};
msg_to_internal([?PUBLISH, RequestId, Options, Topic, Arguments, ArgumentsKw])->
    {publish,  RequestId, dict_deserialize(Options), Topic, Arguments,
     ArgumentsKw};
msg_to_internal([?PUBLISHED, RequestId, PublicationId]) ->
    {published, RequestId, PublicationId};
msg_to_internal([?SUBSCRIBE, RequestId, Options, Topic]) ->
    {subscribe, RequestId, dict_deserialize(Options), Topic};
msg_to_internal([?SUBSCRIBED, RequestId, SubscriptionId]) ->
    {subscribed, RequestId, SubscriptionId};
msg_to_internal([?UNSUBSCRIBE, RequestId, SubscriptionId]) ->
    {unsubscribe, RequestId, SubscriptionId};
msg_to_internal([?UNSUBSCRIBED, RequestId]) ->
    {unsubscribed, RequestId};
msg_to_internal([?EVENT, SubscriptionId, PublicationId, Details]) ->
    {event, SubscriptionId, PublicationId, dict_deserialize(Details),
     undefined, undefined};
msg_to_internal([?EVENT, SubscriptionId, PublicationId, Details, Arguments]) ->
    {event, SubscriptionId, PublicationId, dict_deserialize(Details),
     Arguments, undefined};
msg_to_internal([?EVENT, SubscriptionId, PublicationId, Details, Arguments,
        ArgumentsKw]) ->
    {event, SubscriptionId, PublicationId, dict_deserialize(Details), Arguments,
     ArgumentsKw};
msg_to_internal([?CALL, RequestId, Options, Procedure]) ->
    {call, RequestId, dict_deserialize(Options), Procedure, undefined,
     undefined};
msg_to_internal([?CALL, RequestId, Options, Procedure, Arguments]) ->
    {call, RequestId, dict_deserialize(Options), Procedure,
     value_deserialize(Arguments), undefined};
msg_to_internal([?CALL, RequestId, Options, Procedure, Arguments,
                 ArgumentsKw]) ->
    {call, RequestId, dict_deserialize(Options), Procedure,
     value_deserialize(Arguments), value_deserialize(ArgumentsKw)};
msg_to_internal([?RESULT, RequestId, Details]) ->
    {result, RequestId, dict_deserialize(Details), undefined, undefined};
msg_to_internal([?RESULT, RequestId, Details, Arguments]) ->
    {result, RequestId, dict_deserialize(Details), Arguments, undefined};
msg_to_internal([?RESULT, RequestId, Details, Arguments, ArgumentsKw]) ->
    {result, RequestId, dict_deserialize(Details), Arguments, ArgumentsKw};
msg_to_internal([?REGISTER, RequestId, Options, Procedure]) ->
    {register, RequestId, dict_deserialize(Options), Procedure};
msg_to_internal([?REGISTERED, RequestId, RegistrationId]) ->
    {registered, RequestId, RegistrationId};
msg_to_internal([?UNREGISTER, RequestId, RegistrationId]) ->
    {unregister, RequestId, RegistrationId};
msg_to_internal([?UNREGISTERED, RequestId]) ->
    {unregistered, RequestId};
msg_to_internal([?INVOCATION, RequestId, RegistrationId, Details]) ->
    {invocation, RequestId, RegistrationId, dict_deserialize(Details),
     undefined, undefined};
msg_to_internal([?INVOCATION, RequestId, RegistrationId, Details, Arguments]) ->
    {invocation, RequestId, RegistrationId, dict_deserialize(Details),
      Arguments, undefined};
msg_to_internal([?INVOCATION, RequestId, RegistrationId, Details, Arguments,
            ArgumentsKw]) ->
    {invocation, RequestId, RegistrationId, dict_deserialize(Details),
     Arguments, ArgumentsKw};
msg_to_internal([?YIELD, RequestId, Options]) ->
    {yield, RequestId, dict_deserialize(Options), undefined, undefined};
msg_to_internal([?YIELD, RequestId, Options, Arguments]) ->
    {yield, RequestId, dict_deserialize(Options), Arguments, undefined};
msg_to_internal([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
    {yield, RequestId, dict_deserialize(Options), Arguments, ArgumentsKw};
%% ADVANCED MESSAGES
msg_to_internal([?CHALLENGE, AuthMethod, Extra]) ->
    {challenge, authmethod_deserialize(AuthMethod), dict_deserialize(Extra)};
msg_to_internal([?AUTHENTICATE, Signature, Extra]) ->
    {authenticate, Signature, dict_deserialize(Extra)};
msg_to_internal([?CANCEL, RequestId, Options]) ->
    {cancel, RequestId, dict_deserialize(Options)};
msg_to_internal([?INTERRUPT, RequestId, Options]) ->
    {interrupt, RequestId, dict_deserialize(Options)};
msg_to_internal({ping, _} = Ping) ->
    Ping;
msg_to_internal({pong, _} = Pong) ->
    Pong.
%% msg_to_internal([?HEARTBEAT, IncomingSeq, OutgoingSeq, _Discard]) ->
%%     msg_to_internal([?HEARTBEAT, IncomingSeq, OutgoingSeq]);
%% msg_to_internal([?HEARTBEAT, IncomingSeq, OutgoingSeq]) ->
%%     {heartbeat, sequence_in => IncomingSeq,
%%       sequence_out => OutgoingSeq};


-spec try_error_deserialize(Error) -> binary() | atom() when
      Error :: binary().
try_error_deserialize(Error) ->
    convert_value(deserialize, Error, ?ERROR_MAPPING).


-spec error_serialize(Error) -> binary() when
      Error :: binary() | atom().
error_serialize(Error) ->
    convert_value(serialize, Error, ?ERROR_MAPPING).


-spec dict_deserialize(Dict)  -> map() when
      Dict :: map().
dict_deserialize(Dict) ->
    value_deserialize(Dict).


-spec value_deserialize(Value) -> map() | list() | number() | binary() when
      Value :: map() | list() | number() | binary() | atom().
value_deserialize(Map) when is_map (Map) ->
    PropList = maps:to_list(Map),
    Convert = fun({Key, Value}, NewMap) ->
                NewKey = try_to_atom(Key),
                NewValue = value_deserialize(Value),
                maps:put(NewKey, NewValue, NewMap)
              end,
    lists:foldl(Convert, #{}, PropList);
value_deserialize(Atom) when is_atom(Atom) ->
    Atom;
value_deserialize(Binary) when is_binary(Binary) ->
    try_to_atom(Binary);
value_deserialize(List) when is_list(List) ->
    Convert = fun(Element, NewList) ->
                      NewElement = value_deserialize(Element),
                      [NewElement | NewList]
              end,
    lists:reverse(lists:foldl(Convert, [], List));
value_deserialize(N) when is_number(N) ->
    N.


-spec try_to_atom(Binary) -> binary() | atom() when
      Binary :: binary().
try_to_atom(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom -> Atom
    catch _:_ ->
             Binary
    end.


-spec authmethod_deserialize(Method) -> binary() | atom() when
      Method :: binary().
authmethod_deserialize(Method) ->
    convert_value(deserialize, Method, ?AUTH_METHOD_MAPPING).

-spec authmethod_serialize(Method) -> binary() when
      Method :: atom().
authmethod_serialize(Method) ->
    convert_value(serialize, Method, ?AUTH_METHOD_MAPPING).

-spec request_type_to_atom(RequestType) -> atom() when
      RequestType :: pos_integer().
request_type_to_atom(RequestType) ->
    {RequestType, Atom} = lists:keyfind(RequestType, 1, ?REQUEST_TYPE_MAPPING),
    Atom.

-spec atom_to_request_type(Atom) -> pos_integer() when
      Atom :: ct_msg_type().
atom_to_request_type(Atom) ->
    {RequestType, Atom} = lists:keyfind(Atom, 2, ?REQUEST_TYPE_MAPPING),
    RequestType.


-spec convert_value(Direction, Value, Mapping) ->  any() when
      Direction :: deserialize | serialize,
      Value :: any(),
      Mapping :: list().
convert_value(Direction, Value, Mapping) ->
    ValPos = value_pos(Direction),
    Found = lists:keyfind(Value, ValPos, Mapping),
    ValueTuple = safe_convert_value(Found, Value),
    convert(Direction, ValueTuple).

safe_convert_value({EV, WV}, _Default) ->
    {EV, WV};
safe_convert_value(false, Default) ->
    {Default, Default}.


-spec convert(Direction, Tuple) -> any() when
      Direction :: deserialize | serialize,
      Tuple :: {atom(), any()}.
convert(deserialize, {ErlVal, _}) ->
    ErlVal;
convert(serialize, {_, WampVal}) ->
    WampVal.

-spec value_pos(Direction) -> 1 | 2 when
      Direction :: deserialize | serialize.
value_pos(deserialize) ->
    2;
value_pos(serialize) ->
    1.
