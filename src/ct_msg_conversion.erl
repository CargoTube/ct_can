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
to_wamp(Msg0) ->
    {true, Msg} = ct_msg_validation:enforce_valid(Msg0),
    msg_to_wamp(Msg).

-spec to_internal(WampMsg) -> ct_msg() when
      WampMsg :: list().
to_internal(WampMsg) ->
    ErlMsg = msg_to_internal(WampMsg),
    case ct_msg_validation:get_bad_fields(ErlMsg) of
        [] -> ErlMsg;
        BadCargoList -> {bad_cargo, BadCargoList}
    end.


-spec msg_to_wamp(Msg) -> list() when
      Msg :: ct_msg().
msg_to_wamp(#{type := hello, realm := Realm, details := Details}) ->
    [?HELLO, Realm, Details];
msg_to_wamp(#{type := welcome, session_id := SessionId, details := Details}) ->
    [?WELCOME, SessionId, Details];
msg_to_wamp(#{type := abort, details := Details, reason := Reason}) ->
    [?ABORT, Details, error_unload(Reason)];
msg_to_wamp(#{type := goodbye, details := Details, reason := Reason}) ->
    [?GOODBYE, Details, error_unload(Reason)];
msg_to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error,
          arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    WampType = atom_to_request_type(AtomType),
    Arguments = maps:get(arguments, Msg, []),
    [?ERROR, WampType, RequestId, Details, error_unload(Error), Arguments,
     ArgumentsKw];
msg_to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_unload(Error), Arguments];
msg_to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error}) ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_unload(Error)];
msg_to_wamp(#{type := publish, request_id := RequestId, options := Options,
              topic := Topic, arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?PUBLISH, RequestId, Options, Topic, Arguments,
     ArgumentsKw];
msg_to_wamp(#{type := publish, request_id := RequestId, options := Options,
              topic := Topic, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?PUBLISH, RequestId, Options, Topic, Arguments];
msg_to_wamp(#{type := publish, request_id := RequestId, options := Options,
              topic := Topic}) ->
    [?PUBLISH, RequestId, Options, Topic];
msg_to_wamp(#{type := published, request_id := RequestId,
              publication_id := PublicationId}) ->
    [?PUBLISHED, RequestId, PublicationId];
msg_to_wamp(#{type := subscribe, request_id := RequestId, options := Options,
          topic := Topic}) ->
    [?SUBSCRIBE, RequestId, Options, Topic];
msg_to_wamp(#{type := subscribed, request_id := RequestId,
          subscription_id := SubscriptionId}) ->
    [?SUBSCRIBED, RequestId, SubscriptionId];
msg_to_wamp(#{type := unsubscribe, request_id := RequestId,
          subscription_id := SubscriptionId}) ->
    [?UNSUBSCRIBE, RequestId, SubscriptionId];
msg_to_wamp(#{type := unsubscribed, request_id := RequestId}) ->
    [?UNSUBSCRIBED, RequestId];
msg_to_wamp(#{type := event, subscription_id := SubscriptionId,
          publication_id := PublicationId, details := Details,
          arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?EVENT, SubscriptionId, PublicationId, Details, Arguments,
     ArgumentsKw];
msg_to_wamp(#{type := event, subscription_id := SubscriptionId,
          publication_id := PublicationId, details := Details,
          arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?EVENT, SubscriptionId, PublicationId, Details, Arguments];
msg_to_wamp(#{type := event, subscription_id := SubscriptionId,
          publication_id := PublicationId, details := Details}) ->
    [?EVENT, SubscriptionId, PublicationId, Details];
msg_to_wamp(#{type := call, request_id := RequestId, options := Options,
          procedure := Procedure, arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?CALL, RequestId, Options, Procedure, Arguments,
     ArgumentsKw];
msg_to_wamp(#{type := call, request_id := RequestId, options := Options,
          procedure := Procedure, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?CALL, RequestId, Options, Procedure, Arguments];
msg_to_wamp(#{type := call, request_id := RequestId, options := Options,
          procedure := Procedure}) ->
    [?CALL, RequestId, Options, Procedure];
msg_to_wamp(#{type := result, request_id := RequestId, details := Details,
         arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?RESULT, RequestId, Details, Arguments, ArgumentsKw];
msg_to_wamp(#{type := result, request_id := RequestId, details := Details,
         arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?RESULT, RequestId, Details, Arguments];
msg_to_wamp(#{type := result, request_id := RequestId, details := Details}) ->
    [?RESULT, RequestId, Details];
msg_to_wamp(#{type := register, request_id := RequestId, options := Options,
          procedure := Procedure}) ->
    [?REGISTER, RequestId, Options, Procedure];
msg_to_wamp(#{type := registered, request_id := RequestId,
          registration_id := RegistrationId}) ->
    [?REGISTERED, RequestId, RegistrationId];
msg_to_wamp(#{type := unregister, request_id := RequestId,
          registration_id := RegistrationId}) ->
    [?UNREGISTER, RequestId, RegistrationId];
msg_to_wamp(#{type := unregistered, request_id := RequestId}) ->
    [?UNREGISTERED, RequestId];
msg_to_wamp(#{type := invocation, request_id := RequestId,
         registration_id := RegistrationId, details := Details,
         arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?INVOCATION, RequestId, RegistrationId, Details, Arguments,
    ArgumentsKw];
msg_to_wamp(#{type := invocation, request_id := RequestId,
         registration_id := RegistrationId, details := Details,
         arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?INVOCATION, RequestId, RegistrationId, Details, Arguments];
msg_to_wamp(#{type := invocation, request_id := RequestId,
         registration_id := RegistrationId, details := Details}) ->
    [?INVOCATION, RequestId, RegistrationId, Details];
msg_to_wamp(#{type := yield, request_id := RequestId, options := Options,
         arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?YIELD, RequestId, Options, Arguments, ArgumentsKw];
msg_to_wamp(#{type := yield, request_id := RequestId, options := Options,
         arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?YIELD, RequestId, Options, Arguments];
msg_to_wamp(#{type := yield, request_id := RequestId, options := Options}) ->
    [?YIELD, RequestId, Options];
%% ADVANCED MESSAGES
msg_to_wamp(#{type := challenge, auth_method := AuthMethod, extra := Extra}) ->
    [?CHALLENGE, authmethod_unload(AuthMethod), Extra];
msg_to_wamp(#{type := authenticate, signature := Signature, extra := Extra}) ->
    [?AUTHENTICATE, Signature, Extra];
msg_to_wamp(#{type := cancel, request_id := RequestId, options := Options}) ->
    [?CANCEL, RequestId, Options];
msg_to_wamp(#{type := interrupt, request_id := RequestId,
              options := Options}) ->
    [?INTERRUPT, RequestId, Options].
%% msg_to_wamp({type := heartbeat, IncomingSeq, OutgoingSeq}) ->
%%     [?HEARTBEAT, IncomingSeq, OutgoingSeq];



-spec msg_to_internal(Msg) -> ct_msg() when
      Msg :: list().
msg_to_internal([?HELLO, Realm, Details]) ->
    #{type => hello, realm => Realm, details => dict_load(Details)};
msg_to_internal([?WELCOME, SessionId, Details]) ->
    #{type => welcome, session_id => SessionId,
      details => dict_load(Details)};
msg_to_internal([?ABORT, Details, Reason]) ->
    #{type => abort, details => dict_load(Details),
      reason => try_error_load(Reason)};
msg_to_internal([?GOODBYE, Details, Reason]) ->
    #{type => goodbye, details => dict_load(Details),
      reason => try_error_load(Reason)};
msg_to_internal([?ERROR, RequestType, RequestId, Details, Error, Arguments,
            ArgumentsKw]) ->
    ErlType = request_type_to_atom(RequestType),
    #{type => error, request_type => ErlType, request_id => RequestId,
      details => Details, error => try_error_load(Error),
     arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_internal([?ERROR, RequestType, RequestId, Details, Error, Arguments]) ->
    ErlType = request_type_to_atom(RequestType),
    #{type => error, request_type => ErlType, request_id => RequestId,
      details => Details, error => try_error_load(Error),
     arguments => Arguments};
msg_to_internal([?ERROR, RequestType, RequestId, Details, Error]) ->
    ErlType = request_type_to_atom(RequestType),
    #{type => error, request_type => ErlType, request_id => RequestId,
      details => Details, error => try_error_load(Error)};
msg_to_internal([?PUBLISH, RequestId, Options, Topic]) ->
    #{type => publish,  request_id => RequestId,
      options => dict_load(Options), topic => Topic};
msg_to_internal([?PUBLISH, RequestId, Options, Topic, Arguments]) ->
    #{type => publish,  request_id => RequestId,
      options => dict_load(Options), topic => Topic, arguments => Arguments};
msg_to_internal([?PUBLISH, RequestId, Options, Topic, Arguments, ArgumentsKw])->
    #{type => publish,  request_id => RequestId,
      options => dict_load(Options), topic => Topic, arguments => Arguments,
      arguments_kw => ArgumentsKw};
msg_to_internal([?PUBLISHED, RequestId, PublicationId]) ->
    #{type => published, request_id => RequestId,
      publication_id => PublicationId};
msg_to_internal([?SUBSCRIBE, RequestId, Options, Topic]) ->
    #{type => subscribe, request_id => RequestId,
      options => dict_load(Options), topic => Topic};
msg_to_internal([?SUBSCRIBED, RequestId, SubscriptionId]) ->
    #{type => subscribed, request_id => RequestId,
      subscription_id => SubscriptionId};
msg_to_internal([?UNSUBSCRIBE, RequestId, SubscriptionId]) ->
    #{type => unsubscribe, request_id => RequestId,
      subscription_id => SubscriptionId};
msg_to_internal([?UNSUBSCRIBED, RequestId]) ->
    #{type => unsubscribed, request_id => RequestId};
msg_to_internal([?EVENT, SubscriptionId, PublicationId, Details]) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => dict_load(Details)};
msg_to_internal([?EVENT, SubscriptionId, PublicationId, Details, Arguments]) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => dict_load(Details),
      arguments => Arguments};
msg_to_internal([?EVENT, SubscriptionId, PublicationId, Details, Arguments,
        ArgumentsKw]) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => dict_load(Details),
      arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_internal([?CALL, RequestId, Options, Procedure]) ->
    #{type => call, request_id => RequestId, options => dict_load(Options),
      procedure => Procedure};
msg_to_internal([?CALL, RequestId, Options, Procedure, Arguments]) ->
    #{type => call, request_id => RequestId, options => dict_load(Options),
      procedure => Procedure, arguments => Arguments};
msg_to_internal([?CALL, RequestId, Options, Procedure, Arguments,
                 ArgumentsKw]) ->
    #{type => call, request_id => RequestId, options => dict_load(Options),
      procedure => Procedure, arguments => Arguments,
      arguments_kw => ArgumentsKw};
msg_to_internal([?RESULT, RequestId, Details]) ->
    #{type => result, request_id => RequestId, details => dict_load(Details)};
msg_to_internal([?RESULT, RequestId, Details, Arguments]) ->
    #{type => result, request_id => RequestId, details => dict_load(Details),
      arguments => Arguments};
msg_to_internal([?RESULT, RequestId, Details, Arguments, ArgumentsKw]) ->
    #{type => result, request_id => RequestId, details => dict_load(Details),
      arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_internal([?REGISTER, RequestId, Options, Procedure]) ->
    #{type => register, request_id => RequestId,
      options => dict_load(Options), procedure => Procedure};
msg_to_internal([?REGISTERED, RequestId, RegistrationId]) ->
    #{type => registered, request_id => RequestId,
      registration_id => RegistrationId};
msg_to_internal([?UNREGISTER, RequestId, RegistrationId]) ->
    #{type => unregister, request_id => RequestId,
      registration_id => RegistrationId};
msg_to_internal([?UNREGISTERED, RequestId]) ->
    #{type => unregistered, request_id => RequestId};
msg_to_internal([?INVOCATION, RequestId, RegistrationId, Details]) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => dict_load(Details)};
msg_to_internal([?INVOCATION, RequestId, RegistrationId, Details, Arguments]) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => dict_load(Details),
      arguments => Arguments};
msg_to_internal([?INVOCATION, RequestId, RegistrationId, Details, Arguments,
            ArgumentsKw]) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => dict_load(Details),
      arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_internal([?YIELD, RequestId, Options]) ->
    #{type => yield, request_id => RequestId, options => dict_load(Options)};
msg_to_internal([?YIELD, RequestId, Options, Arguments]) ->
    #{type => yield, request_id => RequestId, options => dict_load(Options),
      arguments => Arguments};
msg_to_internal([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
    #{type => yield, request_id => RequestId, options => dict_load(Options),
      arguments => Arguments, arguments_kw => ArgumentsKw};
%% ADVANCED MESSAGES
msg_to_internal([?CHALLENGE, AuthMethod, Extra]) ->
    #{type => challenge, auth_method => authmethod_load(AuthMethod),
      extra => dict_load(Extra)};
msg_to_internal([?AUTHENTICATE, Signature, Extra]) ->
    #{type => authenticate, signature => Signature,
      extra => dict_load(Extra)};
msg_to_internal([?CANCEL, RequestId, Options]) ->
    #{type => cancel, request_id => RequestId, options => dict_load(Options)};
msg_to_internal([?INTERRUPT, RequestId, Options]) ->
    #{type => interrupt, request_id => RequestId,
      options => dict_load(Options)};
msg_to_internal(#{type := ping} = Ping) ->
    Ping;
msg_to_internal(#{type := pong} = Pong) ->
    Pong.
%% msg_to_internal([?HEARTBEAT, IncomingSeq, OutgoingSeq, _Discard]) ->
%%     msg_to_internal([?HEARTBEAT, IncomingSeq, OutgoingSeq]);
%% msg_to_internal([?HEARTBEAT, IncomingSeq, OutgoingSeq]) ->
%%     #{type => heartbeat, sequence_in => IncomingSeq,
%%       sequence_out => OutgoingSeq};


-spec try_error_load(Error) -> binary() | atom() when
      Error :: binary().
try_error_load(Error) ->
    convert_value(load, Error, ?ERROR_MAPPING).


-spec error_unload(Error) -> binary() when
      Error :: binary() | atom().
error_unload(Error) ->
    convert_value(unload, Error, ?ERROR_MAPPING).


-spec dict_load(Dict)  -> map() when
      Dict :: map().
dict_load(Dict) ->
    value_load(Dict).


-spec value_load(Value) -> map() | list() | number() | binary() when
      Value :: map() | list() | number() | binary() | atom().
value_load(Map) when is_map (Map) ->
    PropList = maps:to_list(Map),
    Convert = fun({Key, Value}, NewMap) ->
                NewKey = try_to_atom(Key),
                NewValue = value_load(Value),
                maps:put(NewKey, NewValue, NewMap)
              end,
    lists:foldl(Convert, #{}, PropList);
value_load(Atom) when is_atom(Atom) ->
    Atom;
value_load(Binary) when is_binary(Binary) ->
    try_to_atom(Binary);
value_load(List) when is_list(List) ->
    Convert = fun(Element, NewList) ->
                      NewElement = value_load(Element),
                      [NewElement | NewList]
              end,
    lists:reverse(lists:foldl(Convert, [], List));
value_load(N) when is_number(N) ->
    N.


-spec try_to_atom(Binary) -> binary() | atom() when
      Binary :: binary().
try_to_atom(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom -> Atom
    catch _:_ ->
             Binary
    end.


-spec authmethod_load(Method) -> binary() | atom() when
      Method :: binary().
authmethod_load(Method) ->
    convert_value(load, Method, ?AUTH_METHOD_MAPPING).

-spec authmethod_unload(Method) -> binary() when
      Method :: atom().
authmethod_unload(Method) ->
    convert_value(unload, Method, ?AUTH_METHOD_MAPPING).

-spec request_type_to_atom(RequestType) -> atom() when
      RequestType :: pos_integer().
request_type_to_atom(RequestType) ->
    {RequestType, Atom} = lists:keyfind(RequestType, 1, ?REQUEST_TYPE_MAPPING),
    Atom.

-spec atom_to_request_type(Atom) -> pos_integer() when
      Atom :: atom().
atom_to_request_type(Atom) ->
    {RequestType, Atom} = lists:keyfind(Atom, 2, ?REQUEST_TYPE_MAPPING),
    RequestType.



-spec convert_value(Direction, Value, Mapping) ->  any() when
      Direction :: load | unload,
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
      Direction :: load | unload,
      Tuple :: {atom(), any()}.
convert(load, {ErlVal, _}) ->
    ErlVal;
convert(unload, {_, WampVal}) ->
    WampVal.

-spec value_pos(Direction) -> 1 | 2 when
      Direction :: load | unload.
value_pos(load) ->
    2;
value_pos(unload) ->
    1.
