%%
%% Copyright (c) 2014-2016 Bas Wegh
%%
-module(sibo_proto_converter).
-author("Bas Wegh, bwegh@github.com").

-include("sibo_proto_mapping.hrl").
-include("sibo_proto_message_codes.hrl").


%% API
-export([to_wamp/1, to_erl/1]).


%% TODO:  heartbeat

to_wamp(ErlWamp) ->
    true = sibo_proto_validator:is_valid_message(ErlWamp),
    msg_to_wamp(ErlWamp).

to_erl(WampMsg) ->
    ErlMsg = msg_to_erl(WampMsg),
    true = sibo_proto_validator:is_valid_message(ErlMsg),
    ErlMsg.

msg_to_wamp(#{type := hello, realm := Realm, details := Details}) ->
    [?HELLO, Realm, Details];
msg_to_wamp(#{type := welcome, session_id := SessionId, details := Details}) ->
    [?WELCOME, SessionId, Details];
msg_to_wamp(#{type := abort, details := Details, reason := Reason}) ->
    [?ABORT, Details, error_to_wamp(Reason)];
msg_to_wamp(#{type := goodbye, details := Details, reason := Reason}) ->
    [?GOODBYE, Details, error_to_wamp(Reason)];
msg_to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error,
          arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    WampType = atom_to_request_type(AtomType),
    Arguments = maps:get(arguments, Msg, []),
    [?ERROR, WampType, RequestId, Details, error_to_wamp(Error), Arguments,
     ArgumentsKw];
msg_to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_to_wamp(Error), Arguments];
msg_to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error}) ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_to_wamp(Error)];
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
msg_to_wamp({publish, RequestId, Options, Topic, Arguments, ArgumentsKw}) ->
    [?PUBLISH, RequestId, Options, Topic, Arguments, ArgumentsKw];
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
    [?CHALLENGE, authmethod_to_wamp(AuthMethod), Extra];
msg_to_wamp(#{type := authenticate, signature := Signature, extra := Extra}) ->
    [?AUTHENTICATE, Signature, Extra];
msg_to_wamp(#{type := cancel, request_id := RequestId, options := Options}) ->
    [?CANCEL, RequestId, Options];
msg_to_wamp(#{type := interrupt, request_id := RequestId,
              options := Options}) ->
    [?INTERRUPT, RequestId, Options].
%% msg_to_wamp({type := heartbeat, IncomingSeq, OutgoingSeq}) ->
%%     [?HEARTBEAT, IncomingSeq, OutgoingSeq];



msg_to_erl([?HELLO, Realm, Details]) ->
    #{type => hello, realm => Realm, details => dict_to_erl(Details)};
msg_to_erl([?WELCOME, SessionId, Details]) ->
    #{type => welcome, session_id => SessionId,
      details => dict_to_erl(Details)};
msg_to_erl([?ABORT, Details, Reason]) ->
    #{type => abort, details => dict_to_erl(Details),
      reason => try_error_to_erl(Reason)};
msg_to_erl([?GOODBYE, Details, Reason]) ->
    #{type => goodbye, details => dict_to_erl(Details),
      reason => try_error_to_erl(Reason)};
msg_to_erl([?ERROR, RequestType, RequestId, Details, Error, Arguments,
            ArgumentsKw]) ->
    ErlType = request_type_to_atom(RequestType),
    #{type => error, request_type => ErlType, request_id => RequestId,
      details => Details, error => try_error_to_erl(Error),
     arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_erl([?ERROR, RequestType, RequestId, Details, Error, Arguments]) ->
    ErlType = request_type_to_atom(RequestType),
    #{type => error, request_type => ErlType, request_id => RequestId,
      details => Details, error => try_error_to_erl(Error),
     arguments => Arguments};
msg_to_erl([?ERROR, RequestType, RequestId, Details, Error]) ->
    ErlType = request_type_to_atom(RequestType),
    #{type => error, request_type => ErlType, request_id => RequestId,
      details => Details, error => try_error_to_erl(Error)};
msg_to_erl([?PUBLISH, RequestId, Options, Topic]) ->
    #{type => publish,  request_id => RequestId,
      options => dict_to_erl(Options), topic => Topic};
msg_to_erl([?PUBLISH, RequestId, Options, Topic, Arguments]) ->
    #{type => publish,  request_id => RequestId,
      options => dict_to_erl(Options), topic => Topic, arguments => Arguments};
msg_to_erl([?PUBLISH, RequestId, Options, Topic, Arguments, ArgumentsKw]) ->
    #{type => publish,  request_id => RequestId,
      options => dict_to_erl(Options), topic => Topic, arguments => Arguments,
      arguments_kw => ArgumentsKw};
msg_to_erl([?PUBLISHED, RequestId, PublicationId]) ->
    #{type => published, request_id => RequestId,
      publication_id => PublicationId};
msg_to_erl([?SUBSCRIBE, RequestId, Options, Topic]) ->
    #{type => subscribe, request_id => RequestId,
      options => dict_to_erl(Options), topic => Topic};
msg_to_erl([?SUBSCRIBED, RequestId, SubscriptionId]) ->
    #{type => subscribed, request_id => RequestId,
      subscription_id => SubscriptionId};
msg_to_erl([?UNSUBSCRIBE, RequestId, SubscriptionId]) ->
    #{type => unsubscribe, request_id => RequestId,
      subscription_id => SubscriptionId};
msg_to_erl([?UNSUBSCRIBED, RequestId]) ->
    #{type => unsubscribed, request_id => RequestId};
msg_to_erl([?EVENT, SubscriptionId, PublicationId, Details]) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => dict_to_erl(Details)};
msg_to_erl([?EVENT, SubscriptionId, PublicationId, Details, Arguments]) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => dict_to_erl(Details),
      arguments => Arguments};
msg_to_erl([?EVENT, SubscriptionId, PublicationId, Details, Arguments,
        ArgumentsKw]) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => dict_to_erl(Details),
      arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_erl([?CALL, RequestId, Options, Procedure]) ->
    #{type => call, request_id => RequestId, options => dict_to_erl(Options),
      procedure => Procedure};
msg_to_erl([?CALL, RequestId, Options, Procedure, Arguments]) ->
    #{type => call, request_id => RequestId, options => dict_to_erl(Options),
      procedure => Procedure, arguments => Arguments};
msg_to_erl([?CALL, RequestId, Options, Procedure, Arguments, ArgumentsKw]) ->
    #{type => call, request_id => RequestId, options => dict_to_erl(Options),
      procedure => Procedure, arguments => Arguments,
      arguments_kw => ArgumentsKw};
msg_to_erl([?RESULT, RequestId, Details]) ->
    #{type => result, request_id => RequestId, details => dict_to_erl(Details)};
msg_to_erl([?RESULT, RequestId, Details, Arguments]) ->
    #{type => result, request_id => RequestId, details => dict_to_erl(Details),
      arguments => Arguments};
msg_to_erl([?RESULT, RequestId, Details, Arguments, ArgumentsKw]) ->
    #{type => result, request_id => RequestId, details => dict_to_erl(Details),
      arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_erl([?REGISTER, RequestId, Options, Procedure]) ->
    #{type => register, request_id => RequestId,
      options => dict_to_erl(Options), procedure => Procedure};
msg_to_erl([?REGISTERED, RequestId, RegistrationId]) ->
    #{type => registered, request_id => RequestId,
      registration_id => RegistrationId};
msg_to_erl([?UNREGISTER, RequestId, RegistrationId]) ->
    #{type => unregister, request_id => RequestId,
      registration_id => RegistrationId};
msg_to_erl([?UNREGISTERED, RequestId]) ->
    #{type => unregistered, request_id => RequestId};
msg_to_erl([?INVOCATION, RequestId, RegistrationId, Details]) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => dict_to_erl(Details)};
msg_to_erl([?INVOCATION, RequestId, RegistrationId, Details, Arguments]) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => dict_to_erl(Details),
      arguments => Arguments};
msg_to_erl([?INVOCATION, RequestId, RegistrationId, Details, Arguments,
            ArgumentsKw]) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => dict_to_erl(Details),
      arguments => Arguments, arguments_kw => ArgumentsKw};
msg_to_erl([?YIELD, RequestId, Options]) ->
    #{type => yield, request_id => RequestId, options => dict_to_erl(Options)};
msg_to_erl([?YIELD, RequestId, Options, Arguments]) ->
    #{type => yield, request_id => RequestId, options => dict_to_erl(Options),
      arguments => Arguments};
msg_to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
    #{type => yield, request_id => RequestId, options => dict_to_erl(Options),
      arguments => Arguments, arguments_kw => ArgumentsKw};
%% ADVANCED MESSAGES
msg_to_erl([?CHALLENGE, AuthMethod, Extra]) ->
    #{type => challenge, auth_method => authmethod_to_erl(AuthMethod),
      extra => dict_to_erl(Extra)};
msg_to_erl([?AUTHENTICATE, Signature, Extra]) ->
    #{type => authenticate, signature => Signature,
      extra => dict_to_erl(Extra)};
msg_to_erl([?CANCEL, RequestId, Options]) ->
    #{type => cancel, request_id => RequestId, options => dict_to_erl(Options)};
msg_to_erl([?INTERRUPT, RequestId, Options]) ->
    #{type => interrupt, request_id => RequestId,
      options => dict_to_erl(Options)};
msg_to_erl(#{type := ping} = Ping) ->
    Ping;
msg_to_erl(#{type := pong} = Pong) ->
    Pong.
%% msg_to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq, _Discard]) ->
%%     msg_to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq]);
%% msg_to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq]) ->
%%     #{type => heartbeat, sequence_in => IncomingSeq,
%%       sequence_out => OutgoingSeq};


%% @private
try_error_to_erl(Error) ->
    convert_value(to_erl, Error, ?ERROR_MAPPING).


%% @private
error_to_wamp(Error) ->
    convert_value(to_wamp, Error, ?ERROR_MAPPING).

dict_to_erl(Dict) when is_map(Dict) ->
    value_to_erl(Dict).

value_to_erl(Map) when is_map (Map) ->
    PropList = maps:to_list(Map),
    Convert = fun({Key, Value}, NewMap) ->
                NewKey = try_to_atom(Key),
                NewValue = value_to_erl(Value),
                maps:put(NewKey, NewValue, NewMap)
              end,
    lists:foldl(Convert, #{}, PropList);
value_to_erl(Binary) when is_binary(Binary) ->
    try_to_atom(Binary);
value_to_erl(List) when is_list(List) ->
    Convert = fun(Element, NewList) ->
                      NewElement = value_to_erl(Element),
                      [NewElement | NewList]
              end,
    lists:reverse(lists:foldl(Convert, [], List));
value_to_erl(N) when is_number(N) ->
    N.

try_to_atom(Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom -> Atom
    catch _:_ ->
             Binary
    end.

authmethod_to_erl(Method) ->
    convert_value(to_erl, Method, ?AUTH_METHOD_MAPPING).

authmethod_to_wamp(Method) ->
    convert_value(to_wamp, Method, ?AUTH_METHOD_MAPPING).

request_type_to_atom(RequestType) ->
    {RequestType, Atom} = lists:keyfind(RequestType, 1, ?REQUEST_TYPE_MAPPING),
    Atom.

atom_to_request_type(Atom) ->
    {RequestType, Atom} = lists:keyfind(Atom, 2, ?REQUEST_TYPE_MAPPING),
    RequestType.

value_pos(Direction) ->
    key_pos(Direction).

key_pos(to_erl) ->
    2;
key_pos(to_wamp) ->
    1.
convert_value(Direction, Value, Mapping) ->
    ValPos = value_pos(Direction),
    ValueTuple = case lists:keyfind(Value, ValPos, Mapping) of
                     {EV, WV, _} -> {EV, WV};
                     {EV, WV} -> {EV, WV};
                     false -> {Value, Value}
                 end,
    convert(Direction, ValueTuple).

convert(to_erl, {ErlVal, _}) ->
    ErlVal;
convert(to_wamp, {_, WampVal}) ->
    WampVal.

