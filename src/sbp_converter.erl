%%
%% Copyright (c) 2014-2016 Bas Wegh
%%
-module(sbp_converter).
-author("Bas Wegh, bwegh@github.com").

-include("sbp_mapping.hrl").
-include("sbp_message_codes.hrl").


%% API
-export([to_wamp/1, to_erl/1]).


%% TODO: challenge, authenticate, hartbeat, cancel, interrupt

to_wamp(#{type := hello, realm := Realm, details := Details}) ->
    [?HELLO, Realm, hello_dict_to_wamp(Details)];
%% to_wamp({challenge, wampcra, Extra}) ->
%%     to_wamp({challenge, <<"wampcra">>, Extra});
%% to_wamp({challenge, AuthMethod, Extra}) ->
%%     [?CHALLENGE, AuthMethod, dict_to_wamp(Extra)];
%% to_wamp({authenticate, Signature, Extra}) ->
%%     [?AUTHENTICATE, Signature, dict_to_wamp(Extra)];
to_wamp(#{type := welcome, session_id := SessionId, details := Details}) ->
    [?WELCOME, SessionId, dict_to_wamp(Details)];
%% to_wamp({type := heartbeat, IncomingSeq, OutgoingSeq}) ->
%%     [?HEARTBEAT, IncomingSeq, OutgoingSeq];
to_wamp(#{type := abort, details := Details, reason := Reason}) ->
    [?ABORT, dict_to_wamp(Details), error_to_wamp(Reason)];
to_wamp(#{type := goodbye, details := Details, reason := Reason}) ->
    [?GOODBYE, dict_to_wamp(Details), error_to_wamp(Reason)];
to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error,
          arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    WampType = atom_to_request_type(AtomType),
    Arguments = maps:get(arguments, Msg, []),
    [?ERROR, WampType, RequestId, Details, error_to_wamp(Error), Arguments,
     ArgumentsKw];
to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_to_wamp(Error), Arguments];
to_wamp(#{type := error, request_type := AtomType, request_id := RequestId,
          details := Details, error := Error}) ->
    WampType = atom_to_request_type(AtomType),
    [?ERROR, WampType, RequestId, Details, error_to_wamp(Error)];
to_wamp(#{type := publish, request_id := RequestId, options := Options,
          topic := Topic, arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
        Arguments = maps:get(arguments, Msg, []),
        [?PUBLISH, RequestId, dict_to_wamp(Options), Topic, Arguments,
         ArgumentsKw];
to_wamp(#{type := publish, request_id := RequestId, options := Options,
          topic := Topic, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
        [?PUBLISH, RequestId, dict_to_wamp(Options), Topic, Arguments];
to_wamp(#{type := publish, request_id := RequestId, options := Options,
          topic := Topic}) ->
    [?PUBLISH, RequestId, dict_to_wamp(Options), Topic];
to_wamp({publish, RequestId, Options, Topic, Arguments, ArgumentsKw}) ->
    [?PUBLISH, RequestId, dict_to_wamp(Options), Topic, Arguments, ArgumentsKw];
to_wamp(#{type := published, request_id := RequestId,
          publication_id := PublicationId}) ->
    [?PUBLISHED, RequestId, PublicationId];
to_wamp(#{type := subscribe, request_id := RequestId, options := Options,
          topic := Topic}) ->
    [?SUBSCRIBE, RequestId, dict_to_wamp(Options), Topic];
to_wamp(#{type := subscribed, request_id := RequestId,
          subscription_id := SubscriptionId}) ->
    [?SUBSCRIBED, RequestId, SubscriptionId];
to_wamp(#{type := unsubscribe, request_id := RequestId,
          subscription_id := SubscriptionId}) ->
    [?UNSUBSCRIBE, RequestId, SubscriptionId];
to_wamp(#{type := unsubscribed, request_id := RequestId}) ->
    [?UNSUBSCRIBED, RequestId];
to_wamp(#{type := event, subscription_id := SubscriptionId,
          publication_id := PublicationId, details := Details,
          arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?EVENT, SubscriptionId, PublicationId, dict_to_wamp(Details), Arguments,
     ArgumentsKw];
to_wamp(#{type := event, subscription_id := SubscriptionId,
          publication_id := PublicationId, details := Details,
          arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?EVENT, SubscriptionId, PublicationId, dict_to_wamp(Details), Arguments];
to_wamp(#{type := event, subscription_id := SubscriptionId,
          publication_id := PublicationId, details := Details}) ->
    [?EVENT, SubscriptionId, PublicationId, dict_to_wamp(Details)];
to_wamp(#{type := call, request_id := RequestId, options := Options,
          procedure := Procedure, arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?CALL, RequestId, dict_to_wamp(Options), Procedure, Arguments,
     ArgumentsKw];
to_wamp(#{type := call, request_id := RequestId, options := Options,
          procedure := Procedure, arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?CALL, RequestId, dict_to_wamp(Options), Procedure, Arguments];
to_wamp(#{type := call, request_id := RequestId, options := Options,
          procedure := Procedure}) ->
    [?CALL, RequestId, dict_to_wamp(Options), Procedure];
%% to_wamp({cancel, RequestId, Options}) ->
%%     [?CANCEL, RequestId, dict_to_wamp(Options)];
to_wamp(#{type := result, request_id := RequestId, details := Details,
         arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?RESULT, RequestId, dict_to_wamp(Details), Arguments, ArgumentsKw];
to_wamp(#{type := result, request_id := RequestId, details := Details,
         arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?RESULT, RequestId, dict_to_wamp(Details), Arguments];
to_wamp(#{type := result, request_id := RequestId, details := Details}) ->
    [?RESULT, RequestId, dict_to_wamp(Details)];
to_wamp(#{type := register, request_id := RequestId, options := Options,
          procedure := Procedure}) ->
    [?REGISTER, RequestId, dict_to_wamp(Options), Procedure];
to_wamp(#{type := registered, request_id := RequestId,
          registration_id := RegistrationId}) ->
    [?REGISTERED, RequestId, RegistrationId];
to_wamp(#{type := unregister, request_id := RequestId,
          registration_id := RegistrationId}) ->
    [?UNREGISTER, RequestId, RegistrationId];
to_wamp(#{type := unregistered, request_id := RequestId}) ->
    [?UNREGISTERED, RequestId];
to_wamp(#{type := invocation, request_id := RequestId,
         registration_id := RegistrationId, details := Details,
         arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?INVOCATION, RequestId, RegistrationId, dict_to_wamp(Details), Arguments,
    ArgumentsKw];
to_wamp(#{type := invocation, request_id := RequestId,
         registration_id := RegistrationId, details := Details,
         arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?INVOCATION, RequestId, RegistrationId, dict_to_wamp(Details), Arguments];
to_wamp(#{type := invocation, request_id := RequestId,
         registration_id := RegistrationId, details := Details}) ->
    [?INVOCATION, RequestId, RegistrationId, dict_to_wamp(Details)];
%% to_wamp({interrupt, RequestId, Options}) ->
%%     [?INTERRUPT, RequestId, dict_to_wamp(Options)];
to_wamp(#{type := yield, request_id := RequestId, options := Options,
         arguments_kw := ArgumentsKw} = Msg)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    Arguments = maps:get(arguments, Msg, []),
    [?YIELD, RequestId, dict_to_wamp(Options), Arguments, ArgumentsKw];
to_wamp(#{type := yield, request_id := RequestId, options := Options,
         arguments := Arguments})
  when is_list(Arguments), length(Arguments) > 0 ->
    [?YIELD, RequestId, dict_to_wamp(Options), Arguments];
to_wamp(#{type := yield, request_id := RequestId, options := Options}) ->
    [?YIELD, RequestId, dict_to_wamp(Options)].

to_erl(WampMsg) ->
    ErlMsg = msg_to_erl(WampMsg),
    true = sbp_validator:is_valid_message(ErlMsg),
    ErlMsg.

msg_to_erl([?HELLO, Realm, Details]) ->
    #{type => hello, realm => Realm, details => hello_dict_to_erl(Details)};
msg_to_erl([?WELCOME, SessionId, Details]) ->
    #{type => welcome, session_id => SessionId,
      details => dict_to_erl(Details)};
msg_to_erl([?ABORT, Details, Reason]) ->
    #{type => abort, details => dict_to_erl(Details),
      reason => try_error_to_erl(Reason)};
msg_to_erl([?CHALLENGE, <<"wampcra">>, Extra]) ->
    msg_to_erl([?CHALLENGE, wampcra, Extra]);
msg_to_erl([?CHALLENGE, AuthMethod, Extra]) ->
    #{type => challenge, auth_method => AuthMethod,
      extra => dict_to_erl(Extra)};
msg_to_erl([?AUTHENTICATE, Signature, Extra]) ->
    #{type => authenticate, signature => Signature,
      extra => dict_to_erl(Extra)};
msg_to_erl([?GOODBYE, Details, Reason]) ->
    #{type => goodbye, details => dict_to_erl(Details),
      reason => try_error_to_erl(Reason)};
msg_to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq, _Discard]) ->
    msg_to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq]);
msg_to_erl([?HEARTBEAT, IncomingSeq, OutgoingSeq]) ->
    #{type => heartbeat, sequence_in => IncomingSeq,
      sequence_out => OutgoingSeq};
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
msg_to_erl([?CANCEL, RequestId, Options]) ->
    #{type => cancel, request_id => RequestId, options => dict_to_erl(Options)};
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
msg_to_erl([?INTERRUPT, RequestId, Options]) ->
    #{type => interrupt, request_id => RequestId,
      options => dict_to_erl(Options)};
msg_to_erl([?YIELD, RequestId, Options]) ->
    #{type => yield, request_id => RequestId, options => dict_to_erl(Options)};
msg_to_erl([?YIELD, RequestId, Options, Arguments]) ->
    #{type => yield, request_id => RequestId, options => dict_to_erl(Options),
      arguments => Arguments};
msg_to_erl([?YIELD, RequestId, Options, Arguments, ArgumentsKw]) ->
    #{type => yield, request_id => RequestId, options => dict_to_erl(Options),
      arguments => Arguments, arguments_kw => ArgumentsKw}.


%% @private
dict_to_erl(Dict) ->
    convert_dict(default, Dict, to_erl).

%% @private
dict_to_wamp(Dict) ->
    convert_dict(default, Dict, to_wamp).
%% @private
hello_dict_to_erl(Dict) ->
    convert_dict(hello, Dict, to_erl).

%% @private
hello_dict_to_wamp(Dict) ->
    convert_dict(hello, Dict, to_wamp).

%% @private
try_error_to_erl(Error) ->
    case error_to_erl(Error) of
        {unknown_error, Error} ->
            Error;
        ErlError ->
            ErlError
    end.

%% @private
error_to_erl(Error) ->
    convert_error(to_erl, Error).

%% @private
error_to_wamp(Error) ->
    convert_error(to_wamp, Error).

%% @private
convert_error(Direction, Error) ->
    KeyPos =
    case Direction of
        to_erl -> 2;
        to_wamp -> 1
    end,
    {ErlError, WampError} =
    case lists:keyfind(Error, KeyPos, ?ERROR_MAPPING) of
        {EE, WE} -> {EE, WE};
        false -> {Error, Error}
    end,
    case Direction of
        to_erl ->
            case is_atom(ErlError) of
                true ->
                    ErlError;
                false ->
                    {unknown_error, WampError}
            end;
        to_wamp ->
            case is_atom(WampError) of
                true ->
                    <<"wamp.error.internal">>;
                false ->
                    WampError
            end
    end.

%% @private
convert_dict(Type, PropList, Direction) when is_list(PropList) ->
    Map = maps:from_list(PropList),
    convert_dict(Type, Map, Direction);
convert_dict(Type, Map, Direction) ->
    Mapping = case Type of
                  hello -> ?HELLO_MAPPING;
                  roles -> ?HELLO_MAPPING;
                  _ -> ?DICT_MAPPING
              end,
    Folding = fun(Key, Value, InMap) ->
                      {ConvKey, ConvValue} =
                      convert_key_value(Direction, Key, Value, Mapping),
                      maps:put(ConvKey, ConvValue, InMap)
              end,
    maps:fold(Folding, #{}, Map).

%% @private
convert_key_value(Direction, Key, Value, Mapping) ->
    KeyPos = key_pos(Direction),
    {ErlKey, WampKey, Deep} =
    case lists:keyfind(Key, KeyPos, Mapping) of
        {Ek, Wk, D} -> {Ek, Wk, D};
        false -> {Key, Key, false}
    end,
    ConvValue =
    case Deep of
        dict -> convert_dict(ErlKey, Value, Direction);
        list -> convert_list(Direction, Value, [], Mapping);
        value -> convert_value(Direction, Value, Mapping);
        _ -> Value
    end,
    ConvKey = convert(Direction, {ErlKey, WampKey}),
    {ConvKey, ConvValue}.

%% @private
convert_list(_, [], [], _) -> [];
convert_list(_, [], Converted, _) -> lists:reverse(Converted);
convert_list(Direction, [Key | T], Converted, Mapping) ->
    KeyPos = key_pos(Direction),
    Keys =
    case lists:keyfind(Key, KeyPos, Mapping) of
        {Ek, Wk, _} -> {Ek, Wk};
        false -> {Key, Key}
    end,
    ConvKey = convert(Direction, Keys),
    convert_list(Direction, T, [ConvKey | Converted], Mapping).

%% @private
convert_value(Direction, Value, Mapping) ->
    ValPos = value_pos(Direction),
    Values =
    case lists:keyfind(Value, ValPos, Mapping) of
        {EV, WV, _} -> {EV, WV};
        false -> {Value, Value}
    end,
    convert(Direction, Values).

-define(REQUEST_TYPE_MAPPING,
        [{?SUBSCRIBE, subscribe},
        {?UNSUBSCRIBE, unsubscribe},
        {?PUBLISH, publish},
        {?REGISTER, register},
        {?UNREGISTER, unregister},
        {?CALL, call},
        {?INVOCATION, invocation}]).

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

convert(to_erl, {ErlVal, _}) ->
    ErlVal;
convert(to_wamp, {_, WampVal}) ->
    WampVal.

