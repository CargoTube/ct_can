%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-module(ct_msg).
-author("Bas Wegh").
-include("ct_msg_mapping.hrl").

-export([
         get_type/1,
         extract_session/1,
         get_request_id/1,

         deserialize/2,
         serialize/2,
         ping/1,
         pong/1
        ]).

-define(JSONB_SEPARATOR, <<24>>).


get_type(Msg) ->
    erlang:element(1, Msg).

extract_session({welcome, SessionId, _}) ->
    {ok, SessionId};
extract_session(_) ->
    {error, not_welcome}.

get_request_id(Message) ->
    Type = get_type(Message),
    ValidMessageTypes = [publish, subscribe, unsubscribe, call, register,
                         unregister, invocation],
    IsValidType = lists:member(Type, ValidMessageTypes),
    maybe_get_request_id(IsValidType, Message).

deserialize(Buffer, Encoding) ->
    ct_msg_serialization:deserialize(Buffer, Encoding).

serialize(Wamp, Enc) ->
    ct_msg_serialization:serialize(Wamp, Enc).

ping(Payload) ->
    ct_msg_serialization:ping(Payload).

pong(Payload) ->
    ct_msg_serialization:pong(Payload).


maybe_get_request_id(true, Message) ->
    {ok, erlang:element(2, Message)};
maybe_get_request_id(_, _) ->
    {error, bad_message}.
