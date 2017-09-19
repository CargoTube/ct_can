%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-module(ct_msg).
-author("Bas Wegh").
-include("ct_msg_mapping.hrl").

-export([
         get_type/1,

         deserialize/2,
         serialize/2,
         ping/1,
         pong/1
        ]).

-define(JSONB_SEPARATOR, <<24>>).


get_type(Msg) ->
    erlang:element(1, Msg).

deserialize(Buffer, Encoding) ->
    ct_msg_serialization:deserialize(Buffer, Encoding).

serialize(Wamp, Enc) ->
    ct_msg_serialization:serialize(Wamp, Enc).

ping(Payload) ->
    ct_msg_serialization:ping(Payload).

pong(Payload) ->
    ct_msg_serialization:pong(Payload).
