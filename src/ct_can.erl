%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-module(ct_can).
-author("Bas Wegh").
-include("ct_can_mapping.hrl").

-export([
         load_cargo/2,
         unload_cargo/2,
         ping/1,
         pong/1
        ]).

-define(JSONB_SEPARATOR, <<24>>).


%% load cargo into the can
%% this will perform decoding of the 'cargo' and put it in an internal
%% representation
load_cargo(Buffer, Encoding) ->
    BinaryEncodings = [raw_msgpack, raw_json, msgpack_batched, raw_erlbin],
    IsBinaryEnc = lists:member(Encoding, BinaryEncodings),
    load_bin_or_text_cargo(IsBinaryEnc, Buffer, Encoding).

%% unload cargo off the can
%% this will perform encoding of the internal representation into the
%% desired encoding
unload_cargo(WampMap, Enc) ->
  WampMsg = ct_can_loading:unload(WampMap),
  unload_message(WampMsg, Enc).

ping(Payload) ->
    add_binary_frame(1, Payload).

pong(Payload) ->
    add_binary_frame(2, Payload).



%% @private
load_bin_or_text_cargo(true, Buffer, Encoding) ->
    load_binary_cargo(Buffer, [], Encoding);
load_bin_or_text_cargo(false, Buffer, Encoding) ->
    load_text_cargo(Buffer, [], Encoding).


-spec load_text_cargo(Buffer :: binary(), Messages :: list(),
                       Encoding :: atom()) ->
    {[Message :: map()], NewBuffer :: binary()}.
load_text_cargo(Buffer, Messages, msgpack) ->
    handle_msgpack_result(msgpack:unpack_stream(Buffer,
                                                [{unpack_str, as_binary}]),
                          Messages, Buffer);
load_text_cargo(Buffer, Messages, json) ->
    handle_json_result(jsone:try_decode(Buffer, []), Messages, Buffer);
load_text_cargo(Buffer, _Messages, json_batched) ->
  Wamps = binary:split(Buffer, [?JSONB_SEPARATOR], [global, trim]),
  Dec = fun(M, List) ->
                [jsone:decode(M, []) | List]
        end,
  {to_erl_reverse(lists:foldl(Dec, [], Wamps)), <<"">>};
load_text_cargo(Buffer, Messages, _) ->
  {to_erl_reverse(Messages), Buffer}.



%% @private

handle_msgpack_result({error, incomplete}, Messages, Buffer) ->
    {to_erl_reverse(Messages), Buffer};
handle_msgpack_result({error, Reason}, _, _) ->
    error(Reason);
handle_msgpack_result({Msg, NewBuffer}, Messages, _Buffer) ->
    load_text_cargo(NewBuffer, [Msg | Messages], msgpack).


handle_json_result({ok, Msg, NewBuffer}, Messages, _Buffer) ->
    {[ct_can_loading:load(Msg) | Messages], NewBuffer};
handle_json_result(_, Messages, Buffer) ->
    {Messages, Buffer}.



-spec load_binary_cargo(Buffer :: binary(), Messages :: list(),
                         Encoding :: atom()) ->
  {[Message :: term()], NewBuffer :: binary()}.
load_binary_cargo(<<LenType:32/unsigned-integer-big, Data/binary>> = Buffer,
                   Messages, Enc) ->
    <<Type:8, Len:24>> = <<LenType:32>>,
    decode_binary_cargo(Type, Len, Data, Enc, Messages, Buffer);
load_binary_cargo(Buffer, Messages, _Enc) ->
    {to_erl_reverse(Messages), Buffer}.


decode_binary_cargo(Type, Len, Data, Enc, Messages, _Buffer)
  when is_integer(Len), byte_size(Data) =< Len ->
    <<Payload:Len/binary, NewBuffer/binary>> = Data,
    decode_binary_msg(Type, Enc, Payload, Messages, NewBuffer);
decode_binary_cargo(_Type, Len, _Data, _Enc, Messages, Buffer)
  when is_integer(Len) ->
    {to_erl_reverse(Messages), Buffer}.

decode_binary_msg(0, Enc, Payload, Messages, Buffer) ->
    {ok, Msg} = binary_to_msg(Enc, Payload),
    load_binary_cargo(Buffer, [Msg | Messages], Enc);
decode_binary_msg(1, Enc, Payload, Messages, Buffer) ->
    load_binary_cargo(Buffer, [#{type => ping, payload => Payload}
                            | Messages], Enc);
decode_binary_msg(2, Enc, Payload, Messages, Buffer) ->
    load_binary_cargo(Buffer, [#{type => pong, payload => Payload}
                            | Messages], Enc).

binary_to_msg(raw_erlbin, Payload) ->
    {ok, binary_to_term(Payload)};
binary_to_msg(raw_json, Payload) ->
    {ok, jsone:decode(Payload, [])};
binary_to_msg(_, Payload) ->
    msgpack:unpack(Payload, [{unpack_str, as_binary}]).





%% @private
unload_message(Msg, msgpack) ->
  msgpack:pack(Msg, [{pack_str, from_binary}]);
unload_message(Msg, msgpack_batched) ->
  unload_message(Msg, raw_msgpack);
unload_message(Msg, json) ->
  jsone:encode(Msg);
unload_message(Msg, json_batched) ->
  Enc = jsone:encode(Msg),
  <<Enc/binary, ?JSONB_SEPARATOR/binary>>;
unload_message(Message, raw_erlbin) ->
  Enc = term_to_binary(Message),
  add_binary_frame(Enc);
unload_message(Message, raw_msgpack) ->
  Enc = msgpack:pack(Message, [{pack_str, from_binary}]),
  add_binary_frame(Enc);
unload_message(Message, raw_json) ->
  Enc = jsone:encode(Message),
  add_binary_frame(Enc).

%% @private
add_binary_frame(Enc) ->
    add_binary_frame(0, Enc).

add_binary_frame(Type, Enc) ->
  Len = byte_size(Enc),
  <<Type:8, Len:24/unsigned-integer-big, Enc/binary>>.

%% @private
to_erl_reverse(List) ->
  to_erl_reverse(List, []).

%% @private
to_erl_reverse([], List) -> List;
to_erl_reverse([H | T], Messages) ->
  to_erl_reverse(T, [ct_can_loading:load(H) | Messages]).
