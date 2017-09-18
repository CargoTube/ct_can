%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-module(ct_msg).
-author("Bas Wegh").
-include("ct_msg_mapping.hrl").

-export([
         get_type/1,

         parse/2,
         serialize/2,
         ping/1,
         pong/1
        ]).

-define(JSONB_SEPARATOR, <<24>>).


get_type(Msg) ->
    erlang:element(1, Msg).


parse(Buffer, Encoding) ->
    BinaryEncodings = [raw_msgpack, raw_json, msgpack_batched, raw_erlbin],
    IsBinaryEnc = lists:member(Encoding, BinaryEncodings),
    parse_bin_or_text(IsBinaryEnc, Buffer, Encoding).

serialize(WampMap, Enc) ->
  WampMsg = ct_msg_conversion:to_wamp(WampMap),
  serialize_message(WampMsg, Enc).

ping(Paydeserialize) ->
    add_binary_frame(1, Paydeserialize).

pong(Paydeserialize) ->
    add_binary_frame(2, Paydeserialize).



%% @privte
parse_bin_or_text(true, Buffer, Encoding) ->
    parse_binary(Buffer, [], Encoding);
parse_bin_or_text(false, Buffer, Encoding) ->
    parse_text(Buffer, [], Encoding).


-spec parse_text(Buffer :: binary(), Messages :: list(),
                       Encoding :: atom()) ->
    {[Message :: map()], NewBuffer :: binary()}.
parse_text(Buffer, Messages, msgpack) ->
    handle_msgpack_result(msgpack:unpack_stream(Buffer,
                                                [{unpack_str, as_binary}]),
                          Messages, Buffer);
parse_text(Buffer, Messages, json) ->
    handle_json_result(jsone:try_decode(Buffer, []), Messages, Buffer);
parse_text(Buffer, _Messages, json_batched) ->
  Wamps = binary:split(Buffer, [?JSONB_SEPARATOR], [global, trim]),
  Dec = fun(M, List) ->
                [jsone:decode(M, []) | List]
        end,
  {to_erl_reverse(lists:foldl(Dec, [], Wamps)), <<"">>};
parse_text(Buffer, Messages, _) ->
  {to_erl_reverse(Messages), Buffer}.



%% @private

handle_msgpack_result({error, incomplete}, Messages, Buffer) ->
    {to_erl_reverse(Messages), Buffer};
handle_msgpack_result({error, Reason}, _, _) ->
    error(Reason);
handle_msgpack_result({Msg, NewBuffer}, Messages, _Buffer) ->
    parse_text(NewBuffer, [Msg | Messages], msgpack).


handle_json_result({ok, Msg, NewBuffer}, Messages, _Buffer) ->
    {[ct_msg_conversion:to_internal(Msg) | Messages], NewBuffer};
handle_json_result(_, Messages, Buffer) ->
    {Messages, Buffer}.



-spec parse_binary(Buffer :: binary(), Messages :: list(),
                         Encoding :: atom()) ->
  {[Message :: term()], NewBuffer :: binary()}.
parse_binary(<<LenType:32/unsigned-integer-big, Data/binary>> = Buffer,
                   Messages, Enc) ->
    <<Type:8, Len:24>> = <<LenType:32>>,
    decode_binary(Type, Len, Data, Enc, Messages, Buffer);
parse_binary(Buffer, Messages, _Enc) ->
    {to_erl_reverse(Messages), Buffer}.


decode_binary(Type, Len, Data, Enc, Messages, _Buffer)
  when is_integer(Len), byte_size(Data) =< Len ->
    <<Paydeserialize:Len/binary, NewBuffer/binary>> = Data,
    decode_binary_msg(Type, Enc, Paydeserialize, Messages, NewBuffer);
decode_binary(_Type, Len, _Data, _Enc, Messages, Buffer)
  when is_integer(Len) ->
    {to_erl_reverse(Messages), Buffer}.

decode_binary_msg(0, Enc, Paydeserialize, Messages, Buffer) ->
    {ok, Msg} = binary_to_msg(Enc, Paydeserialize),
    parse_binary(Buffer, [Msg | Messages], Enc);
decode_binary_msg(1, Enc, Paydeserialize, Messages, Buffer) ->
    parse_binary(Buffer, [{ping, Paydeserialize}
                            | Messages], Enc);
decode_binary_msg(2, Enc, Paydeserialize, Messages, Buffer) ->
    parse_binary(Buffer, [{pong, Paydeserialize}
                            | Messages], Enc).

binary_to_msg(raw_erlbin, Paydeserialize) ->
    {ok, binary_to_term(Paydeserialize)};
binary_to_msg(raw_json, Paydeserialize) ->
    {ok, jsone:decode(Paydeserialize, [])};
binary_to_msg(_, Paydeserialize) ->
    msgpack:unpack(Paydeserialize, [{unpack_str, as_binary}]).





%% @private
serialize_message(Msg, msgpack) ->
  msgpack:pack(Msg, [{pack_str, from_binary}]);
serialize_message(Msg, msgpack_batched) ->
  serialize_message(Msg, raw_msgpack);
serialize_message(Msg, json) ->
  jsone:encode(Msg);
serialize_message(Msg, json_batched) ->
  Enc = jsone:encode(Msg),
  <<Enc/binary, ?JSONB_SEPARATOR/binary>>;
serialize_message(Message, raw_erlbin) ->
  Enc = term_to_binary(Message),
  add_binary_frame(Enc);
serialize_message(Message, raw_msgpack) ->
  Enc = msgpack:pack(Message, [{pack_str, from_binary}]),
  add_binary_frame(Enc);
serialize_message(Message, raw_json) ->
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
  to_erl_reverse(T, [ct_msg_conversion:to_internal(H) | Messages]).
