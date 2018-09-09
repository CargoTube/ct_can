-module(ct_msg_serialization).

-export([
         serialize/2,
         deserialize/2,
         ping/1,
         pong/1
        ]).


-define(JSONB_SEPARATOR, <<24>>).

ping(Payload) ->
    add_binary_frame(1, Payload).

pong(Payload) ->
    add_binary_frame(2, Payload).

serialize(Message, Encoding) ->
  WampMsg = ct_msg_conversion:to_wamp(Message),
  serialize_message(WampMsg, Encoding).


deserialize(Buffer, none) ->
	Buffer;
deserialize(Buffer, Enc)
  when Enc == raw_erlbin; Enc == raw_msgpack; Enc == msgpack_batched;
       Enc == raw_json->
    deserialize_binary(Buffer, [], Enc);
deserialize(Buffer, msgpack) ->
    deserialize_msgpack(Buffer, []);
deserialize(Buffer, json) ->
    deserialize_json(Buffer);
deserialize(Buffer, json_batched) ->
    deserialize_json_batched(Buffer).







serialize_message(Msg, none) ->
  Msg;
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




deserialize_json(Buffer) ->
    handle_json_result(jsone:try_decode(Buffer, []), Buffer).

handle_json_result({ok, Msg, NewBuffer}, _Buffer) ->
    {[ct_msg_conversion:to_internal(Msg)], NewBuffer};
handle_json_result(_, Buffer) ->
    {[], Buffer}.


deserialize_json_batched(Buffer) ->
  Wamps = binary:split(Buffer, [?JSONB_SEPARATOR], [global, trim]),
  Dec = fun(M, List) ->
                Msg = ct_msg_conversion:to_internal(jsone:decode(M, [])),
                [ Msg | List]
            end,
    {lists:reverse(lists:foldl(Dec, [], Wamps)), <<"">>}.

deserialize_msgpack(Buffer, Messages) ->
    handle_msgpack_result(msgpack:unpack_stream(Buffer,
                                                [{unpack_str, as_binary}]),
                          Messages, Buffer).

handle_msgpack_result({error, incomplete}, Messages, Buffer) ->
    {lists:reverse(Messages), Buffer};
handle_msgpack_result({error, Reason}, _, _) ->
    error(Reason);
handle_msgpack_result({Msg0, NewBuffer}, Messages, _Buffer) ->
    Msg = ct_msg_conversion:to_internal(Msg0),
    deserialize_msgpack(NewBuffer, [ Msg | Messages]).


deserialize_binary(Buffer, Messages, Enc) ->
    {Msg0, NewBuffer} = extract_bin_msg(Buffer),
    deserialize_binary_msg(Msg0, Messages, Enc, NewBuffer).


extract_bin_msg(<<LenType:32/unsigned-integer-big, Data/binary>> = Buffer) ->
    <<Type:8, Len:24>> = <<LenType:32>>,
    maybe_return_msg(Type, Len, Data, Buffer);
extract_bin_msg(Buffer) ->
    {none, Buffer}.

maybe_return_msg(Type, Len, Data, _Buffer)
  when is_integer(Len), byte_size(Data) >= Len ->
    <<Payload:Len/binary, NewBuffer/binary>> = Data,
    convert_binary_type(Type, Payload, NewBuffer);
maybe_return_msg(_, _, _, Buffer) ->
    {none, Buffer}.

convert_binary_type(0, Payload, NewBuffer) ->
    {{msg, Payload}, NewBuffer};
convert_binary_type(1, Payload, NewBuffer) ->
    {{ping, Payload}, NewBuffer};
convert_binary_type(2, Payload, NewBuffer) ->
    {{pong, Payload}, NewBuffer}.



deserialize_binary_msg(none, Messages, _, NewBuffer) ->
    {lists:reverse(Messages), NewBuffer};
deserialize_binary_msg({msg, Payload}, Messages, Enc, NewBuffer) ->
    Msg = to_internal_msg(deserialize_binary_msg(Payload, Enc)),
    deserialize_binary(NewBuffer, [Msg | Messages], Enc);
deserialize_binary_msg({ping, _} = Ping, Messages, Enc, NewBuffer) ->
    deserialize_binary(NewBuffer, [Ping | Messages], Enc);
deserialize_binary_msg({pong, _} = Pong, Messages, Enc, NewBuffer) ->
    deserialize_binary(NewBuffer, [Pong | Messages], Enc).


deserialize_binary_msg(Payload, raw_erlbin) ->
    deserialize_raw_erlbin(Payload);
deserialize_binary_msg(Payload, raw_msgpack) ->
    deserialize_raw_msgpack(Payload);
deserialize_binary_msg(Payload, msgpack_batched) ->
    deserialize_raw_msgpack(Payload);
deserialize_binary_msg(Payload, raw_json) ->
    deserialize_raw_json(Payload).

deserialize_raw_msgpack(Data) ->
    msgpack:unpack(Data, [{unpack_str, as_binary}]).

deserialize_raw_json(Data) ->
    {ok, jsone:decode(Data, [])}.

deserialize_raw_erlbin(Data) ->
    {ok, binary_to_term(Data)}.

to_internal_msg({ok, Msg}) ->
    ct_msg_conversion:to_internal(Msg);
to_internal_msg(_) ->
    bad_message.
