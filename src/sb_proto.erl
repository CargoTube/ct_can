%%
%% Copyright (c) 2014-2016 Bas Wegh
%%

%% @private
-module(sb_proto).
-author("Bas Wegh").

-export([deserialize/2, serialize/2]).

-define(JSONB_SEPARATOR, <<24>>).

deserialize(Buffer, Encoding) ->
    BinaryEncodings = [raw_msgpack, raw_json, msgpack_batched, raw_erlbin],
    IsBinaryEnc = lists:member(Encoding, BinaryEncodings),
    deserialize_bin_or_text(IsBinaryEnc, Buffer, Encoding).

deserialize_bin_or_text(true, Buffer, Encoding) ->
    deserialize_binary(Buffer, [], Encoding);
deserialize_bin_or_text(false, Buffer, Encoding) ->
    deserialize_text(Buffer, [], Encoding).



serialize(Erwa, Enc) ->
  WAMP = case {lists:member(Enc, [erlbin, raw_erlbin]), is_tuple(Erwa)} of
           {false, true} -> sbp_converter:to_wamp(Erwa);
           _ -> Erwa
         end,
  serialize_message(WAMP, Enc).


%% @private
-spec deserialize_text(Buffer :: binary(), Messages :: list(),
                       Encoding :: atom()) -> {[Message :: map()],
                                               NewBuffer :: binary()}.
deserialize_text(Buffer, Messages, erlbin) ->
  Msg = binary_to_term(Buffer),
  true = sbp_validator:is_valid_message(Msg),
  {[Msg | Messages], <<"">>};
deserialize_text(Buffer, Messages, msgpack) ->
  case msgpack:unpack_stream(Buffer, []) of
    {error, incomplete} ->
      {to_erl_reverse(Messages), Buffer};
    {error, Reason} ->
      error(Reason);
    {Msg, NewBuffer} ->
      deserialize_text(NewBuffer, [Msg | Messages], msgpack)
  end;
deserialize_text(Buffer, Messages, json) ->
  %% is it possible to check the data here ?
  %% length and stuff, yet should not be needed
  Msg = jsx:decode(Buffer, [return_maps, {labels, attempt_atom}]),
  {[sbp_converter:to_erl(Msg) | Messages], <<"">>};
deserialize_text(Buffer, _Messages, json_batched) ->
  Wamps = binary:split(Buffer, [?JSONB_SEPARATOR], [global, trim]),
  Dec = fun(M, List) ->
                [jsx:decode(M, [return_maps, {labels, attempt_atom}]) | List]
        end,
  {to_erl_reverse(lists:foldl(Dec, [], Wamps)), <<"">>};
deserialize_text(Buffer, Messages, _) ->
  {to_erl_reverse(Messages), Buffer}.

%% @private
-spec deserialize_binary(Buffer :: binary(), Messages :: list(),
                         Encoding :: atom()) ->
  {[Message :: term()], NewBuffer :: binary()}.
deserialize_binary(<<LenType:32/unsigned-integer-big, Data/binary>> = Buffer,
                   Messages, Enc) ->
  <<Type:8, Len:24>> = <<LenType:32>>,
  case {Type, byte_size(Data) >= Len} of
    {0, true} ->
      <<EncMsg:Len/binary, NewBuffer/binary>> = Data,
      {ok, Msg} = case Enc of
                    raw_erlbin ->
                      DecMsg = binary_to_term(EncMsg),
                      true = sbp_validator:is_valid_message(DecMsg),
                      {ok, DecMsg};
                    raw_json ->
                      {ok, jsx:decode(EncMsg, [return_maps, {labels,
                                                             attempt_atom}])};
                    _ ->
                      msgpack:unpack(EncMsg, [])
                  end,
      deserialize_binary(NewBuffer, [Msg | Messages], Enc);
    {1, true} ->      %Ping
      <<Ping:Len/binary, NewBuffer/binary>> = Data,
      deserialize_binary(NewBuffer, [{ping, Ping} | Messages], Enc);
    {2, true} ->
      <<Pong:Len/binary, NewBuffer/binary>> = Data,
      deserialize_binary(NewBuffer, [{pong, Pong} | Messages], Enc);
    {_, false} ->  %Pong
      case Enc of
        raw_erlbin -> {lists:reverse(Messages), Buffer};
        _ -> {to_erl_reverse(Messages), Buffer}
      end
  end;
deserialize_binary(Buffer, Messages, Enc) ->
  case Enc of
    raw_erlbin -> {lists:reverse(Messages), Buffer};
    _ -> {to_erl_reverse(Messages), Buffer}
  end.

%% @private
serialize_message(Msg, msgpack) ->
  case msgpack:pack(Msg, []) of
    {error, Reason} ->
      error(sbp_msgpack, [Reason]);
    M ->
      M
  end;
serialize_message(Msg, erlbin) ->
  term_to_binary(Msg);
serialize_message(Msg, msgpack_batched) ->
  serialize(Msg, raw_msgpack);
serialize_message(Msg, json) ->
  jsx:encode(Msg);
serialize_message(Msg, json_batched) ->
  Enc = jsx:encode(Msg),
  <<Enc/binary, ?JSONB_SEPARATOR/binary>>;
serialize_message(Message, raw_erlbin) ->
  Enc = term_to_binary(Message),
  add_binary_frame(Enc);
serialize_message(Message, raw_msgpack) ->
    Enc = case msgpack:pack(Message, [{allow_atom, pack}]) of
          {error, Reason} ->
            error(Reason);
          Msg ->
            Msg
        end,
  add_binary_frame(Enc);
serialize_message(Message, raw_json) ->
  Enc = jsx:encode(Message),
  add_binary_frame(Enc).

%% @private
add_binary_frame(Enc) ->
  Len = byte_size(Enc),
  <<0:8, Len:24/unsigned-integer-big, Enc/binary>>.

%% @private
to_erl_reverse(List) ->
  to_erl_reverse(List, []).

%% @private
to_erl_reverse([], List) -> List;
to_erl_reverse([H | T], Messages) ->
  to_erl_reverse(T, [sbp_converter:to_erl(H) | Messages]).



