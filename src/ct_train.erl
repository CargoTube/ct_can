%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

%% @private
-module(ct_train).
-author("Bas Wegh").
-include("ct_train_mapping.hrl").

-export([
         load_cargo/2,
         unload_cargo/2,
         ping/1,
         pong/1
        ]).

-define(JSONB_SEPARATOR, <<24>>).

load_cargo(Buffer, Encoding) ->
    BinaryEncodings = [raw_msgpack, raw_json, msgpack_batched, raw_erlbin],
    IsBinaryEnc = lists:member(Encoding, BinaryEncodings),
    load_bin_or_text(IsBinaryEnc, Buffer, Encoding).


unload_cargo(WampMap, Enc) ->
  WampMsg = ct_train_loading:unload(WampMap),
  unload_message(WampMsg, Enc).

ping(Payload) ->
    add_binary_frame(1, Payload).

pong(Payload) ->
    add_binary_frame(2, Payload).

load_bin_or_text(true, Buffer, Encoding) ->
    load_binary(Buffer, [], Encoding);
load_bin_or_text(false, Buffer, Encoding) ->
    load_text(Buffer, [], Encoding).


%% @private
-spec load_text(Buffer :: binary(), Messages :: list(),
                       Encoding :: atom()) ->
    {[Message :: map()], NewBuffer :: binary()}.
load_text(Buffer, Messages, msgpack) ->
  case msgpack:unpack_stream(Buffer, []) of
    {error, incomplete} ->
      {to_erl_reverse(Messages), Buffer};
    {error, Reason} ->
      error(Reason);
    {Msg, NewBuffer} ->
      load_text(NewBuffer, [Msg | Messages], msgpack)
  end;
load_text(Buffer, Messages, json) ->
    case jsone:try_decode(Buffer, []) of
        {ok, Msg, NewBuffer} ->
            {[ct_train_loading:load(Msg) | Messages], NewBuffer};
        _ ->
            {Messages, Buffer}
    end;
load_text(Buffer, _Messages, json_batched) ->
  Wamps = binary:split(Buffer, [?JSONB_SEPARATOR], [global, trim]),
  Dec = fun(M, List) ->
                [jsone:decode(M, []) | List]
        end,
  {to_erl_reverse(lists:foldl(Dec, [], Wamps)), <<"">>};
load_text(Buffer, Messages, _) ->
  {to_erl_reverse(Messages), Buffer}.

%% @private
-spec load_binary(Buffer :: binary(), Messages :: list(),
                         Encoding :: atom()) ->
  {[Message :: term()], NewBuffer :: binary()}.
load_binary(<<LenType:32/unsigned-integer-big, Data/binary>> = Buffer,
                   Messages, Enc) ->
    <<Type:8, Len:24>> = <<LenType:32>>,
    case {Type, byte_size(Data) >= Len} of
        {0, true} ->
            <<EncMsg:Len/binary, NewBuffer/binary>> = Data,
            {ok, Msg} = case Enc of
                            raw_erlbin ->
                                {ok, binary_to_term(EncMsg)};
                            raw_json ->
                                {ok, jsone:decode(EncMsg, [])};
                            _ ->
                                msgpack:unpack(EncMsg, [])
                        end,
            load_binary(NewBuffer, [Msg | Messages], Enc);
        {1, true} ->      %Ping
            <<Payload:Len/binary, NewBuffer/binary>> = Data,
            load_binary(NewBuffer, [#{type => ping, payload => Payload}
                                           | Messages], Enc);
        {2, true} ->      %Pong
            <<Payload:Len/binary, NewBuffer/binary>> = Data,
            load_binary(NewBuffer, [#{type => pong, payload => Payload}
                                           | Messages], Enc);
        {_, false} ->
            {to_erl_reverse(Messages), Buffer}
    end;
load_binary(Buffer, Messages, _Enc) ->
    {to_erl_reverse(Messages), Buffer}.

%% @private
unload_message(Msg, msgpack) ->
  msgpack:pack(Msg, []);
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
  Enc = msgpack:pack(Message, []),
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
  to_erl_reverse(T, [ct_train_loading:load(H) | Messages]).
