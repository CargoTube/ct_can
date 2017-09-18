-module(ct_msg_test).
-include_lib("eunit/include/eunit.hrl").
-include("ct_msg_codes.hrl").


-define(MSGS, [
            {[?HELLO, Realm, #{}],
             {hello, Realm, #{}} },
            {[?CHALLENGE, <<"wampcra">>, #{}],
             {challenge, wampcra, #{}} },
            {[?AUTHENTICATE, Signature, #{}],
             {authenticate, Signature, #{}} },
            {[?WELCOME, 234, #{}],
             {welcome, 234, #{}}},
            {[?ABORT, #{}, Error],
             {abort, #{}, Error}},
            {[?PUBLISH, 456, #{}, Topic],
             {publish, 456, #{}, Topic} },
            {[?PUBLISH, 456, #{}, Topic, Arg],
             {publish, 456, #{}, Topic, Arg }},
            {[?PUBLISH, 456, #{}, Topic, [], ArgKw ],
             {publish, 456, #{}, Topic,  [], ArgKw } },
            {[?PUBLISHED, 123, 456],
             {published, 123, 456}},
            {[?SUBSCRIBE, 123, #{}, Topic],
             {subscribe, 123, #{}, Topic}},
            {[?SUBSCRIBED, 123, 456],
             {subscribed, 123, 456}},
            {[?UNSUBSCRIBE, 123, 456],
             {unsubscribe, 123, 456}},
            {[?UNSUBSCRIBED, 123],
             {unsubscribed, 123}},
            {[?EVENT, 456, 789, #{}],
             {event, 456, 789, #{}}},
            {[?EVENT, 456, 789, #{}, Arg],
             {event, 456, 789, #{}, Arg}},
            {[?EVENT, 456, 789, #{}, [], ArgKw],
             {event, 456, 789, #{}, [], ArgKw}},
            {[?CALL, 123, #{}, Procedure],
             {call, 123, #{}, Procedure}},
            {[?CALL, 123, #{}, Procedure, Arg],
             {call, 123, #{}, Procedure, Arg}},
            {[?CALL, 123, #{}, Procedure, [], ArgKw],
             {call, 123, #{}, Procedure, [], ArgKw}},
            {[?CANCEL, 123, #{}],
             {cancel, 123, #{}} },
            {[?INTERRUPT, 123, #{}],
             {interrupt, 123, #{}} },
            {[?RESULT, 123, #{}],
             {result, 123, #{}}},
            {[?RESULT, 123, #{}, Arg],
             {result, 123, #{}, Arg}},
            {[?RESULT, 123, #{}, [], ArgKw],
             {result, 123, #{}, [], ArgKw}},
            {[?REGISTER, 123, #{}, Procedure],
             {register, 123, #{}, Procedure}},
            {[?REGISTERED, 123, 456],
             {registered, 123, 456}},
            {[?UNREGISTER, 123, 456],
             {unregister, 123, 456 }},
            {[?UNREGISTERED, 123],
             {unregistered, 123}},
            {[?INVOCATION, 123, 456, #{}],
             {invocation, 123, 456, #{}}},
            {[?INVOCATION, 123, 456, #{}, Arg],
             {invocation, 123, 456, #{}, Arg}},
            {[?INVOCATION, 123, 456, #{}, [], ArgKw],
             {invocation, 123, 456, #{}, [], ArgKw}},
            {[?YIELD, 123, #{}],
             {yield, 123, #{}}},
            {[?YIELD, 123, #{}, Arg],
             {yield, 123, #{}, Arg}},
            {[?YIELD, 123, #{}, [], ArgKw],
             {yield, 123, #{}, [], ArgKw}},
            {[?GOODBYE, #{}, Error],
             {goodbye, #{}, Error }},
            %% ADVANCED MESSAGES
            {[?CHALLENGE, <<"sample method">>, #{}],
             {challenge, <<"sample method">>, #{} }},
            {[?CHALLENGE, <<"wampcra">>, #{}],
             {challenge, wampcra, #{} }},
            {[?AUTHENTICATE, <<"AFFE">>, #{}],
             {authenticate, <<"AFFE">>, #{} }},
            {[?CANCEL, 123, #{}],
             {cancel, 123, #{} }},
            {[?INTERRUPT, 123, #{}],
             {interrupt, 123, #{} }}
           ]).

-define(TYPE_MAPPING, [
                    {?SUBSCRIBE, subscribe},
                    {?UNSUBSCRIBE, unsubscribe},
                    {?PUBLISH, publish},
                    {?REGISTER, register},
                    {?UNREGISTER, unregister},
                    {?CALL, call},
                    {?INVOCATION, invocation} ]).

basic_convert_test_() ->
    ConvertToErl = fun({Wamp, Exp}, List) ->
                           F = fun() ->
                                       io:format("converting ~p to erl~n",[Wamp]),
                                       Erl = ct_msg_conversion:to_internal(Wamp),
                                       io:format("   result:~p~n",[Erl]),
                                       io:format("   expecting:~p~n",[Exp]),
                                       Erl
                               end,
                           [?_assertEqual(Exp, F()) | List]
                   end,
    ConvertToWamp = fun({Exp, Erl}, List) ->
                            F = fun() ->
                                        io:format("converting ~p to wamp~n", [Erl]),
                                        Wamp = ct_msg_conversion:to_wamp(Erl),
                                        io:format("   result:~p~n",[Wamp]),
                                        io:format("   expecting:~p~n",[Exp]),
                                        Wamp
                                end,
                            [?_assertEqual(Exp, F()) | List]
                    end,
    basic_test(ConvertToErl) ++ basic_test(ConvertToWamp).




deserialize_hello_json_test() ->
    WampMsg = <<"[1,\"test\",{\"roles\":{\"publisher\":{}}}]">>,
    {[Hello], Buffer} = ct_msg:parse(WampMsg, json),
    ?assertEqual(<<>>, Buffer),
    ?assertEqual({hello, <<"test">>, #{roles => #{publisher => #{}}}}, Hello).

deserialize_hello_msgpack_test() ->
    WampMsg = <<147,1,196,4,116,101,115,116,129,196,5,114,111,108,101,115,129,
                196,9,112,117,98,108,105,115,104,101,114,128>>,
    {[Hello], Buffer} = ct_msg:parse(WampMsg, msgpack),
    ?assertEqual(<<>>, Buffer),
    ?assertEqual({hello, <<"test">>, #{roles => #{publisher => #{}}}}, Hello).


roundtrip_text_test_() ->
    Json = roundtrip_test(json),
    JsonBatched = roundtrip_test(json_batched),
    MsgPack = roundtrip_test(msgpack),
    Json ++ JsonBatched ++ MsgPack.

roundtrip_binary_test_() ->
    Json = roundtrip_test(raw_json),
    MsgPack = roundtrip_test(raw_msgpack),
    MsgPackBatched = roundtrip_test(msgpack_batched),
    Json ++ MsgPack ++ MsgPackBatched.

roundtrip_erlbin_test_() ->
    %% not part of the official spec
    roundtrip_test(raw_erlbin).

roundtrip_test(Encoding) ->
    Roundrip = fun({_, Erl}, List) ->
                       F = fun() ->
                                   io:format("roundtrip ~p~n",[Encoding]),
                                   io:format("in: ~p~n",[Erl]),
                                   Data = ct_msg:serialize(Erl, Encoding),
                                   io:format("data:~p~n",[Data]),
                                   Result = ct_msg:parse(Data, Encoding),
                                   io:format("out:~p~n",[Result]),
                                   Result
                           end,
                       [?_assertEqual({[Erl],<<>>}, F()) | List]
               end,
    basic_test(Roundrip).


basic_test(Convert) ->
    Realm = <<"test.uri">>,
    Signature = <<"very secret">>,
    Error = <<"wamp.error.test">>,
    Topic = <<"topic.test">>,
    Arg = [1,2,3],
    ArgKw = #{<<"key">> => <<"value">>},
    Procedure = <<"test.procedure">>,

    ToError =fun({WampType, ErlType}, List) ->
                     [{[?ERROR, WampType, 123, #{}, Error],
                       {error, ErlType, 123, #{}, Error}},
                      {[?ERROR, WampType, 123, #{}, Error, Arg],
                       {error, ErlType, 123, #{}, Error, Arg}},
                      {[?ERROR, WampType, 123, #{}, Error, [], ArgKw],
                       {error, ErlType, 123, #{}, Error, [], ArgKw}}
                      | List]
             end,
    AllMsgs = lists:reverse(lists:foldl(ToError, lists:reverse(?MSGS),
                                        ?TYPE_MAPPING)),

    lists:reverse(lists:foldl(Convert, [], AllMsgs)).

basic_msgpack_test() ->
    WampMsg = <<147,1,166,114,101,97,108,109,49,130,165,97,103,101,110,116,175,
                87,97,109,112,121,46,106,115,32,118,49,46,48,46,54,165,114,111,
                108,101,115,132,169,112,117,98,108,105,115,104,101,114,129,168,
                102,101,97,116,117,114,101,115,131,189,115,117,98,115,99,114,
                105,98,101,114,95,98,108,97,99,107,119,104,105,116,101,95,108,
                105,115,116,105,110,103,195,179,112,117,98,108,105,115,104,101,
                114,95,101,120,99,108,117,115,105,111,110,195,184,112,117,98,
                108,105,115,104,101,114,95,105,100,101,110,116,105,102,105,99,
                97,116,105,111,110,195,170,115,117,98,115,99,114,105,98,101,114,
                128,166,99,97,108,108,101,114,129,168,102,101,97,116,117,114,
                101,115,133,185,99,97,108,108,101,101,95,98,108,97,99,107,119,
                104,105,116,101,95,108,105,115,116,105,110,103,195,176,99,97,
                108,108,101,114,95,101,120,99,108,117,115,105,111,110,195,181,
                99,97,108,108,101,114,95,105,100,101,110,116,105,102,105,99,97,
                116,105,111,110,195,184,112,114,111,103,114,101,115,115,105,118,
                101,95,99,97,108,108,95,114,101,115,117,108,116,115,195,174,99,
                97,108,108,95,99,97,110,99,101,108,105,110,103,195,166,99,97,
                108,108,101,101,129,168,102,101,97,116,117,114,101,115,129,181,
                99,97,108,108,101,114,95,105,100,101,110,116,105,102,105,99,97,
                116,105,111,110,195>>,
    {[Msg], <<>>} = ct_msg:parse(WampMsg, msgpack),
    true = case Msg of
               {hello, <<"realm1">>, _} ->
                   true;
               {bad_cargo, List} ->
                   io:format("bad cargo: ~p", [List]),
                   false;
               Other ->
                   io:format("very bad: ~p", [Other]),
                   false
           end.





ping_pong_test() ->
    Payload = <<"this is some payload">>,
    Ping1 = <<1,0,0,1,13>>,
    Ping2 = ct_msg:ping(Payload),
    Pong1 = <<2,0,0,1,13>>,
    Pong2 = ct_msg:pong(Payload),
    {[PingMsg1],<<>>} = ct_msg:parse(Ping1, raw_json),
    {[PingMsg2],<<>>} = ct_msg:parse(Ping2, raw_json),
    {[PongMsg1],<<>>} = ct_msg:parse(Pong1, raw_json),
    {[PongMsg2],<<>>} = ct_msg:parse(Pong2, raw_json),

    ?assertEqual({ping, <<13>>}, PingMsg1),
    ?assertEqual({ping, Payload}, PingMsg2),
    ?assertEqual({pong, <<13>>}, PongMsg1),
    ?assertEqual({pong, Payload}, PongMsg2).
