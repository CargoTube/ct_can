-module(ct_msg_test).
-include_lib("eunit/include/eunit.hrl").
-include("ct_msg_codes.hrl").


-define(MSGS, [
            {[?HELLO, Realm, #{}],
             #{type => hello, details => #{}, realm => Realm} },
            {[?CHALLENGE, <<"wampcra">>, #{}],
             #{type => challenge, auth_method => wampcra, extra => #{}} },
            {[?AUTHENTICATE, Signature, #{}],
             #{type => authenticate, signature => Signature, extra => #{}} },
            {[?WELCOME, 234, #{}],
             #{type => welcome, details => #{}, session_id => 234}},
            {[?ABORT, #{}, Error],
             #{type => abort, details => #{}, reason => Error}},
            {[?PUBLISH, 456, #{}, Topic],
             #{type => publish, options => #{}, topic => Topic,
               request_id => 456} },
            {[?PUBLISH, 456, #{}, Topic, Arg],
             #{type => publish, options => #{}, topic => Topic,
               request_id => 456, arguments => Arg }},
            {[?PUBLISH, 456, #{}, Topic, [], ArgKw ],
             #{type => publish, options => #{}, topic => Topic,
               request_id => 456, arguments_kw => ArgKw, arguments => [] } },
            {[?PUBLISHED, 123, 456],
             #{type => published, request_id => 123, publication_id => 456}},
            {[?SUBSCRIBE, 123, #{}, Topic],
             #{type => subscribe, request_id => 123, options => #{},
               topic => Topic}},
            {[?SUBSCRIBED, 123, 456],
             #{type => subscribed, request_id => 123, subscription_id => 456}},
            {[?UNSUBSCRIBE, 123, 456],
             #{type => unsubscribe, request_id => 123, subscription_id => 456}},
            {[?UNSUBSCRIBED, 123],
             #{type => unsubscribed, request_id => 123}},
            {[?EVENT, 456, 789, #{}],
             #{type => event, subscription_id => 456, publication_id => 789,
               details => #{}}},
            {[?EVENT, 456, 789, #{}, Arg],
             #{type => event, subscription_id => 456, publication_id => 789,
               details => #{}, arguments => Arg}},
            {[?EVENT, 456, 789, #{}, [], ArgKw],
             #{type => event, subscription_id => 456, publication_id => 789,
               details => #{}, arguments => [], arguments_kw => ArgKw}},
            {[?CALL, 123, #{}, Procedure],
             #{type => call, request_id => 123, options => #{},
               procedure => Procedure}},
            {[?CALL, 123, #{}, Procedure, Arg],
             #{type => call, request_id => 123, options => #{},
               procedure => Procedure, arguments => Arg}},
            {[?CALL, 123, #{}, Procedure, [], ArgKw],
             #{type => call, request_id => 123, options => #{},
               procedure => Procedure, arguments_kw=> ArgKw, arguments => []}},
            {[?CANCEL, 123, #{}],
             #{type => cancel, request_id => 123, options => #{}} },
            {[?INTERRUPT, 123, #{}],
             #{type => interrupt, request_id => 123, options => #{}} },
            {[?RESULT, 123, #{}],
             #{type => result, request_id => 123, details => #{}}},
            {[?RESULT, 123, #{}, Arg],
             #{type => result, request_id => 123, details => #{},
               arguments => Arg}},
            {[?RESULT, 123, #{}, [], ArgKw],
             #{type => result, request_id => 123, details => #{},
               arguments => [], arguments_kw => ArgKw}},
            {[?REGISTER, 123, #{}, Procedure],
             #{type => register, request_id => 123, options => #{},
               procedure => Procedure}},
            {[?REGISTERED, 123, 456],
             #{type => registered, request_id => 123, registration_id => 456}},
            {[?UNREGISTER, 123, 456],
             #{type => unregister, request_id => 123, registration_id => 456 }},
            {[?UNREGISTERED, 123],
             #{type => unregistered, request_id => 123}},
            {[?INVOCATION, 123, 456, #{}],
             #{type => invocation, request_id => 123, registration_id => 456,
              details => #{}}},
            {[?INVOCATION, 123, 456, #{}, Arg],
             #{type => invocation, request_id => 123, registration_id => 456,
              details => #{}, arguments => Arg}},
            {[?INVOCATION, 123, 456, #{}, [], ArgKw],
             #{type => invocation, request_id => 123, registration_id => 456,
              details => #{}, arguments => [], arguments_kw => ArgKw}},
            {[?YIELD, 123, #{}],
             #{type => yield, request_id => 123, options => #{}}},
            {[?YIELD, 123, #{}, Arg],
             #{type => yield, request_id => 123, options => #{},
               arguments => Arg}},
            {[?YIELD, 123, #{}, [], ArgKw],
             #{type => yield, request_id => 123, options => #{},
               arguments => [], arguments_kw => ArgKw}},
            {[?GOODBYE, #{}, Error],
             #{type => goodbye, details => #{},
               reason => Error }},
            %% ADVANCED MESSAGES
            {[?CHALLENGE, <<"sample method">>, #{}],
             #{type => challenge, extra => #{},
               auth_method => <<"sample method">> }},
            {[?CHALLENGE, <<"wampcra">>, #{}],
             #{type => challenge, extra => #{}, auth_method => wampcra }},
            {[?AUTHENTICATE, <<"AFFE">>, #{}],
             #{type => authenticate, extra => #{},
               signature => <<"AFFE">> }},
            {[?CANCEL, 123, #{}],
             #{type => cancel, options => #{}, request_id => 123 }},
            {[?INTERRUPT, 123, #{}],
             #{type => interrupt, options => #{}, request_id => 123 }}
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
    ?assertEqual(#{type=>hello, realm => <<"test">>,
                   details => #{roles => #{publisher => #{}}}}, Hello).

deserialize_hello_msgpack_test() ->
    WampMsg = <<147,1,196,4,116,101,115,116,129,196,5,114,111,108,101,115,129,
                196,9,112,117,98,108,105,115,104,101,114,128>>,
    {[Hello], Buffer} = ct_msg:parse(WampMsg, msgpack),
    ?assertEqual(<<>>, Buffer),
    ?assertEqual(#{type=>hello, realm => <<"test">>,
                   details => #{roles => #{publisher => #{}}}}, Hello).


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
                       #{type => error, request_type => ErlType,
                         request_id => 123, details => #{}, error => Error}},
                      {[?ERROR, WampType, 123, #{}, Error, Arg],
                       #{type => error, request_type => ErlType,
                         request_id => 123, details => #{}, error => Error,
                        arguments => Arg}},
                      {[?ERROR, WampType, 123, #{}, Error, [], ArgKw],
                       #{type => error, request_type => ErlType,
                         request_id => 123, details => #{}, error => Error,
                        arguments => [], arguments_kw => ArgKw}}
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
               Map when is_map(Map) ->
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

    ?assertEqual(#{type => ping, payload => <<13>>}, PingMsg1),
    ?assertEqual(#{type => ping, payload => Payload}, PingMsg2),
    ?assertEqual(#{type => pong, payload => <<13>>}, PongMsg1),
    ?assertEqual(#{type => pong, payload => Payload}, PongMsg2).
