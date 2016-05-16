-module(convert_test).
-include_lib("eunit/include/eunit.hrl").
-include("sbp_message_codes.hrl").

hello_test() ->
    Msg = [?HELLO, <<"test.uri">>, #{}],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := hello, details := #{}, realm := <<"test.uri">>} = ErlMsg,
    ok.

welcome_test() ->
    Msg = [?WELCOME, 234, #{}],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := welcome, details := #{}, session_id := 234} = ErlMsg,
    ok.

abort_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

%% error_test() ->
%%     Msg = [?ABORT, #{}, <<"wamp.error.test">>],
%%     ErlMsg = sbp_converter:to_erl(Msg),
%%     #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
%%     ok.

publish_test() ->
    Msg1 = [?PUBLISH, 456, #{}, <<"test.topic">>],
    Msg2 = [?PUBLISH, 456, #{}, <<"test.topic">>, []],
    Msg3 = [?PUBLISH, 456, #{}, <<"test.topic">>, [], #{}],
    ErlMsg1 = sbp_converter:to_erl(Msg1),
    ErlMsg2 = sbp_converter:to_erl(Msg2),
    ErlMsg3 = sbp_converter:to_erl(Msg3),
    #{type := publish, options := #{}, topic := <<"test.topic">>,
      request_id := 456} = ErlMsg1,
    #{type := publish, options := #{}, topic := <<"test.topic">>,
      request_id := 456, arguments := []} = ErlMsg2,
    #{type := publish, options := #{}, topic := <<"test.topic">>,
      request_id := 456, arguments := [], arguments_kw := #{}} = ErlMsg3,
    ok.

published_test() ->
    Msg = [?PUBLISHED, 123, 456],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := published, request_id := 123, publication_id := 456} = ErlMsg,
    ok.

subscribe_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

subscribed_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

unsubscribe_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

unsubscribed_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

event_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

call_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

result_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

register_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

registered_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

unregister_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

unregistered_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

invocation_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

yield_test() ->
    Msg = [?ABORT, #{}, <<"wamp.error.test">>],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := abort, details := #{}, reason := <<"wamp.error.test">>} = ErlMsg,
    ok.

goodbye_test() ->
    Msg = [?HELLO, <<"test.uri">>, #{}],
    ErlMsg = sbp_converter:to_erl(Msg),
    #{type := hello, details := #{}, realm := <<"test.uri">>} = ErlMsg,
    ok.
