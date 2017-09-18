-module(ct_msg_factory_test).
-include_lib("eunit/include/eunit.hrl").
-include("ct_msg.hrl").

hello_test() ->
    Msg = ?HELLO(<<"test">>,#{}),
    validate(Msg, hello).

welcome_test() ->
    Msg = ?WELCOME(123,#{}),
    validate(Msg, welcome).

challenge_test() ->
    Msg = ?CHALLENGE(<<"some_auth_msg">>,#{}),
    validate(Msg, challenge).

authenticate_test() ->
    Msg = ?AUTHENTICATE(<<"signature">>,#{}),
    validate(Msg, authenticate).

abort_test() ->
    Msg = ?ABORT(#{},<<"error.msg">>),
    validate(Msg, abort).

goodbye_test() ->
    Msg = ?GOODBYE(#{},<<"error.msg">>),
    validate(Msg, goodbye).

error_test() ->
    Msg1 = ?ERROR(call, 123, #{}, <<"some.error">>),
    Msg2 = ?ERROR(call, 123, #{}, <<"some.error">>, [456]),
    Msg3 = ?ERROR(call, 123, #{}, <<"some.error">>, [], #{key => value}),
    validate(Msg1, error),
    validate_arg(Msg2, error),
    validate_argkw(Msg3, error).

publish_test() ->
    Msg1 = ?PUBLISH(123, #{}, <<"topic">>),
    Msg2 = ?PUBLISH(123, #{}, <<"topic">>, [345]),
    Msg3 = ?PUBLISH(123, #{}, <<"topic">>, [], #{key => value}),
    validate(Msg1, publish),
    validate_arg(Msg2, publish),
    validate_argkw(Msg3, publish).


published_test() ->
    Msg = ?PUBLISHED(123, 456),
    validate(Msg, published).

subscribe_test() ->
    Msg = ?SUBSCRIBE(123, #{}, <<"some.topic">>),
    validate(Msg, subscribe).

subscribed_test() ->
    Msg = ?SUBSCRIBED(123, 456),
    validate(Msg, subscribed).

unsubscribe_test() ->
    Msg = ?UNSUBSCRIBE(123, 456),
    validate(Msg, unsubscribe).

unsubscribed_test() ->
    Msg = ?UNSUBSCRIBED(123),
    validate(Msg, unsubscribed).

event_test() ->
    Msg1 = ?EVENT(123, 456, #{}),
    Msg2 = ?EVENT(123, 456, #{}, [345]),
    Msg3 = ?EVENT(123, 456, #{}, [], #{key => value}),
    validate(Msg1, event),
    validate_arg(Msg2, event),
    validate_argkw(Msg3, event).


call_test() ->
    Msg1 = ?CALL(123, #{}, <<"some.procedure">>),
    Msg2 = ?CALL(123, #{}, <<"some.procedure">>, [456]),
    Msg3 = ?CALL(123, #{}, <<"some.procedure">>, [], #{key => value}),
    validate(Msg1, call),
    validate_arg(Msg2, call),
    validate_argkw(Msg3, call).

cancel_test() ->
    Msg = ?CANCEL(123,#{}),
    validate(Msg, cancel).

interrupt_test() ->
    Msg = ?INTERRUPT(123,#{}),
    validate(Msg, interrupt).

result_test() ->
    Msg1 = ?RESULT(123, #{}),
    Msg2 = ?RESULT(123, #{}, [456]),
    Msg3 = ?RESULT(123, #{}, [], #{key => value}),
    validate(Msg1, result),
    validate_arg(Msg2, result),
    validate_argkw(Msg3, result).

register_test() ->
    Msg = ?REGISTER(123, #{}, <<"some.topic">>),
    validate(Msg, register).


registered_test() ->
    Msg = ?REGISTERED(123, 456),
    validate(Msg, registered).

unregister_test() ->
    Msg = ?UNREGISTER(123, 456),
    validate(Msg, unregister).

unregistered_test() ->
    Msg = ?UNREGISTERED(123),
    validate(Msg, unregistered).

invocation_test() ->
    Msg1 = ?INVOCATION(123, 456, #{}),
    Msg2 = ?INVOCATION(123, 456, #{}, [456]),
    Msg3 = ?INVOCATION(123, 456, #{}, [], #{key => value}),
    validate(Msg1, invocation),
    validate_arg(Msg2, invocation),
    validate_argkw(Msg3, invocation).

yield_test() ->
    Msg1 = ?YIELD(123, #{}),
    Msg2 = ?YIELD(123, #{}, [456]),
    Msg3 = ?YIELD(123, #{}, [], #{key => value}),
    validate(Msg1, yield),
    validate_arg(Msg2, yield),
    validate_argkw(Msg3, yield).

validate_arg(Msg, Type) ->
    validate(Msg, Type, [arguments]).

validate_argkw(Msg, Type) ->
    validate(Msg, Type, [arguments, arguments_kw]).

validate(Msg, Type) ->
    validate(Msg, Type, []).

validate(Msg, Type, MustKeys) ->
    io:format("msg: ~p~n",[Msg]),
    ?assertEqual(Type, ct_msg:get_type(Msg)),
    ?assertEqual(true, ct_msg_validation:is_valid(Msg)),
    ContainsKey = fun(Key, Map) ->
                          io:format("  testing for key ~p~n",[Key]),
                          ?assertEqual(true, maps:is_key(Key, Map)),
                          Map
                  end,
    lists:foldl(ContainsKey, Msg, MustKeys).
