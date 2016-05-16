-module(validator_test).
-include_lib("eunit/include/eunit.hrl").
-include("sbp_message_codes.hrl").

hello_test() ->
    Msg = #{type => hello, details => #{}, realm => <<"test.uri">>},
    true = sbp_validator:is_valid_message(Msg),
    ok.

