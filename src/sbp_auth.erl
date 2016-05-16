%%
%% Copyright (c) 2015-2016 Bas Wegh
%%
-module(sbp_auth).
-author("Bas Wegh, bwegh@github.com").

-export([wamp_cra/2]).
-export([pbkdf2/4]).
-export([create_wampcra_challenge/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc calculates the cryptographic hash of the challenge by using the
%% secret key.
-spec wamp_cra(Key :: binary(), Challenge :: binary()) -> binary().
wamp_cra(Key, Challenge) ->
  Bin = crypto:hmac(sha256, Key, Challenge),
  base64:encode(Bin).

%% @doc calculates the derived key from secret key, using salt and iterations.
-spec pbkdf2(SecretKey :: binary(), Salt :: binary(),
    Iterations :: non_neg_integer(),
    Length :: non_neg_integer()) -> {ok, NewKey :: binary()}.
pbkdf2(SecretKey, Salt, Iterations, Length) ->
  pbkdf2:pbkdf2(SecretKey, Salt, Iterations, Length).

-spec create_wampcra_challenge(AuthProvider :: binary(), AuthId :: binary(),
                               Authrole :: binary(),
    Session :: non_neg_integer()) -> {ok, Challenge :: binary(),
                                      calendar:timestamp()}.
create_wampcra_challenge(AuthProvider, AuthId, Authrole, Session) ->
  Now = os:timestamp(),
  {{Year, Month, Day},
   {Hour, Minute, Seconds}} = calendar:now_to_universal_time(Now),
  Timestamp = list_to_binary(
                io_lib:format(
                  "~.10B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.000Z",
                  [Year, Month, Day, Hour, Minute, Seconds])),
  Challenge = jsx:encode([{<<"nonce">>, nonce()},
                          {<<"authprovider">>, AuthProvider},
    {<<"authid">>, AuthId}, {<<"timestamp">>, Timestamp},
    {<<"authrole">>, Authrole}, {<<"authmethod">>, <<"wampcra">>},
    {<<"session">>, Session}]),
  {ok, Challenge, Now}.


%% @private
nonce() ->
  base64:encode(crypto:strong_rand_bytes(15)).
