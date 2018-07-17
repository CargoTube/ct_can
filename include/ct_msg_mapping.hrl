%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-define(ERROR_MAPPING,
        [ {authorization_failed, <<"wamp.error.authorization_failed">>},
          {canceled, <<"wamp.error.canceled">>},
          {close_realm, <<"wamp.error.close_realm">>},
          {disclose_not_allowed, <<"wamp.error.disclose_me.not_allowed">>},
          {goodbye_and_out, <<"wamp.error.goodbye_and_out">>},
          {invalid_argument, <<"wamp.error.invalid_argument">>},
          {invalid_uri, <<"wamp.error.invalid_uri">>},
          {network_failure, <<"wamp.error.network_failure">>},
          {no_eligible_callee, <<"wamp.error.no_eligible_callee">>},
          {no_such_procedure, <<"wamp.error.no_such_procedure">>},
          {no_such_realm, <<"wamp.error.no_such_realm">>},
          {no_such_registration, <<"wamp.error.no_such_registration">>},
          {no_such_role, <<"wamp.error.no_such_role">>},
          {no_such_session, <<"wamp.error.no_such_session">>},
          {no_such_subscription, <<"wamp.error.no_such_subscription">>},
          {not_authorized, <<"wamp.error.not_authorized">>},
          {option_disclose_me, <<"wamp.error.option_disallowed.disclose_me">>},
          {option_not_allowed, <<"wamp.error.option_not_allowed">>},
          {procedure_already_exists, <<"wamp.error.procedure_already_exists">>},
          {system_shutdown, <<"wamp.error.system_shutdown">>}
        ]).

-define(AUTH_METHOD_MAPPING,
        [ {wampcra, <<"wampcra">>},
          {ticket, <<"ticket">>}
        ]).

-define(REQUEST_TYPE_MAPPING,
        [{?SUBSCRIBE, subscribe},
        {?UNSUBSCRIBE, unsubscribe},
        {?PUBLISH, publish},
        {?REGISTER, register},
        {?UNREGISTER, unregister},
        {?CALL, call},
        {?INVOCATION, invocation}]).
