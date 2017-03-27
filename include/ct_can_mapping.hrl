%%
%% Copyright (c) 2014-2016 Bas Wegh
%%

-define(ERROR_MAPPING,
        [ {goodbye_and_out, <<"wamp.error.goodbye_and_out">>},
          {authorization_failed, <<"wamp.error.authorization_failed">>},
          {canceled, <<"wamp.error.canceled">>},
          {close_realm, <<"wamp.error.close_realm">>},
          {disclose_not_allowed, <<"wamp.error.disclose_me.not_allowed">>},
          {invalid_argument, <<"wamp.error.invalid_argument">>},
          {invalid_uri, <<"wamp.error.invalid_uri">>},
          {no_such_procedure, <<"wamp.error.no_such_procedure">>},
          {no_such_realm, <<"wamp.error.no_such_realm">>},
          {no_such_registration, <<"wamp.error.no_such_registration">>},
          {no_such_role, <<"wamp.error.no_such_role">>},
          {no_such_subscription, <<"wamp.error.no_such_subscription">>},
          {not_authorized, <<"wamp.error.not_authorized">>},
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
