%%
%% Copyright (c) 2016 Bas Wegh
%%

-define(HELLO(Realm, Options), sbp_msg:hello(Realm, Options)).
-define(WELCOME(SessionId, Details), sbp_msg:welcome(SessionId, Details)).
-define(ABORT(Details, Reason), sbp_msg:abort(Details, Reason)).
-define(GOODBYE(Details, Reason), sbp_msg:goodbye(Details, Reason)).


-define(ERROR(RequestType, RequestId, Details, Error),
        sbp_msg:error(RequestType, RequestId, Details, Error)).
-define(ERROR(RequestType, RequestId, Details, Error, Arguments),
        sbp_msg:error(RequestType, RequestId, Details, Error, Arguments)).
-define(ERROR(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw),
        sbp_msg:error(RequestType, RequestId, Details, Error, Arguments,
                      ArgumentsKw)).

-define(PUBLISH(RequestId, Options, Topic),
        sbp_msg:publish(RequestId, Options, Topic)).
-define(PUBLISH(RequestId, Options, Topic, Arguments),
        sbp_msg:publish(RequestId, Options, Topic, Arguments)).
-define(PUBLISH(RequestId, Options, Topic, Arguments, ArgumentsKw),
        sbp_msg:publish(RequestId, Options, Topic, Arguments, ArgumentsKw)).

-define(PUBLISHED(RequestId, PublicationId),
        sbp_msg:published(RequestId, PublicationId)).

-define(SUBSCRIBE(RequestId, Options, Topic),
        sbp_msg:subscribe(RequestId, Options, Topic)).

-define(SUBSCRIBED(RequestId, SubscriptionId),
        sbp_msg:subscribed(RequestId, SubscriptionId)).

-define(UNSUBSCRIBE(RequestId, SubscriptionId),
        sbp_msg:unsubscribe(RequestId, SubscriptionId)).

-define(UNSUBSCRIBED(RequestId),
        sbp_msg:unsubscribed(RequestId)).

-define(EVENT(SubscriptionId, PublicationId, Details),
        sbp_msg:event(SubscriptionId, PublicationId, Details)).
-define(EVENT(SubscriptionId, PublicationId, Details, Arguments),
        sbp_msg:event(SubscriptionId, PublicationId, Details, Arguments)).
-define(EVENT(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw),
        sbp_msg:event(SubscriptionId, PublicationId, Details, Arguments,
                      ArgumentsKw)).

-define(CALL(RequestId, Options, Procedure),
        sbp_msg:call(RequestId, Options, Procedure)).
-define(CALL(RequestId, Options, Procedure, Arguments),
        sbp_msg:call(RequestId, Options, Procedure, Arguments)).
-define(CALL(RequestId, Options, Procedure, Arguments, ArgumentsKw),
        sbp_msg:call(RequestId, Options, Procedure, Arguments, ArgumentsKw)).

-define(RESULT(RequestId, Details),
        sbp_msg:result(RequestId, Details)).
-define(RESULT(RequestId, Details, Arguments),
        sbp_msg:result(RequestId, Details, Arguments)).
-define(RESULT(RequestId, Details, Arguments, ArgumentsKw),
        sbp_msg:result(RequestId, Details, Arguments, Arguments)).

-define(REGISTER(RequestId, Options, Procedure),
        sbp_msg:register(RequestId, Options, Procedure)).

-define(REGISTERED(RequestId, RegistrationId),
        sbp_msg:registered(RequestId, RegistrationId)).

-define(UNREGISTER(RequestId, RegistrationId),
        sbp_msg:unregister(RequestId, RegistrationId)).

-define(UNREGISTERED(RequestId),
        sbp_msg:unregistered(RequestId)).

-define(INVOCATION(RequestId, RegistrationId, Details),
        sbp_msg:invocation(RequestId, RegistrationId, Details)).
-define(INVOCATION(RequestId, RegistrationId, Details, Arguments),
        sbp_msg:invocation(RequestId, RegistrationId, Details, Arguments)).
-define(INVOCATION(RequestId, RegistrationId, Details, Arguments, ArgumentsKw),
        sbp_msg:invocation(RequestId, RegistrationId, Details, Arguments,
                           ArgumentsKw)).

-define(YIELD(RequestId, Options),
        sbp_msg:yield(RequestId, Options)).
-define(YIELD(RequestId, Options, Arguments),
        sbp_msg:yield(RequestId, Options, Arguments)).
-define(YIELD(RequestId, Options, Arguments, ArgumentsKw),
        sbp_msg:yield(RequestId, Options, Arguments, ArgumentsKw)).
