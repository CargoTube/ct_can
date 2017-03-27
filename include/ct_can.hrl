%%
%% Copyright (c) 2016 Bas Wegh
%%

-define(HELLO(Realm, Options), ct_can_factory:hello(Realm, Options)).
-define(WELCOME(SessionId, Details), ct_can_factory:welcome(SessionId, Details)).
-define(CHALLENGE(AuthMethod, Extra), ct_can_factory:challenge(AuthMethod, Extra)).
-define(AUTHENTICATE(Signature, Extra), ct_can_factory:authenticate(Signature, Extra)).
-define(ABORT(Details, Reason), ct_can_factory:abort(Details, Reason)).
-define(GOODBYE(Details, Reason), ct_can_factory:goodbye(Details, Reason)).


-define(ERROR(RequestType, RequestId, Details, Error),
        ct_can_factory:error(RequestType, RequestId, Details, Error)).
-define(ERROR(RequestType, RequestId, Details, Error, Arguments),
        ct_can_factory:error(RequestType, RequestId, Details, Error, Arguments)).
-define(ERROR(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw),
        ct_can_factory:error(RequestType, RequestId, Details, Error, Arguments,
                      ArgumentsKw)).

-define(PUBLISH(RequestId, Options, Topic),
        ct_can_factory:publish(RequestId, Options, Topic)).
-define(PUBLISH(RequestId, Options, Topic, Arguments),
        ct_can_factory:publish(RequestId, Options, Topic, Arguments)).
-define(PUBLISH(RequestId, Options, Topic, Arguments, ArgumentsKw),
        ct_can_factory:publish(RequestId, Options, Topic, Arguments, ArgumentsKw)).

-define(PUBLISHED(RequestId, PublicationId),
        ct_can_factory:published(RequestId, PublicationId)).

-define(SUBSCRIBE(RequestId, Options, Topic),
        ct_can_factory:subscribe(RequestId, Options, Topic)).

-define(SUBSCRIBED(RequestId, SubscriptionId),
        ct_can_factory:subscribed(RequestId, SubscriptionId)).

-define(UNSUBSCRIBE(RequestId, SubscriptionId),
        ct_can_factory:unsubscribe(RequestId, SubscriptionId)).

-define(UNSUBSCRIBED(RequestId),
        ct_can_factory:unsubscribed(RequestId)).

-define(EVENT(SubscriptionId, PublicationId, Details),
        ct_can_factory:event(SubscriptionId, PublicationId, Details)).
-define(EVENT(SubscriptionId, PublicationId, Details, Arguments),
        ct_can_factory:event(SubscriptionId, PublicationId, Details, Arguments)).
-define(EVENT(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw),
        ct_can_factory:event(SubscriptionId, PublicationId, Details, Arguments,
                      ArgumentsKw)).

-define(CALL(RequestId, Options, Procedure),
        ct_can_factory:call(RequestId, Options, Procedure)).
-define(CALL(RequestId, Options, Procedure, Arguments),
        ct_can_factory:call(RequestId, Options, Procedure, Arguments)).
-define(CALL(RequestId, Options, Procedure, Arguments, ArgumentsKw),
        ct_can_factory:call(RequestId, Options, Procedure, Arguments, ArgumentsKw)).

-define(CANCEL(RequestId, Options), ct_can_factory:cancel(RequestId, Options)).

-define(RESULT(RequestId, Details),
        ct_can_factory:result(RequestId, Details)).
-define(RESULT(RequestId, Details, Arguments),
        ct_can_factory:result(RequestId, Details, Arguments)).
-define(RESULT(RequestId, Details, Arguments, ArgumentsKw),
        ct_can_factory:result(RequestId, Details, Arguments, ArgumentsKw)).

-define(REGISTER(RequestId, Options, Procedure),
        ct_can_factory:register(RequestId, Options, Procedure)).

-define(REGISTERED(RequestId, RegistrationId),
        ct_can_factory:registered(RequestId, RegistrationId)).

-define(UNREGISTER(RequestId, RegistrationId),
        ct_can_factory:unregister(RequestId, RegistrationId)).

-define(UNREGISTERED(RequestId),
        ct_can_factory:unregistered(RequestId)).

-define(INVOCATION(RequestId, RegistrationId, Details),
        ct_can_factory:invocation(RequestId, RegistrationId, Details)).
-define(INVOCATION(RequestId, RegistrationId, Details, Arguments),
        ct_can_factory:invocation(RequestId, RegistrationId, Details, Arguments)).
-define(INVOCATION(RequestId, RegistrationId, Details, Arguments, ArgumentsKw),
        ct_can_factory:invocation(RequestId, RegistrationId, Details, Arguments,
                           ArgumentsKw)).

-define(INTERRUPT(RequestId, Options), ct_can_factory:interrupt(RequestId, Options)).

-define(YIELD(RequestId, Options),
        ct_can_factory:yield(RequestId, Options)).
-define(YIELD(RequestId, Options, Arguments),
        ct_can_factory:yield(RequestId, Options, Arguments)).
-define(YIELD(RequestId, Options, Arguments, ArgumentsKw),
        ct_can_factory:yield(RequestId, Options, Arguments, ArgumentsKw)).
