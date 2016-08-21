%%
%% Copyright (c) 2016 Bas Wegh
%%

-define(HELLO(Realm, Options), sibo_proto_msg:hello(Realm, Options)).
-define(WELCOME(SessionId, Details), sibo_proto_msg:welcome(SessionId, Details)).
-define(ABORT(Details, Reason), sibo_proto_msg:abort(Details, Reason)).
-define(GOODBYE(Details, Reason), sibo_proto_msg:goodbye(Details, Reason)).


-define(ERROR(RequestType, RequestId, Details, Error),
        sibo_proto_msg:error(RequestType, RequestId, Details, Error)).
-define(ERROR(RequestType, RequestId, Details, Error, Arguments),
        sibo_proto_msg:error(RequestType, RequestId, Details, Error, Arguments)).
-define(ERROR(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw),
        sibo_proto_msg:error(RequestType, RequestId, Details, Error, Arguments,
                      ArgumentsKw)).

-define(PUBLISH(RequestId, Options, Topic),
        sibo_proto_msg:publish(RequestId, Options, Topic)).
-define(PUBLISH(RequestId, Options, Topic, Arguments),
        sibo_proto_msg:publish(RequestId, Options, Topic, Arguments)).
-define(PUBLISH(RequestId, Options, Topic, Arguments, ArgumentsKw),
        sibo_proto_msg:publish(RequestId, Options, Topic, Arguments, ArgumentsKw)).

-define(PUBLISHED(RequestId, PublicationId),
        sibo_proto_msg:published(RequestId, PublicationId)).

-define(SUBSCRIBE(RequestId, Options, Topic),
        sibo_proto_msg:subscribe(RequestId, Options, Topic)).

-define(SUBSCRIBED(RequestId, SubscriptionId),
        sibo_proto_msg:subscribed(RequestId, SubscriptionId)).

-define(UNSUBSCRIBE(RequestId, SubscriptionId),
        sibo_proto_msg:unsubscribe(RequestId, SubscriptionId)).

-define(UNSUBSCRIBED(RequestId),
        sibo_proto_msg:unsubscribed(RequestId)).

-define(EVENT(SubscriptionId, PublicationId, Details),
        sibo_proto_msg:event(SubscriptionId, PublicationId, Details)).
-define(EVENT(SubscriptionId, PublicationId, Details, Arguments),
        sibo_proto_msg:event(SubscriptionId, PublicationId, Details, Arguments)).
-define(EVENT(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw),
        sibo_proto_msg:event(SubscriptionId, PublicationId, Details, Arguments,
                      ArgumentsKw)).

-define(CALL(RequestId, Options, Procedure),
        sibo_proto_msg:call(RequestId, Options, Procedure)).
-define(CALL(RequestId, Options, Procedure, Arguments),
        sibo_proto_msg:call(RequestId, Options, Procedure, Arguments)).
-define(CALL(RequestId, Options, Procedure, Arguments, ArgumentsKw),
        sibo_proto_msg:call(RequestId, Options, Procedure, Arguments, ArgumentsKw)).

-define(RESULT(RequestId, Details),
        sibo_proto_msg:result(RequestId, Details)).
-define(RESULT(RequestId, Details, Arguments),
        sibo_proto_msg:result(RequestId, Details, Arguments)).
-define(RESULT(RequestId, Details, Arguments, ArgumentsKw),
        sibo_proto_msg:result(RequestId, Details, Arguments, Arguments)).

-define(REGISTER(RequestId, Options, Procedure),
        sibo_proto_msg:register(RequestId, Options, Procedure)).

-define(REGISTERED(RequestId, RegistrationId),
        sibo_proto_msg:registered(RequestId, RegistrationId)).

-define(UNREGISTER(RequestId, RegistrationId),
        sibo_proto_msg:unregister(RequestId, RegistrationId)).

-define(UNREGISTERED(RequestId),
        sibo_proto_msg:unregistered(RequestId)).

-define(INVOCATION(RequestId, RegistrationId, Details),
        sibo_proto_msg:invocation(RequestId, RegistrationId, Details)).
-define(INVOCATION(RequestId, RegistrationId, Details, Arguments),
        sibo_proto_msg:invocation(RequestId, RegistrationId, Details, Arguments)).
-define(INVOCATION(RequestId, RegistrationId, Details, Arguments, ArgumentsKw),
        sibo_proto_msg:invocation(RequestId, RegistrationId, Details, Arguments,
                           ArgumentsKw)).

-define(YIELD(RequestId, Options),
        sibo_proto_msg:yield(RequestId, Options)).
-define(YIELD(RequestId, Options, Arguments),
        sibo_proto_msg:yield(RequestId, Options, Arguments)).
-define(YIELD(RequestId, Options, Arguments, ArgumentsKw),
        sibo_proto_msg:yield(RequestId, Options, Arguments, ArgumentsKw)).
