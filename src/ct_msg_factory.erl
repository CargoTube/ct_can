%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-module(ct_msg_factory).
-include("ct_msg_types.hrl").
-export([
         hello/2,
         challenge/2,
         authenticate/2,
         welcome/2,
         abort/2,
         goodbye/2,

         error/4,
         error/5,
         error/6,

         publish/3,
         publish/4,
         publish/5,

         published/2,
         subscribe/3,
         subscribed/2,
         unsubscribe/2,
         unsubscribed/1,

         event/3,
         event/4,
         event/5,

         call/3,
         call/4,
         call/5,

         cancel/2,

         result/2,
         result/3,
         result/4,

         register/3,
         registered/2,
         unregister/2,
         unregistered/1,

         invocation/3,
         invocation/4,
         invocation/5,

         interrupt/2,

         yield/2,
         yield/3,
         yield/4
        ]).

-spec hello(RealmName, Details) -> ct_msg_hello() when
      RealmName :: binary(),
      Details :: map().
hello(RealmName, Details) ->
    {hello, RealmName, Details}.

-spec challenge(AuthMethod, Extra) -> ct_msg_challenge() when
      AuthMethod :: binary() | atom(),
      Extra :: map().
challenge(AuthMethod, Extra) ->
    {challenge, AuthMethod, Extra}.

-spec authenticate(Signature, Extra) -> ct_msg_authenticate() when
      Signature :: binary(),
      Extra :: map().
authenticate(Signature, Extra) ->
    {authenticate, Signature, Extra}.

-spec welcome(SessionId , Details) -> ct_msg_welcome() when
      SessionId :: pos_integer(),
      Details :: map().
welcome(SessionId, Details) ->
    {welcome, SessionId, Details}.


-spec abort(Details, Reason) -> ct_msg_abort() when
      Details :: map(),
      Reason :: binary() | atom().
abort(Details, Reason) ->
    {abort, Details, Reason}.

-spec goodbye(Details, Reason) -> ct_msg_goodbye() when
      Details :: map(),
      Reason :: binary() | atom().
goodbye(Details, Reason) ->
    {goodbye, Details, Reason}.


-spec error(RequestType, RequestId, Details, Error) -> ct_msg_error() when
      RequestType :: ct_msg_type(),
      RequestId :: pos_integer(),
      Details :: map(),
      Error :: binary() | atom().
error(RequestType, RequestId, Details, Error) ->
    error(RequestType, RequestId, Details, Error, [], #{}).

-spec error(RequestType, RequestId, Details, Error, Arguments) ->
                   ct_msg_error() when
      RequestType :: ct_msg_type(),
      RequestId :: pos_integer(),
      Details :: map(),
      Error :: binary() | atom(),
      Arguments :: list().
error(RequestType, RequestId, Details, Error, Arguments) ->
    error(RequestType, RequestId, Details, Error, Arguments, #{}).

-spec error(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw) ->
                  ct_msg_error() when
      RequestType :: ct_msg_type(),
      RequestId :: pos_integer(),
      Details :: map(),
      Error :: binary() | atom(),
      Arguments :: list(),
      ArgumentsKw :: map().
error(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {error, RequestType, RequestId, Details, Error, Arguments, ArgumentsKw};
error(RequestType, RequestId, Details, Error, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {error, RequestType, RequestId, Details, Error, Arguments, undefined};
error(RequestType, RequestId, Details, Error, _Arguments, _ArgumentsKw) ->
    {error, RequestType, RequestId, Details, Error, undefined, undefined}.

-spec publish(RequestId, Options, Topic) -> ct_msg_publish() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary().
publish(RequestId, Options, Topic) ->
    publish(RequestId, Options, Topic, [], #{}).

-spec publish(RequestId, Options, Topic, Arguments) -> ct_msg_publish() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary(),
      Arguments :: list().
publish(RequestId, Options, Topic, Arguments) ->
    publish(RequestId, Options, Topic, Arguments, #{}).

-spec publish(RequestId, Options, Topic, Arguments, ArgumentsKw) ->
                     ct_msg_publish() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary(),
      Arguments :: list(),
      ArgumentsKw :: map().
publish(RequestId, Options, Topic, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {publish, RequestId, Options, Topic, Arguments, ArgumentsKw};
publish(RequestId, Options, Topic, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {publish, RequestId, Options, Topic, Arguments, undefined};
publish(RequestId, Options, Topic, _Arguments, _ArgumentsKw)  ->
    {publish, RequestId, Options, Topic, undefined, undefined}.

-spec published(RequestId, PublicationId) -> ct_msg_published() when
      RequestId :: pos_integer(),
      PublicationId :: pos_integer().
published(RequestId, PublicationId) ->
    {published, RequestId, PublicationId}.

-spec subscribe(RequestId, Options, Topic) -> ct_msg_subscribe() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary().
subscribe(RequestId, Options, Topic) ->
    {subscribe, RequestId, Options, Topic}.

-spec subscribed(RequestId, SubscriptionId) -> ct_msg_subscribed() when
      RequestId :: pos_integer(),
      SubscriptionId :: pos_integer().
subscribed(RequestId, SubscriptionId) ->
    {subscribed, RequestId, SubscriptionId}.

-spec unsubscribe(RequestId, SubscriptionId) -> ct_msg_unsubscribe() when
      RequestId :: pos_integer(),
      SubscriptionId :: pos_integer().
unsubscribe(RequestId, SubscriptionId) ->
    {unsubscribe, RequestId, SubscriptionId}.

-spec unsubscribed(RequestId) -> ct_msg_unsubscribed() when
      RequestId :: pos_integer().
unsubscribed(RequestId) ->
    {unsubscribed, RequestId}.

-spec event(SubscriptionId, PublicationId, Details) -> ct_msg_event() when
      SubscriptionId :: pos_integer(),
      PublicationId :: pos_integer(),
      Details :: map().
event(SubscriptionId, PublicationId, Details) ->
    event(SubscriptionId, PublicationId, Details, [], #{}).

-spec event(SubscriptionId, PublicationId, Details, Arguments) ->
                   ct_msg_event() when
      SubscriptionId :: pos_integer(),
      PublicationId :: pos_integer(),
      Details :: map(),
      Arguments :: list().
event(SubscriptionId, PublicationId, Details, Arguments) ->
    event(SubscriptionId, PublicationId, Details, Arguments, #{}).

-spec event(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw) ->
                  ct_msg_event() when
      SubscriptionId :: pos_integer(),
      PublicationId :: pos_integer(),
      Details :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
event(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {event, SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw};
event(SubscriptionId, PublicationId, Details, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {event, SubscriptionId, PublicationId, Details, Arguments, undefined};
event(SubscriptionId, PublicationId, Details, _Arguments, _ArgumentsKw) ->
    {event, SubscriptionId, PublicationId, Details, undefined, undefined}.

-spec call(RequestId, Options, Procedure) -> ct_msg_call() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary().
call(RequestId, Options, Procedure) ->
    call(RequestId, Options, Procedure, [], #{}).

-spec call(RequestId, Options, Procedure, Arguments) -> ct_msg_call() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary(),
      Arguments :: list().
call(RequestId, Options, Procedure, Arguments) ->
    call(RequestId, Options, Procedure, Arguments, #{}).


-spec call(RequestId, Options, Procedure, Arguments, ArgumentsKw) ->
                  ct_msg_call() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary(),
      Arguments :: list(),
      ArgumentsKw :: map().
call(RequestId, Options, Procedure, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {call, RequestId, Options, Procedure, Arguments, ArgumentsKw};
call(RequestId, Options, Procedure, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {call, RequestId, Options, Procedure, Arguments, undefined};
call(RequestId, Options, Procedure, _Arguments, _ArgumentsKw) ->
    {call, RequestId, Options, Procedure, undefined, undefined}.

-spec cancel(RequestId, Options) -> ct_msg_cancel() when
      RequestId :: pos_integer(),
      Options :: map().
cancel(RequestId, Options) ->
    {cancel, RequestId, Options}.

-spec result(RequestId, Details) -> ct_msg_result() when
      RequestId :: pos_integer(),
      Details :: map().
result(RequestId, Details) ->
    result(RequestId, Details, [], #{}).

-spec result(RequestId, Details, Arguments) -> ct_msg_result() when
      RequestId :: pos_integer(),
      Details :: map(),
      Arguments :: list().
result(RequestId, Details, Arguments) ->
    result(RequestId, Details, Arguments, #{}).

-spec result(RequestId, Details, Arguments, ArgumentsKw) ->
                    ct_msg_result() when
      RequestId :: pos_integer(),
      Details :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
result(RequestId, Details, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {result, RequestId, Details, Arguments, ArgumentsKw};
result(RequestId, Details, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {result, RequestId, Details, Arguments, undefined};
result(RequestId, Details, _Arguments, _ArgumentsKw) ->
    {result, RequestId, Details, undefined, undefined}.

-spec register(RequestId, Options, Procedure) -> ct_msg_register() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary().
register(RequestId, Options, Procedure) ->
    {register, RequestId, Options, Procedure}.

-spec registered(RequestId, RegistrationId) -> ct_msg_registered() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer().
registered(RequestId, RegistrationId) ->
    {registered, RequestId, RegistrationId}.

-spec unregister(RequestId, RegistrationId) -> ct_msg_unregister() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer().
unregister(RequestId, RegistrationId) ->
    {unregister, RequestId, RegistrationId}.

-spec unregistered(RequestId) -> ct_msg_unregistered() when
      RequestId :: pos_integer().
unregistered(RequestId) ->
    {unregistered, RequestId}.

-spec invocation(RequestId, RegistrationId, Details) ->
                        ct_msg_invocation() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer(),
      Details :: map().
invocation(RequestId, RegistrationId, Details) ->
    invocation(RequestId, RegistrationId, Details, [], #{}).

-spec invocation(RequestId, RegistrationId, Details, Arguments) ->
                        ct_msg_invocation() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer(),
      Details :: map(),
      Arguments :: list().
invocation(RequestId, RegistrationId, Details, Arguments) ->
    invocation(RequestId, RegistrationId, Details, Arguments, #{}).

-spec invocation(RequestId, RegistrationId, Details, Arguments, ArgumentsKw) ->
                        ct_msg_invocation() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer(),
      Details :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
invocation(RequestId, RegistrationId, Details, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {invocation, RequestId, RegistrationId, Details, Arguments, ArgumentsKw };
invocation(RequestId, RegistrationId, Details, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {invocation, RequestId, RegistrationId, Details, Arguments , undefined};
invocation(RequestId, RegistrationId, Details, _Arguments, _ArgumentsKw) ->
    {invocation, RequestId, RegistrationId, Details , undefined, undefined}.

-spec interrupt(RequestId, Options) -> ct_msg_interrupt() when
      RequestId :: pos_integer(),
      Options :: map().
interrupt(RequestId, Options) ->
    {interrupt, RequestId, Options}.

-spec yield(RequestId, Options) -> ct_msg_yield() when
      RequestId :: pos_integer(),
      Options :: map().
yield(RequestId, Options) ->
    yield(RequestId, Options, [], #{}).

-spec yield(RequestId, Options, Arguments) -> ct_msg_yield() when
      RequestId :: pos_integer(),
      Options :: map(),
      Arguments :: list().
yield(RequestId, Options, Arguments) ->
    yield(RequestId, Options, Arguments, #{}).

-spec yield(RequestId, Options, Arguments, ArgumentsKw) -> ct_msg_yield() when
      RequestId :: pos_integer(),
      Options :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
yield(RequestId, Options, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    {yield, RequestId, Options, Arguments, ArgumentsKw};
yield(RequestId, Options, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    {yield, RequestId, Options, Arguments, undefined};
yield(RequestId, Options, _Arguments, _ArgumentsKw) ->
    {yield, RequestId, Options, undefined, undefined}.
