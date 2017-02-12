%%
%% Copyright (c) 2014-2017 Bas Wegh
%%

-module(ct_train_factory).
-include("ct_train_types.hrl").
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

-spec hello(RealmName, Details) -> ct_train_hello() when
      RealmName :: binary(),
      Details :: map().
hello(RealmName, Details) ->
    #{type => hello, realm => RealmName, details => Details}.

-spec challenge(AuthMethod, Extra) -> ct_train_challenge() when
      AuthMethod :: binary() | atom(),
      Extra :: map().
challenge(AuthMethod, Extra) ->
    #{type => challenge, auth_method => AuthMethod, extra => Extra}.

-spec authenticate(Signature, Extra) -> ct_train_authenticate() when
      Signature :: binary(),
      Extra :: map().
authenticate(Signature, Extra) ->
    #{type => authenticate, signature => Signature, extra => Extra}.

-spec welcome(SessionId , Details) -> ct_train_welcome() when
      SessionId :: pos_integer(),
      Details :: map().
welcome(SessionId, Details) ->
    #{type => welcome, session_id => SessionId, details => Details}.


-spec abort(Details, Reason) -> ct_train_abort() when
      Details :: map(),
      Reason :: binary() | atom().
abort(Details, Reason) ->
    #{type => abort, details => Details, reason => Reason}.

-spec goodbye(Details, Reason) -> ct_train_goodbye() when
      Details :: map(),
      Reason :: binary() | atom().
goodbye(Details, Reason) ->
    #{type => goodbye, details => Details, reason => Reason}.


-spec error(RequestType, RequestId, Details, Error) -> ct_train_error() when
      RequestType :: ct_train_type(),
      RequestId :: pos_integer(),
      Details :: map(),
      Error :: binary() | atom().
error(RequestType, RequestId, Details, Error) ->
    error(RequestType, RequestId, Details, Error, [], #{}).

-spec error(RequestType, RequestId, Details, Error, Arguments) ->
                   ct_train_error() when
      RequestType :: ct_train_type(),
      RequestId :: pos_integer(),
      Details :: map(),
      Error :: binary() | atom(),
      Arguments :: list().
error(RequestType, RequestId, Details, Error, Arguments) ->
    error(RequestType, RequestId, Details, Error, Arguments, #{}).

-spec error(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw) ->
                  ct_train_error() when
      RequestType :: ct_train_type(),
      RequestId :: pos_integer(),
      Details :: map(),
      Error :: binary() | atom(),
      Arguments :: list(),
      ArgumentsKw :: map().
error(RequestType, RequestId, Details, Error, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => error,  request_type => RequestType, request_id => RequestId,
     details => Details, error => Error, arguments => Arguments,
     arguments_kw => ArgumentsKw};
error(RequestType, RequestId, Details, Error, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => error,  request_type => RequestType, request_id => RequestId,
     details => Details, error => Error, arguments => Arguments};
error(RequestType, RequestId, Details, Error, _Arguments, _ArgumentsKw) ->
    #{type => error,  request_type => RequestType, request_id => RequestId,
     details => Details, error => Error}.

-spec publish(RequestId, Options, Topic) -> ct_train_publish() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary().
publish(RequestId, Options, Topic) ->
    publish(RequestId, Options, Topic, [], #{}).

-spec publish(RequestId, Options, Topic, Arguments) -> ct_train_publish() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary(),
      Arguments :: list().
publish(RequestId, Options, Topic, Arguments) ->
    publish(RequestId, Options, Topic, Arguments, #{}).

-spec publish(RequestId, Options, Topic, Arguments, ArgumentsKw) ->
                     ct_train_publish() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary(),
      Arguments :: list(),
      ArgumentsKw :: map().
publish(RequestId, Options, Topic, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => publish, request_id => RequestId, options => Options,
     topic => Topic, arguments => Arguments, arguments_kw => ArgumentsKw};
publish(RequestId, Options, Topic, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => publish, request_id => RequestId, options => Options,
     topic => Topic, arguments => Arguments};
publish(RequestId, Options, Topic, _Arguments, _ArgumentsKw)  ->
    #{type => publish, request_id => RequestId, options => Options,
     topic => Topic}.

-spec published(RequestId, PublicationId) -> ct_train_published() when
      RequestId :: pos_integer(),
      PublicationId :: pos_integer().
published(RequestId, PublicationId) ->
    #{type => published, request_id => RequestId,
      publication_id => PublicationId}.

-spec subscribe(RequestId, Options, Topic) -> ct_train_subscribe() when
      RequestId :: pos_integer(),
      Options :: map(),
      Topic :: binary().
subscribe(RequestId, Options, Topic) ->
    #{type => subscribe, request_id => RequestId,
      options => Options, topic => Topic}.

-spec subscribed(RequestId, SubscriptionId) -> ct_train_subscribed() when
      RequestId :: pos_integer(),
      SubscriptionId :: pos_integer().
subscribed(RequestId, SubscriptionId) ->
    #{type => subscribed, request_id => RequestId,
      subscription_id => SubscriptionId}.

-spec unsubscribe(RequestId, SubscriptionId) -> ct_train_unsubscribe() when
      RequestId :: pos_integer(),
      SubscriptionId :: pos_integer().
unsubscribe(RequestId, SubscriptionId) ->
    #{type => unsubscribe, request_id => RequestId,
      subscription_id => SubscriptionId}.

-spec unsubscribed(RequestId) -> ct_train_unsubscribed() when
      RequestId :: pos_integer().
unsubscribed(RequestId) ->
    #{type => unsubscribed, request_id => RequestId}.

-spec event(SubscriptionId, PublicationId, Details) -> ct_train_event() when
      SubscriptionId :: pos_integer(),
      PublicationId :: pos_integer(),
      Details :: map().
event(SubscriptionId, PublicationId, Details) ->
    event(SubscriptionId, PublicationId, Details, [], #{}).

-spec event(SubscriptionId, PublicationId, Details, Arguments) ->
                   ct_train_event() when
      SubscriptionId :: pos_integer(),
      PublicationId :: pos_integer(),
      Details :: map(),
      Arguments :: list().
event(SubscriptionId, PublicationId, Details, Arguments) ->
    event(SubscriptionId, PublicationId, Details, Arguments, #{}).

-spec event(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw) ->
                  ct_train_event() when
      SubscriptionId :: pos_integer(),
      PublicationId :: pos_integer(),
      Details :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
event(SubscriptionId, PublicationId, Details, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => Details,
      arguments => Arguments, arguments_kw => ArgumentsKw};
event(SubscriptionId, PublicationId, Details, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => Details,
      arguments => Arguments};
event(SubscriptionId, PublicationId, Details, _Arguments, _ArgumentsKw) ->
    #{type => event, subscription_id => SubscriptionId,
      publication_id => PublicationId, details => Details}.

-spec call(RequestId, Options, Procedure) -> ct_train_call() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary().
call(RequestId, Options, Procedure) ->
    call(RequestId, Options, Procedure, [], #{}).

-spec call(RequestId, Options, Procedure, Arguments) -> ct_train_call() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary(),
      Arguments :: list().
call(RequestId, Options, Procedure, Arguments) ->
    call(RequestId, Options, Procedure, Arguments, #{}).


-spec call(RequestId, Options, Procedure, Arguments, ArgumentsKw) ->
                  ct_train_call() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary(),
      Arguments :: list(),
      ArgumentsKw :: map().
call(RequestId, Options, Procedure, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => call, request_id => RequestId, options => Options,
     procedure => Procedure, arguments => Arguments,
     arguments_kw => ArgumentsKw};
call(RequestId, Options, Procedure, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => call, request_id => RequestId, options => Options,
     procedure => Procedure, arguments => Arguments};
call(RequestId, Options, Procedure, _Arguments, _ArgumentsKw) ->
    #{type => call, request_id => RequestId, options => Options,
     procedure => Procedure}.

-spec cancel(RequestId, Options) -> ct_train_cancel() when
      RequestId :: pos_integer(),
      Options :: map().
cancel(RequestId, Options) ->
    #{type => cancel, request_id => RequestId, options => Options}.

-spec result(RequestId, Details) -> ct_train_result() when
      RequestId :: pos_integer(),
      Details :: map().
result(RequestId, Details) ->
    result(RequestId, Details, [], #{}).

-spec result(RequestId, Details, Arguments) -> ct_train_result() when
      RequestId :: pos_integer(),
      Details :: map(),
      Arguments :: list().
result(RequestId, Details, Arguments) ->
    result(RequestId, Details, Arguments, #{}).

-spec result(RequestId, Details, Arguments, ArgumentsKw) -> ct_train_result() when
      RequestId :: pos_integer(),
      Details :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
result(RequestId, Details, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => result, request_id => RequestId, details => Details,
      arguments => Arguments, arguments_kw => ArgumentsKw};
result(RequestId, Details, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => result, request_id => RequestId, details => Details,
      arguments => Arguments};
result(RequestId, Details, _Arguments, _ArgumentsKw) ->
    #{type => result, request_id => RequestId, details => Details}.

-spec register(RequestId, Options, Procedure) -> ct_train_register() when
      RequestId :: pos_integer(),
      Options :: map(),
      Procedure :: binary().
register(RequestId, Options, Procedure) ->
    #{type => register, request_id => RequestId, options => Options,
      procedure => Procedure}.

-spec registered(RequestId, RegistrationId) -> ct_train_registered() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer().
registered(RequestId, RegistrationId) ->
    #{type => registered, request_id => RequestId, registration_id =>
      RegistrationId}.

-spec unregister(RequestId, RegistrationId) -> ct_train_unregister() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer().
unregister(RequestId, RegistrationId) ->
    #{type => unregister, request_id => RequestId,
      registration_id => RegistrationId}.

-spec unregistered(RequestId) -> ct_train_unregistered() when
      RequestId :: pos_integer().
unregistered(RequestId) ->
    #{type => unregistered, request_id => RequestId}.

-spec invocation(RequestId, RegistrationId, Details) -> ct_train_invocation() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer(),
      Details :: map().
invocation(RequestId, RegistrationId, Details) ->
    invocation(RequestId, RegistrationId, Details, [], #{}).

-spec invocation(RequestId, RegistrationId, Details, Arguments) ->
                        ct_train_invocation() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer(),
      Details :: map(),
      Arguments :: list().
invocation(RequestId, RegistrationId, Details, Arguments) ->
    invocation(RequestId, RegistrationId, Details, Arguments, #{}).

-spec invocation(RequestId, RegistrationId, Details, Arguments, ArgumentsKw) ->
                        ct_train_invocation() when
      RequestId :: pos_integer(),
      RegistrationId :: pos_integer(),
      Details :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
invocation(RequestId, RegistrationId, Details, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => Details,
      arguments => Arguments, arguments_kw => ArgumentsKw };
invocation(RequestId, RegistrationId, Details, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => Details,
      arguments => Arguments };
invocation(RequestId, RegistrationId, Details, _Arguments, _ArgumentsKw) ->
    #{type => invocation, request_id => RequestId,
      registration_id => RegistrationId, details => Details}.

-spec interrupt(RequestId, Options) -> ct_train_interrupt() when
      RequestId :: pos_integer(),
      Options :: map().
interrupt(RequestId, Options) ->
    #{type => interrupt, request_id => RequestId, options => Options}.

-spec yield(RequestId, Options) -> ct_train_yield() when
      RequestId :: pos_integer(),
      Options :: map().
yield(RequestId, Options) ->
    yield(RequestId, Options, [], #{}).

-spec yield(RequestId, Options, Arguments) -> ct_train_yield() when
      RequestId :: pos_integer(),
      Options :: map(),
      Arguments :: list().
yield(RequestId, Options, Arguments) ->
    yield(RequestId, Options, Arguments, #{}).

-spec yield(RequestId, Options, Arguments, ArgumentsKw) -> ct_train_yield() when
      RequestId :: pos_integer(),
      Options :: map(),
      Arguments :: list(),
      ArgumentsKw :: map().
yield(RequestId, Options, Arguments, ArgumentsKw)
  when is_map(ArgumentsKw), map_size(ArgumentsKw) > 0 ->
    #{type => yield, request_id => RequestId, options => Options,
      arguments => Arguments, arguments_kw => ArgumentsKw};
yield(RequestId, Options, Arguments, _ArgumentsKw)
  when is_list(Arguments), length(Arguments) > 0 ->
    #{type => yield, request_id => RequestId, options => Options,
      arguments => Arguments};
yield(RequestId, Options, _Arguments, _ArgumentsKw) ->
    #{type => yield, request_id => RequestId, options => Options}.
