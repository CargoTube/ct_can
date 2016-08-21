-module(sibo_proto_msg).

-export([
         hello/2,
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

         yield/2,
         yield/3,
         yield/4
        ]).

hello(RealmName, Details) ->
    #{type => hello, realm => RealmName, details => Details}.

welcome(SessionId, Details) ->
    #{type => welcome, session_id => SessionId, details => Details}.

abort(Details, Reason) ->
    #{type => abort, details => Details, reason => Reason}.

goodbye(Details, Reason) ->
    #{type => goodbye, details => Details, reason => Reason}.

error(RequestType, RequestId, Details, Error) ->
    error(RequestType, RequestId, Details, Error, [], #{}).

error(RequestType, RequestId, Details, Error, Arguments) ->
    error(RequestType, RequestId, Details, Error, Arguments, #{}).

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

publish(RequestId, Options, Topic) ->
    publish(RequestId, Options, Topic, [], {}).

publish(RequestId, Options, Topic, Arguments) ->
    publish(RequestId, Options, Topic, Arguments, {}).

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

published(RequestId, PublicationId) ->
    #{type => published, request_id => RequestId,
      publication_id => PublicationId}.

subscribe(RequestId, Options, Topic) ->
    #{type => subscribe, request_id => RequestId,
      options => Options, topic => Topic}.

subscribed(RequestId, SubscriptionId) ->
    #{type => subscribed, request_id => RequestId,
      subscription_id => SubscriptionId}.

unsubscribe(RequestId, SubscriptionId) ->
    #{type => unsubscribe, request_id => RequestId,
      subscription_id => SubscriptionId}.

unsubscribed(RequestId) ->
    #{type => unsubscribed, request_id => RequestId}.

event(SubscriptionId, PublicationId, Details) ->
    event(SubscriptionId, PublicationId, Details, [], {}).

event(SubscriptionId, PublicationId, Details, Arguments) ->
    event(SubscriptionId, PublicationId, Details, Arguments, {}).

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

call(RequestId, Options, Procedure) ->
    call(RequestId, Options, Procedure, [], #{}).

call(RequestId, Options, Procedure, Arguments) ->
    call(RequestId, Options, Procedure, Arguments, #{}).

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


result(RequestId, Details) ->
    result(RequestId, Details, [], #{}).

result(RequestId, Details, Arguments) ->
    result(RequestId, Details, Arguments, #{}).

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

register(RequestId, Options, Procedure) ->
    #{type => register, request_id => RequestId, options => Options,
      procedure => Procedure}.

registered(RequestId, RegistrationId) ->
    #{type => registered, request_id => RequestId, registration_id =>
      RegistrationId}.

unregister(RequestId, RegistrationId) ->
    #{type => unregister, request_id => RequestId,
      registration_id => RegistrationId}.

unregistered(RequestId) ->
    #{type => unregistered, request_id => RequestId}.

invocation(RequestId, RegistrationId, Details) ->
    invocation(RequestId, RegistrationId, Details, [], #{}).

invocation(RequestId, RegistrationId, Details, Arguments) ->
    invocation(RequestId, RegistrationId, Details, Arguments, #{}).

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

yield(RequestId, Options) ->
    yield(RequestId, Options, [], #{}).

yield(RequestId, Options, Arguments) ->
    yield(RequestId, Options, Arguments, #{}).

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

