


-type ct_can_type() :: hello | challenge | authenticate | welcome |
                         goodbye | abort | error | publish | published |
                         subscribe | subscribed | unsubscribe |
                         unsubscribed | event | call | cancel | result |
                         register | registered | unregister |
                         unregistered | invocation | interrupt | yield.

-type ct_can_hello() :: #{type => hello,
                        realm => binary(),
                        details => map()}.
-type ct_can_challenge() :: #{type => challenge,
                            auth_method => binary() | atom(),
                            extra => map()}.
-type ct_can_authenticate() :: #{type => authenticate,
                               signature => binary(),
                               extra => map()}.
-type ct_can_welcome() :: #{type => welcome,
                          session_id => pos_integer(),
                          details => map()}.
-type ct_can_abort() :: #{type => abort,
                        reason => binary() | atom(),
                        details => map()}.
-type ct_can_goodbye() :: #{type => goodbye,
                          reason => binary() | atom(),
                          details => map()}.
-type ct_can_error() :: #{type => error,
                        request_type => ct_can_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom()} |
                      #{type => error,
                        request_type => ct_can_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list()} |
                      #{type => error,
                        request_type => ct_can_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list(),
                        arugments_kw => map()}.
-type ct_can_publish() :: #{type => publish,
                          request_id => pos_integer(),
                          options => map(),
                          topic => binary()} |
                        #{type => publish,
                          request_id => pos_integer(),
                          options => map(),
                          topic => binary(),
                          arguments => list()} |
                        #{type => publish,
                          request_id => pos_integer(),
                          options => map(),
                          topic => binary(),
                          arguments => list(),
                          arguments_kw => map()}.
-type ct_can_published() :: #{type => published,
                            request_id => pos_integer(),
                            publication_id => pos_integer()}.
-type ct_can_subscribe() :: #{type => subscribe,
                            request_id => pos_integer(),
                            options => map(),
                            topic => binary()}.
-type ct_can_subscribed() :: #{type => subscribed,
                             request_id => pos_integer(),
                             subscription_id => pos_integer()}.
-type ct_can_unsubscribe() :: #{type => unsubscribe,
                              request_id => pos_integer(),
                              subscription_id => pos_integer()}.
-type ct_can_unsubscribed() :: #{type => unsubscribed,
                               request_id => pos_integer()}.
-type ct_can_event() :: #{type => event,
                        subscription_id => pos_integer(),
                        publication_id => pos_integer(),
                        details => map()} |
                      #{type => event,
                        subscription_id => pos_integer(),
                        publication_id => pos_integer(),
                        details => map(),
                        arugments => list()} |
                      #{type => event,
                        subscription_id => pos_integer(),
                        publication_id => pos_integer(),
                        details => map(),
                        arguments => list(),
                        arguments_kw => map()}.
-type ct_can_call() :: #{type => call,
                       request_id => pos_integer(),
                       procedure => binary(),
                       options => map()} |
                     #{type => call,
                       request_id => pos_integer(),
                       procedure => binary(),
                       options => map(),
                       arguments => list()} |
                     #{type => call,
                       request_id => pos_integer(),
                       procedure => binary(),
                       options => map(),
                       arguments => list(),
                       arguments_kw => map()}.
-type ct_can_cancel() :: #{type => cancel,
                         request_id => pos_integer(),
                         options => map()}.
-type ct_can_result() :: #{type => result,
                         request_id => pos_integer(),
                         details => map()} |
                       #{type => result,
                         request_id => pos_integer(),
                         details => map(),
                         arguments => list()} |
                       #{type => result,
                         request_id => pos_integer(),
                         details => map(),
                         arguments => list(),
                         arguments_kw => map()}.
-type ct_can_register() :: #{type => register,
                           request_id => pos_integer(),
                           options => map(),
                           procedure => binary()}.
-type ct_can_registered() :: #{type => registered,
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type ct_can_unregister() :: #{type => unregister,
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type ct_can_unregistered() :: #{type => unregistered,
                               request_id => pos_integer()}.
-type ct_can_invocation() :: #{type => invocation,
                             request_id => pos_integer(),
                             registration_id => pos_integer(),
                             details => map()} |
                           #{type => invocation,
                             request_id => pos_integer(),
                             registration_id => pos_integer(),
                             details => map(),
                             arguments => list()} |
                           #{type => invocation,
                             request_id => pos_integer(),
                             registration_id => pos_integer(),
                             details => map(),
                             arguments => list(),
                             arguments_kw => map()}.
-type ct_can_interrupt() :: #{type => interrupt,
                            request_id => pos_integer(),
                            options => map()}.
-type ct_can_yield() :: #{type => yield,
                        request_id => pos_integer(),
                        options => map()} |
                      #{type => yield,
                        request_id => pos_integer(),
                        options => map(),
                        arguments => list()} |
                      #{type => yield,
                        request_id => pos_integer(),
                        options => map(),
                        arguments => list(),
                        arguments_kw => map()}.

-type ct_can() :: ct_can_hello() |
                    ct_can_challenge() |
                    ct_can_authenticate() |
                    ct_can_welcome() |
                    ct_can_abort() |
                    ct_can_goodbye() |
                    ct_can_error() |
                    ct_can_publish() |
                    ct_can_publish() |
                    ct_can_published() |
                    ct_can_subscribe() |
                    ct_can_subscribed() |
                    ct_can_unsubscribe() |
                    ct_can_unsubscribed() |
                    ct_can_event() |
                    ct_can_call() |
                    ct_can_cancel() |
                    ct_can_result() |
                    ct_can_register() |
                    ct_can_registered() |
                    ct_can_unregister() |
                    ct_can_unregistered() |
                    ct_can_invocation() |
                    ct_can_interrupt() |
                    ct_can_yield().
