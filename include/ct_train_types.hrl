


-type ct_train_type() :: hello | challenge | authenticate | welcome |
                         goodbye | abort | error | publish | published |
                         subscribe | subscribed | unsubscribe |
                         unsubscribed | event | call | cancel | result |
                         register | registered | unregister |
                         unregistered | invocation | interrupt | yield.

-type ct_train_hello() :: #{type => hello,
                        realm => binary(),
                        details => map()}.
-type ct_train_challenge() :: #{type => challenge,
                            auth_method => binary() | atom(),
                            extra => map()}.
-type ct_train_authenticate() :: #{type => authenticate,
                               signature => binary(),
                               extra => map()}.
-type ct_train_welcome() :: #{type => welcome,
                          session_id => pos_integer(),
                          details => map()}.
-type ct_train_abort() :: #{type => abort,
                        reason => binary() | atom(),
                        details => map()}.
-type ct_train_goodbye() :: #{type => goodbye,
                          reason => binary() | atom(),
                          details => map()}.
-type ct_train_error() :: #{type => error,
                        request_type => ct_train_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom()} |
                      #{type => error,
                        request_type => ct_train_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list()} |
                      #{type => error,
                        request_type => ct_train_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list(),
                        arugments_kw => map()}.
-type ct_train_publish() :: #{type => publish,
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
-type ct_train_published() :: #{type => published,
                            request_id => pos_integer(),
                            publication_id => pos_integer()}.
-type ct_train_subscribe() :: #{type => subscribe,
                            request_id => pos_integer(),
                            options => map(),
                            topic => binary()}.
-type ct_train_subscribed() :: #{type => subscribed,
                             request_id => pos_integer(),
                             subscription_id => pos_integer()}.
-type ct_train_unsubscribe() :: #{type => unsubscribe,
                              request_id => pos_integer(),
                              subscription_id => pos_integer()}.
-type ct_train_unsubscribed() :: #{type => unsubscribed,
                               request_id => pos_integer()}.
-type ct_train_event() :: #{type => event,
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
-type ct_train_call() :: #{type => call,
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
-type ct_train_cancel() :: #{type => cancel,
                         request_id => pos_integer(),
                         options => map()}.
-type ct_train_result() :: #{type => result,
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
-type ct_train_register() :: #{type => register,
                           request_id => pos_integer(),
                           options => map(),
                           procedure => binary()}.
-type ct_train_registered() :: #{type => registered,
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type ct_train_unregister() :: #{type => unregister,
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type ct_train_unregistered() :: #{type => unregistered,
                               request_id => pos_integer()}.
-type ct_train_invocation() :: #{type => invocation,
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
-type ct_train_interrupt() :: #{type => interrupt,
                            request_id => pos_integer(),
                            options => map()}.
-type ct_train_yield() :: #{type => yield,
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

-type ct_train() :: ct_train_hello() |
                    ct_train_challenge() |
                    ct_train_authenticate() |
                    ct_train_welcome() |
                    ct_train_abort() |
                    ct_train_goodbye() |
                    ct_train_error() |
                    ct_train_publish() |
                    ct_train_publish() |
                    ct_train_published() |
                    ct_train_subscribe() |
                    ct_train_subscribed() |
                    ct_train_unsubscribe() |
                    ct_train_unsubscribed() |
                    ct_train_event() |
                    ct_train_call() |
                    ct_train_cancel() |
                    ct_train_result() |
                    ct_train_register() |
                    ct_train_registered() |
                    ct_train_unregister() |
                    ct_train_unregistered() |
                    ct_train_invocation() |
                    ct_train_interrupt() |
                    ct_train_yield().
