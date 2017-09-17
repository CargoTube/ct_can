


-type ct_msg_type() :: hello | challenge | authenticate | welcome |
                         goodbye | abort | error | publish | published |
                         subscribe | subscribed | unsubscribe |
                         unsubscribed | event | call | cancel | result |
                         register | registered | unregister |
                         unregistered | invocation | interrupt | yield.

-type ct_msg_hello() :: #{type => hello,
                        realm => binary(),
                        details => map()}.
-type ct_msg_challenge() :: #{type => challenge,
                            auth_method => binary() | atom(),
                            extra => map()}.
-type ct_msg_authenticate() :: #{type => authenticate,
                               signature => binary(),
                               extra => map()}.
-type ct_msg_welcome() :: #{type => welcome,
                          session_id => pos_integer(),
                          details => map()}.
-type ct_msg_abort() :: #{type => abort,
                        reason => binary() | atom(),
                        details => map()}.
-type ct_msg_goodbye() :: #{type => goodbye,
                          reason => binary() | atom(),
                          details => map()}.
-type ct_msg_error() :: #{type => error,
                        request_type => ct_msg_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom()} |
                      #{type => error,
                        request_type => ct_msg_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list()} |
                      #{type => error,
                        request_type => ct_msg_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list(),
                        arugments_kw => map()}.
-type ct_msg_publish() :: #{type => publish,
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
-type ct_msg_published() :: #{type => published,
                            request_id => pos_integer(),
                            publication_id => pos_integer()}.
-type ct_msg_subscribe() :: #{type => subscribe,
                            request_id => pos_integer(),
                            options => map(),
                            topic => binary()}.
-type ct_msg_subscribed() :: #{type => subscribed,
                             request_id => pos_integer(),
                             subscription_id => pos_integer()}.
-type ct_msg_unsubscribe() :: #{type => unsubscribe,
                              request_id => pos_integer(),
                              subscription_id => pos_integer()}.
-type ct_msg_unsubscribed() :: #{type => unsubscribed,
                               request_id => pos_integer()}.
-type ct_msg_event() :: #{type => event,
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
-type ct_msg_call() :: #{type => call,
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
-type ct_msg_cancel() :: #{type => cancel,
                         request_id => pos_integer(),
                         options => map()}.
-type ct_msg_result() :: #{type => result,
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
-type ct_msg_register() :: #{type => register,
                           request_id => pos_integer(),
                           options => map(),
                           procedure => binary()}.
-type ct_msg_registered() :: #{type => registered,
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type ct_msg_unregister() :: #{type => unregister,
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type ct_msg_unregistered() :: #{type => unregistered,
                               request_id => pos_integer()}.
-type ct_msg_invocation() :: #{type => invocation,
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
-type ct_msg_interrupt() :: #{type => interrupt,
                            request_id => pos_integer(),
                            options => map()}.
-type ct_msg_yield() :: #{type => yield,
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

-type ct_msg() :: ct_msg_hello() |
                    ct_msg_challenge() |
                    ct_msg_authenticate() |
                    ct_msg_welcome() |
                    ct_msg_abort() |
                    ct_msg_goodbye() |
                    ct_msg_error() |
                    ct_msg_publish() |
                    ct_msg_publish() |
                    ct_msg_published() |
                    ct_msg_subscribe() |
                    ct_msg_subscribed() |
                    ct_msg_unsubscribe() |
                    ct_msg_unsubscribed() |
                    ct_msg_event() |
                    ct_msg_call() |
                    ct_msg_cancel() |
                    ct_msg_result() |
                    ct_msg_register() |
                    ct_msg_registered() |
                    ct_msg_unregister() |
                    ct_msg_unregistered() |
                    ct_msg_invocation() |
                    ct_msg_interrupt() |
                    ct_msg_yield().
