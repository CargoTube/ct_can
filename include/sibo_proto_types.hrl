


-type sibo_msg_type() :: hello | challenge | authenticate | welcome |
                         goodbye | abort | error | publish | published |
                         subscribe | subscribed | unsubscribe |
                         unsubscribed | event | call | cancel | result |
                         register | registered | unregister |
                         unregistered | invocation | interrupt | yield.

-type sibo_hello() :: #{type => hello, 
                        realm => binary(), 
                        details => map()}. 
-type sibo_challenge() :: #{type => challenge, 
                            auth_method => binary() | atom(), 
                            details => map()}. 
-type sibo_authenticate() :: #{type => authenticate, 
                               signature => binary(),
                               extra => map()}.
-type sibo_welcome() :: #{type => welcome, 
                          session_id => pos_integer(),
                          details => map()}.
-type sibo_abort() :: #{type => abort, 
                        reason => binary() | atom(),
                        details => map()}.
-type sibo_goodbye() :: #{type => goodbye, 
                          reason => binary() | atom(),
                          details => map()}.
-type sibo_error() :: #{type => error, 
                        request_type => sibo_msg_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom()} |
                      #{type => error, 
                        request_type => sibo_msg_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list()} |
                      #{type => error, 
                        request_type => sibo_msg_type(),
                        request_id => pos_integer(),
                        details => map(),
                        error => binary() | atom(),
                        arguments => list(),
                        arugments_kw => map()}.
-type sibo_publish() :: #{type => publish, 
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
-type sibo_published() :: #{type => published, 
                            request_id => pos_integer(),
                            publication_id => pos_integer()}.
-type sibo_subscribe() :: #{type => subscribe, 
                            request_id => pos_integer(),
                            options => map(),
                            topic => binary()}.
-type sibo_subscribed() :: #{type => subscribed, 
                             request_id => pos_integer(),
                             subscription_id => pos_integer()}.
-type sibo_unsubscribe() :: #{type => unsubscribe, 
                              request_id => pos_integer(),
                              subscription_id => pos_integer()}.
-type sibo_unsubscribed() :: #{type => unsubscribed, 
                               request_id => pos_integer()}.
-type sibo_event() :: #{type => event, 
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
-type sibo_call() :: #{type => call, 
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
-type sibo_cancel() :: #{type => cancel, 
                         request_id => pos_integer(),
                         options => map()}.
-type sibo_result() :: #{type => result, 
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
-type sibo_register() :: #{type => register, 
                           request_id => pos_integer(),
                           options => map(),
                           procedure => binary()}.
-type sibo_registered() :: #{type => registered, 
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type sibo_unregister() :: #{type => unregister, 
                             request_id => pos_integer(),
                             registration_id => pos_integer()}.
-type sibo_unregistered() :: #{type => unregistered, 
                               request_id => pos_integer()}.
-type sibo_invocation() :: #{type => invocation, 
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
-type sibo_interrupt() :: #{type => interrupt, 
                            request_id => pos_integer(),
                            options => map()}.
-type sibo_yield() :: #{type => yield, 
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

-type sibo_msg() :: sibo_hello() |
                    sibo_challenge() |
                    sibo_authenticate() |
                    sibo_welcome() |
                    sibo_abort() |
                    sibo_goodbye() |
                    sibo_error() |
                    sibo_publish() |
                    sibo_publish() |
                    sibo_published() |
                    sibo_subscribe() |
                    sibo_subscribed() |
                    sibo_unsubscribe() |
                    sibo_unsubscribed() |
                    sibo_event() |
                    sibo_call() |
                    sibo_cancel() |
                    sibo_result() |
                    sibo_register() |
                    sibo_registered() |
                    sibo_unregister() |
                    sibo_unregistered() |
                    sibo_invocation() |
                    sibo_interrupt() |
                    sibo_yield().

