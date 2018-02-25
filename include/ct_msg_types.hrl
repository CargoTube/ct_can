


-type ct_msg_type() :: hello | challenge | authenticate | welcome |
                         goodbye | abort | error | publish | published |
                         subscribe | subscribed | unsubscribe |
                         unsubscribed | event | call | cancel | result |
                         register | registered | unregister |
                         unregistered | invocation | interrupt | yield.

-type ct_msg_uri() :: binary().
-type ct_msg_error_uri() :: binary() | atom().
-type ct_msg_authmetod() :: binary() | atom().

-type ct_msg_hello() :: {hello, binary(), map()}.
-type ct_msg_challenge() :: {challenge, ct_msg_authmetod(), map()}.
-type ct_msg_authenticate() :: {authenticate, binary(), map()}.
-type ct_msg_welcome() :: {welcome, non_neg_integer(), map()}.
-type ct_msg_abort() :: {abort, map(), ct_msg_error_uri()}.
-type ct_msg_goodbye() :: {goodbye, map(), ct_msg_error_uri()}.
-type ct_msg_error() :: {error, ct_msg_type(), non_neg_integer(), map(),
                          ct_msg_error_uri()} |
                      {error, ct_msg_type(), non_neg_integer(), map(),
                        ct_msg_error_uri(), list()} |
                      {error, ct_msg_type(), non_neg_integer(), map(),
                        ct_msg_error_uri(), list(), map()} .
-type ct_msg_publish() :: {publish, non_neg_integer(), map(), ct_msg_uri()}|
                          {publish, non_neg_integer(), map(), ct_msg_uri(),
                           list()}|
                          {publish, non_neg_integer(), map(), ct_msg_uri(),
                           list(), map()}.
-type ct_msg_published() :: {published, non_neg_integer(), non_neg_integer()}.
-type ct_msg_subscribe() :: {subscribe, non_neg_integer(), map(), ct_msg_uri()}.
-type ct_msg_subscribed() :: {subscribed, non_neg_integer(), non_neg_integer()}.
-type ct_msg_unsubscribe() :: {unsubscribe, non_neg_integer(),
                               non_neg_integer()}.
-type ct_msg_unsubscribed() :: {unsubscribed, non_neg_integer()}.
-type ct_msg_event() :: {event, non_neg_integer(), non_neg_integer(), map()} |
                        {event, non_neg_integer(), non_neg_integer(), map(),
                         list()} |
                        {event, non_neg_integer(), non_neg_integer(), map(),
                         list(), map()}.
-type ct_msg_call() :: {call, non_neg_integer(), map(), ct_msg_uri()} |
                       {call, non_neg_integer(), map(), ct_msg_uri(), list()} |
                       {call, non_neg_integer(), map(), ct_msg_uri(), list(),
                        map()}
                       .
-type ct_msg_cancel() :: {cancel, non_neg_integer(), map()}.
-type ct_msg_result() :: {result, non_neg_integer(), map()} |
                         {result, non_neg_integer(), map(), list()} |
                         {result, non_neg_integer(), map(), list(), map()}.
-type ct_msg_register() :: {register, non_neg_integer(), map(), ct_msg_uri()}.
-type ct_msg_registered() :: {registered, non_neg_integer(), non_neg_integer()}.
-type ct_msg_unregister() :: {unregister, non_neg_integer(), non_neg_integer()}.
-type ct_msg_unregistered() :: {unregistered, non_neg_integer()}.
-type ct_msg_invocation() :: {invocation, non_neg_integer(), non_neg_integer(),
                              map()} |
                             {invocation, non_neg_integer(), non_neg_integer(),
                              map(), list()} |
                             {invocation, non_neg_integer(), non_neg_integer(),
                              map(), list(), map()}.
-type ct_msg_interrupt() :: {interrupt, non_neg_integer(), map()}.
-type ct_msg_yield() :: {yield, non_neg_integer(), map()} |
                        {yield, non_neg_integer(), map(), list()} |
                        {yield, non_neg_integer(), map(), list(), map()} .


-type ct_msg() :: ct_msg_hello() |
                  ct_msg_challenge() |
                  ct_msg_authenticate() |
                  ct_msg_welcome() |
                  ct_msg_abort() |
                  ct_msg_established().

-type ct_msg_established() :: ct_msg_goodbye() |
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
