


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
-type ct_msg_error() :: {error, ct_msg_type(), pos_integer(), map(),
                          ct_msg_error_uri()} |
                      {error, ct_msg_type(), pos_integer(), map(),
                        ct_msg_error_uri(), list()} |
                      {error, ct_msg_type(), pos_integer(), map(),
                        ct_msg_error_uri(), list(), map()} .
-type ct_msg_publish() :: {publish, pos_integer(), map(), ct_msg_uri()}|
                          {publish, pos_integer(), map(), ct_msg_uri(), list()}|
                          {publish, pos_integer(), map(), ct_msg_uri(), list(),
                           map()}.
-type ct_msg_published() :: {published, pos_integer(), pos_integer()}.
-type ct_msg_subscribe() :: {subscribe, pos_integer(), map(), ct_msg_uri()}.
-type ct_msg_subscribed() :: {subscribed, pos_integer(), pos_integer()}.
-type ct_msg_unsubscribe() :: {unsubscribe, pos_integer(), pos_integer()}.
-type ct_msg_unsubscribed() :: {unsubscribed, pos_integer()}.
-type ct_msg_event() :: {event, pos_integer(), pos_integer(), map()} |
                        {event, pos_integer(), pos_integer(), map(), list()} |
                        {event, pos_integer(), pos_integer(), map(), list(),
                         map()}.
-type ct_msg_call() :: {call, pos_integer(), map(), ct_msg_uri()} |
                       {call, pos_integer(), map(), ct_msg_uri(), list()} |
                       {call, pos_integer(), map(), ct_msg_uri(), list(), map()}
                       .
-type ct_msg_cancel() :: {cancel, pos_integer(), map()}.
-type ct_msg_result() :: {result, pos_integer(), map()} |
                         {result, pos_integer(), map(), list()} |
                         {result, pos_integer(), map(), list(), map()}.
-type ct_msg_register() :: {register, pos_integer(), map(), ct_msg_uri()}.
-type ct_msg_registered() :: {registered, pos_integer(), pos_integer()}.
-type ct_msg_unregister() :: {unregister, pos_integer(), pos_integer()}.
-type ct_msg_unregistered() :: {unregistered, pos_integer()}.
-type ct_msg_invocation() :: {invocation, pos_integer(), pos_integer(), map()} |
                             {invocation, pos_integer(), pos_integer(), map(),
                              list()} |
                             {invocation, pos_integer(), pos_integer(), map(),
                              list(), map()}.
-type ct_msg_interrupt() :: {interrupt, pos_integer(), map()}.
-type ct_msg_yield() :: {yield, pos_integer(), map()} |
                        {yield, pos_integer(), map(), list()} |
                        {yield, pos_integer(), map(), list(), map()} .


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
