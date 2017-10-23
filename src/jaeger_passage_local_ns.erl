-module(jaeger_passage_local_ns).

-export([child_spec/0]).
-export([reporter_name/1]).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    local:name_server_child_spec(?MODULE).

-spec reporter_name(jaeger_passage_reporter:reporter_id()) -> local:otp_name().
reporter_name(TracerId) ->
    local:otp_name({?MODULE, {reporeter, TracerId}}).
