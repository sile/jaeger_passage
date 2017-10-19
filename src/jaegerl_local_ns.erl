-module(jaegerl_local_ns).

-export([child_spec/0]).
-export([tracer_name/1]).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    local:name_server_child_spec(?MODULE).

-spec tracer_name(jaegerl:tracer_id()) -> local:otp_name().
tracer_name(TracerId) ->
    local:otp_name({?MODULE, {tracer, TracerId}}).
