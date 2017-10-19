-module(jaegerl_span).

-export([start_span/2]).

-export_type([span/0, maybe_span/0]).

-define(SPAN, ?MODULE).

-record(?SPAN,
        {
          tracer :: jaegerl:tracer_id(),
          operation_name :: jaegerl:operation_name(),
          start_time :: erlang:timestamp(),
          tags :: jaegerl:tags(),
          refs :: [jaegerl:span_reference()],
          baggage_items :: #{},
          trace_id :: integer(), % 128-bit
          span_id :: integer(), % 64-bit
          parent_span_id :: integer(), % 64-bit
          flags :: non_neg_integer(), % 32-bit
          debug_id :: binary() | undefined
        }).

-opaque span() :: #?SPAN{}.
-type maybe_span() :: span() | undefined.

-spec start_span(jaegerl:operation_name(), jaegerl:start_span_options()) -> maybe_span().
start_span(OperationName, Options) ->
    error(unimplemented, [OperationName, Options]).
