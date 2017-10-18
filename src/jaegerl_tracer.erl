-module(jaegerl_tracer).

-export([start_span/1, start_span/2]).

-export_type([tracer/0]).
-export_type([start_span_option/0, start_span_options/0]).

-opaque tracer() :: #{}.

-type start_span_options() :: [start_span_option()].
-type start_span_option() :: {tracer, tracer()}.

-spec start_span(jaegerl:operation_name()) -> jaegerl_span:span().
start_span(OperationName) ->
    start_span(OperationName, []).

-spec start_span(jaeger:operation_name(), start_span_options()) -> jaegerl_span:span().
start_span(OperationName, Options) ->
    error(unimplemented, [OperationName, Options]).
