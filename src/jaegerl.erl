-module(jaegerl).

-export([start_span/1, start_span/2]).
-export([finish_span/0, finish_span/1]).
-export([current_span/0]).
-export([log/1, log/2]).
-export([set_tags/1]).

-export_type([tracer_id/0]).
-export_type([start_span_option/0, start_span_options/0]).
-export_type([finish_span_option/0, finish_span_options/0]).
-export_type([tags/0, tag_name/0, tag_value/0]).
-export_type([span_reference/0, span_reference_type/0]).
-export_type([span/0]).
-export_type([operation_name/0]).
-export_type([log_fields/0, log_field_name/0, log_field_value/0]).

-type tracer_id() :: atom().

-type start_span_options() :: [start_span_option()].

-type start_span_option() :: {tracer, tracer_id()}
                           | {start_time, erlang:timestamp()}
                           | {refs, [span_reference()]}
                           | {tags, tags()}.

-type finish_span_options() :: [finish_span_option()].

-type finish_span_option() :: {finish_time, erlang:timestamp()}
                            | {owner, pid()}.

-type tags() :: #{tag_name() => tag_value()}.
-type tag_name() :: atom() | binary().
-type tag_value() :: atom() | binary() | number() | boolean().

-type span_reference() :: {span_reference_type(), span()}.
-type span_reference_type() :: child_of | follows_from.

-type span() :: jaegerl_span:maybe_span().

-type operation_name() :: atom() | binary().

-type log_fields() :: #{log_field_name() => log_field_value()}.
-type log_field_name() :: atom() | binary().
-type log_field_value() :: atom() | binary() | number() | boolean().

-spec start_span(operation_name()) -> span().
start_span(OperationName) ->
    start_span(OperationName, []).

-spec start_span(operation_name(), start_span_options()) -> ok.
start_span(OperationName, Options) ->
    Ancestors = get_ancestors(),
    Span = jaegerl_span:start_span(OperationName, Options),
    put_ancestors([Span | Ancestors]).

-spec set_tags(tags()) -> ok.
set_tags(Tags) ->
    error(unimplemented, [Tags]).

-spec log(log_fields()) -> ok.
log(Log) ->
    log(Log, os:timestamp()).

-spec log(log_fields(), erlang:timestamp()) -> ok.
log(Log, Time) ->
    error(unimplemented, [Log, Time]).

-spec finish_span() -> ok.
finish_span() ->
    finish_span([]).

-spec finish_span(finish_span_options()) -> ok.
finish_span(Options) ->
    error(unimplemented, [Options]).

-spec current_span() -> span().
current_span() ->
    case get(jaegerl_ancestors) of
        undefined  -> undefined;
        [Span | _] -> Span
    end.

-spec get_ancestors() -> [span()].
get_ancestors() ->
    case get(jaegerl_ancestors) of
        undefined -> [];
        Ancestors -> Ancestors
    end.

-spec put_ancestors([span()]) -> ok.
put_ancestors(Ancestors) ->
    put(jaegerl_ancestors, Ancestors),
    ok.
