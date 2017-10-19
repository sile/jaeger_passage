%% @private
-module(jaegerl_tracer).

-export([start_span/1, start_span/2]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([tracer/0]).
-export_type([start_span_option/0, start_span_options/0]).

-opaque tracer() :: #{}.

-type start_span_options() :: [start_span_option()].
-type start_span_option() :: {tracer, tracer()}.

-define(STATE, ?MODULE).

-record(?STATE,
        {
          watchings = [] :: [Span :: term()],
          reporters = [] :: [module()]
        }).

-spec start_link(jaegerl:tracer_id()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(TracerId) ->
    gen_server:start_link(jaegerl_local_ns:tracer_name(TracerId), ?MODULE, [], []).

-spec start_span(jaegerl:operation_name()) -> jaegerl_span:span().
start_span(OperationName) ->
    start_span(OperationName, []).

-spec start_span(jaeger:operation_name(), start_span_options()) -> jaegerl_span:span().
start_span(OperationName, Options) ->
    error(unimplemented, [OperationName, Options]).

%% @private
init([]) ->
    {ok, #?STATE{}}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
