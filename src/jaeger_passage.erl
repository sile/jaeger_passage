%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Utility Functions
-module(jaeger_passage).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_tracer/2, start_tracer/3]).
-export([stop_tracer/1]).

-export_type([start_tracer_option/0, start_tracer_options/0]).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type start_tracer_options() :: [start_tracer_option()].
%% Options for {@link start_tracer/3}.

-type start_tracer_option() :: {reporter_id, jaeger_passage_reporter:reporter_id()}
                             | jaeger_passage_reporter:start_option().
%% <ul>
%%   <li><b>reporter_id</b>: In {@link start_tracer/3} function, it starts a reporter with this identifier. The default value is `Tracer'.</li>
%% </ul>

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start_tracer(TracerId, Sampler, [])
-spec start_tracer(Tracer, Sampler) -> ok | {error, Reason :: term()} when
      Tracer :: passage:tracer_id(),
      Sampler :: passage_sampler:sampler().
start_tracer(Tracer, Sampler) ->
    start_tracer(Tracer, Sampler, []).

%% @doc Starts a tracer for Jaeger.
%%
%% This function also starts a reporter for the jaeger agent.
%%
%% Conceptually this is equivalent to the following code:
%%
%% ```
%% ReporterId = proplists:get_value(reporter_id, Options, Tracer),
%% Context = jaeger_passage_span_context,
%% {ok, Reporter} = jaeger_passage_reporter:start(ReporterId, Options),
%% passage_tracer_registry:register(Tracer, Context, Sampler, Reporter).
%% '''
%%
%% === Examples ===
%%
%% ```
%% %% Starts `test_tracer'
%% Sampler = passage_sampler_all:new().
%% ok = jaeger_passage:start_tracer(example_tracer, Sampler).
%%
%% [example_tracer] = passage_tracer_registry:which_tracers().
%% [example_tracer] = jaeger_passage_reporter:which_reporters().
%%
%% %% Starts and finishes spans
%% passage_pd:start_span(example_span, [{tracer, example_tracer}]).
%% passage_pd:log(#{message => "something wrong"}, [error]).
%% passage_pd:finish_span().
%%
%% %% Stops `example_tracer'
%% ok = jaeger_passage:stop_tracer(example_tracer).
%% [] = passage_tracer_registry:which_tracers().
%% [] = jaeger_passage_reporter:which_reporters().
%% '''
-spec start_tracer(Tracer, Sampler, Options) -> ok | {error, Reason :: term()} when
      Tracer :: passage:tracer_id(),
      Sampler :: passage_sampler:sampler(),
      Options :: start_tracer_options().
start_tracer(Tracer, Sampler, Options) ->
    ReporterId = proplists:get_value(reporter_id, Options, Tracer),
    Context = jaeger_passage_span_context,
    case jaeger_passage_reporter:start(ReporterId, Options) of
        {error, Reason} -> {error, Reason};
        {ok, Reporter}  ->
            passage_tracer_registry:register(Tracer, Context, Sampler, Reporter)
    end.

%% @doc Stops the tracer which has been started by {@link start_tracer/3}.
-spec stop_tracer(passage:tracer_id()) -> ok.
stop_tracer(Tracer) ->
    case passage_tracer_registry:get_reporter(Tracer) of
        error          -> ok;
        {ok, Reporter} ->
            ReporterId = jaeger_passage_reporter:get_id(Reporter),
            jaeger_passage_reporter:stop(ReporterId),
            passage_tracer_registry:deregister(Tracer)
    end.
