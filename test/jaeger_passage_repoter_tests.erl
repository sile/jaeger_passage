%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(jaeger_passage_repoter_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
basic_test() ->
    {ok, _} = application:ensure_all_started(jaeger_passage),

    %% Starts `test_reporter'
    {ok, Reporter} = jaeger_passage_reporter:start(test_reporter),
    [test_reporter] = jaeger_passage_reporter:which_reporters(),

    %% Registers `test_tracer'
    Context = jaeger_passage_span_context,
    Sampler = passage_sampler_all:new(),
    ok = passage_tracer_registry:register(test_tracer, Context, Sampler, Reporter),

    %% Starts and finishes spans
    passage_pd:start_span(test_root, [{tracer, test_tracer}]),
    passage_pd:start_span(test_child),
    passage_pd:log(#{message => "Hello World"}),
    passage_pd:finish_span(),
    passage_pd:finish_span(),
    timer:sleep(50),

    ok = jaeger_passage_reporter:stop(test_reporter),
    [] = jaeger_passage_reporter:which_reporters(),

    ok = application:stop(jaeger_passage).
