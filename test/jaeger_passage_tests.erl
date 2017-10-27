%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(jaeger_passage_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
basic_test() ->
    {ok, _} = application:ensure_all_started(jaeger_passage),

    %% Starts `test_tracer'
    Sampler = passage_sampler_all:new(),
    ok = jaeger_passage:start_tracer(test_tracer, Sampler),

    [test_tracer] = passage_tracer_registry:which_tracers(),
    [test_tracer] = jaeger_passage_reporter:which_reporters(),

    %% Starts and finishes spans
    passage_pd:start_span(test_root, [{tracer, test_tracer}]),
    passage_pd:start_span(test_child),
    passage_pd:log(#{message => "Hello World"}),
    passage_pd:finish_span(),
    passage_pd:finish_span(),
    timer:sleep(50),

    %% Stops `test_tracer'
    ok = jaeger_passage:stop_tracer(test_tracer),
    [] = passage_tracer_registry:which_tracers(),
    [] = jaeger_passage_reporter:which_reporters(),

    ok = application:stop(jaeger_passage).
