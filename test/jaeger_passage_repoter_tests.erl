%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(jaeger_passage_repoter_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
basic_udp_test() ->
    {ok, _} = application:ensure_all_started(jaeger_passage),

    %% Starts `udp_reporter'
    {ok, Reporter} = jaeger_passage_reporter:start(udp_reporter),
    [udp_reporter] = jaeger_passage_reporter:which_reporters(),

    %% Registers `test_tracer'
    Context = jaeger_passage_span_context,
    Sampler = passage_sampler_all:new(),
    ok = passage_tracer_registry:register(udp_tracer, Context, Sampler, Reporter),

    %% Starts and finishes spans
    passage_pd:start_span(test_root, [{tracer, udp_tracer}]),
    passage_pd:start_span(test_child),
    passage_pd:log(#{message => "Hello World"}),
    passage_pd:finish_span(),
    passage_pd:finish_span(),
    timer:sleep(50),

    ok = jaeger_passage_reporter:stop(udp_reporter),
    [] = jaeger_passage_reporter:which_reporters(),

    ok = application:stop(jaeger_passage).

basic_http_test() ->
    {ok, _} = application:ensure_all_started(jaeger_passage),

    %% Starts `http_reporter'
    {ok, Reporter} = jaeger_passage_reporter:start(http_reporter, [
        {protocol, http},
        {http_client, fun http_client/5}
    ]),
    [http_reporter] = jaeger_passage_reporter:which_reporters(),

    %% Registers `test_tracer'
    Context = jaeger_passage_span_context,
    Sampler = passage_sampler_all:new(),
    ok = passage_tracer_registry:register(http_tracer, Context, Sampler, Reporter),

    %% Starts and finishes spans
    passage_pd:start_span(test_root, [{tracer, http_tracer}]),
    passage_pd:start_span(test_child),
    passage_pd:log(#{message => "Hello World"}),
    passage_pd:finish_span(),
    passage_pd:finish_span(),
    timer:sleep(50),

    ok = jaeger_passage_reporter:stop(http_reporter),
    [] = jaeger_passage_reporter:which_reporters(),

    ok = application:stop(jaeger_passage).

error_http_test() ->
    {ok, _} = application:ensure_all_started(jaeger_passage),

    %% Starts `http_reporter'
    ?assertMatch({error, {{badarg, _}, _}}, jaeger_passage_reporter:start(http_reporter, [
        {protocol, http},
        {http_client, undefined}
    ])),

    %% Starts `http_reporter'
    ?assertError(badarg, jaeger_passage_reporter:start(http_reporter, [
        {protocol, undefined}
    ])).


http_client(_URI, _Method, _Headers, _Encoded, _Options) ->
    ok.