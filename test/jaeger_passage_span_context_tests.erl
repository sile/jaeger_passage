%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
-module(jaeger_passage_span_context_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Test Cases
%%------------------------------------------------------------------------------
propagation_test_() ->
    {foreach,
     fun () -> {ok, _} = application:ensure_all_started(jaeger_passage) end,
     fun (_) ->
             ok = application:stop(jaeger_passage),
             ok = application:stop(passage)
     end,
     [
      {"Binary format",
       fun () ->
               ok = start_tracer(),
               Span0 = passage:start_span(foo, [{tracer, tracer}]),
               Span1 = passage:set_baggage_items(Span0, #{<<"a">> => <<"b">>}),

               #{<<"binary">> := Injected} =
                   passage:inject_span(Span1, binary, fun maps:put/3, #{}),

               Extracted =
                   passage:extract_span(
                     tracer, binary,
                     fun iterate_list/1, [{<<"binary">>, Injected}]),

               ?assertEqual(
                  passage_span:get_context(Span1),
                  passage_span:get_context(Extracted))
       end},
      {"TextMap format",
       fun () ->
               ok = start_tracer(),
               Span0 = passage:start_span(foo, [{tracer, tracer}]),
               Span1 = passage:set_baggage_items(Span0, #{<<"a">> => <<"b">>}),

               #{<<"uber-trace-id">> := _, <<"uberctx-a">> := _} = Injected =
                   passage:inject_span(Span1, text_map, fun maps:put/3, #{}),

               Extracted =
                   passage:extract_span(
                     tracer, text_map,
                     fun iterate_list/1, maps:to_list(Injected)),

               ?assertEqual(
                  passage_span:get_context(Span1),
                  passage_span:get_context(Extracted))
       end},
      {"HttpHeader format",
       fun () ->
               ok = start_tracer(),
               Span0 = passage:start_span(foo, [{tracer, tracer}]),
               Span1 = passage:set_baggage_items(Span0, #{<<"a">> => <<"b">>}),

               #{<<"uber-trace-id">> := _, <<"uberctx-a">> := _} = Injected =
                   passage:inject_span(Span1, http_header, fun maps:put/3, #{}),

               Extracted =
                   passage:extract_span(
                     tracer, http_header,
                     fun iterate_list/1, maps:to_list(Injected)),

               ?assertEqual(
                  passage_span:get_context(Span1),
                  passage_span:get_context(Extracted))
       end},
      {"Debug ID",
       fun () ->
               ok = start_tracer(),
               Span =
                   passage:extract_span(
                     tracer, text_map,
                     fun iterate_list/1, [{<<"jaeger-debug-id">>, <<"foo">>}]),
               Context = passage_span:get_context(Span),

               ?assertEqual(0, jaeger_passage_span_context:get_span_id(Context)),
               ?assertEqual({ok, <<"foo">>},
                            jaeger_passage_span_context:get_debug_id(Context))
       end},
      {"Baggage Items",
       fun () ->
               ok = start_tracer(),
               Span =
                   passage:extract_span(
                     tracer, text_map,
                     fun iterate_list/1, [{<<"jaeger-debug-id">>, <<"foo">>},
                                          {<<"jaeger-baggage">>, <<"a=b, 1=2">>}]),
               ?assertEqual(#{<<"a">> => <<"b">>, <<"1">> => <<"2">>},
                            passage:get_baggage_items(Span))
       end}
     ]}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec start_tracer() -> ok.
start_tracer() ->
    Context = jaeger_passage_span_context,
    Sampler = passage_sampler_all:new(),
    Reporter = passage_reporter_null:new(),
    ok = passage_tracer_registry:register(tracer, Context, Sampler, Reporter).

-spec iterate_list(list()) -> {ok, binary(), binary(), list()} | error.
iterate_list([])              -> error;
iterate_list([{K, V} | List]) -> {ok, K, V, List}.
