-module(jaeger_passage_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    jaeger_passage_sup:start_link().

stop(_State) ->
    ok.
