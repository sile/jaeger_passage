-module(jaegerl_tracer_sup).

-behaviour(supervisor).

-export([start_child/1]).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_child(jaegerl:tracer_id()) -> {ok, pid()} | {error, Reason :: term()}.
start_child(TracerId) ->
    Child = #{
      id      => jaegerl_tracer,
      start   => {jaegerl_tracer, start_link, [TracerId]},
      type    => worker,
      restart => permanent
     },
    supervisor:start_child(?MODULE, Child).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {#{}, []} }.
