-module(jaegerl_tracer_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Child = #{
      id    => jaegerl_tracer,
      start => {jaegerl_tracer, start_link, []}
     },
    {ok, {#{strategy => simple_one_for_one}, [Child]} }.
