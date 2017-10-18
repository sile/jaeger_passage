-module(jaegerl_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    TracerSup = #{
      id    => jaegerl_tracer_sup,
      start => {jaegerl_tracer_sup, start_link, []},
      type  => supervisor
     },
    {ok, {#{}, [TracerSup]} }.
