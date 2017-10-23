-module(jaeger_passage_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    NameServer = jaeger_passage_local_ns:child_spec(),
    ReporterSup = #{
      id    => jaeger_passage_reporter_sup,
      start => {jaeger_passage_reporter_sup, start_link, []},
      type  => supervisor
     },
    {ok, {#{strategy => rest_for_one}, [NameServer, ReporterSup]} }.
