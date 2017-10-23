-module(jaeger_passage_reporter_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2]).
-export([stop_child/1]).
-export([which_children/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(jaeger_passage_reporter:reporter_id(), jaeger_passage_reporter:start_options()) ->
                         {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_child(ReporterId, Options) ->
    Child = #{
      id      => ReporterId,
      start   => {jaeger_passage_reporter, start_link, [ReporterId, Options]},
      restart => permanent
     },
    supervisor:start_child(?MODULE, Child).

-spec stop_child(jaeger_passage_reporter:reporter_id()) -> ok.
stop_child(ReporterId) ->
    _ = supervisor:terminate_child(?MODULE, ReporterId),
    _ = supervisor:delete_child(?MODULE, ReporterId),
    ok.

-spec which_children() -> [jaeger_passage_reporter:reporter_id()].
which_children() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

init([]) ->
    {ok, {#{}, []}}.
