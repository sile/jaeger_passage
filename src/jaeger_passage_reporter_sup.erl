%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(jaeger_passage_reporter_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_child/3]).
-export([stop_child/1]).
-export([which_children/0]).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(ReporterId, ReporterModule, Options) -> {ok, pid()} | {error, Reason} when
      ReporterId :: jaeger_passage_reporter:reporter_id(),
      ReporterModule :: module(),
      Options :: jaeger_passage_reporter:start_options(),
      Reason :: {already_started, pid()} | term().
start_child(ReporterId, ReporterModule, Options) ->
    Child = #{
      id      => ReporterId,
      start   => {ReporterModule, start_link, [ReporterId, Options]},
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

%%------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------
init([]) ->
    {ok, {#{}, []}}.
