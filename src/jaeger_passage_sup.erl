%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(jaeger_passage_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/0]).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%-------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------
init([]) ->
    NameServer = jaeger_passage_local_ns:child_spec(),
    ReporterSup = #{
      id    => jaeger_passage_reporter_sup,
      start => {jaeger_passage_reporter_sup, start_link, []},
      type  => supervisor
     },
    {ok, {#{strategy => rest_for_one}, [NameServer, ReporterSup]} }.
