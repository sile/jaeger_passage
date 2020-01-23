%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A reporter that sends the spans to an jaeger agent
%%
%% === Examples ===
%%
%% ```
%% %% Starts `example_reporter'
%% {ok, Reporter} = jaeger_passage_reporter:start(example_reporter).
%% [example_reporter] = jaeger_passage_reporter:which_reporters().
%%
%% %% Registers `example_tracer'
%% Context = jaeger_passage_span_context.
%% Sampler = passage_sampler_all:new().
%% ok = passage_tracer_registry:register(example_tracer, Context, Sampler, Reporter).
%%
%% %% Starts and finishes a span
%% Span = passage:start_span(example, [{tracer, example_tracer}]).
%%
%% passage:finish_span(Span). % The span will send to the jaeger agent on the localhost
%% '''
%%
%% === Refereces ===
%%
%% <ul>
%% <li><a href="http://jaeger.readthedocs.io/en/latest/architecture/#agent">Jaeger - Architecture - Agent</a></li>
%% <li><a href="http://jaeger.readthedocs.io/en/latest/deployment/#agent">Jaeger - Deployment - Agent</a></li>
%% </ul>
-module(jaeger_passage_reporter).

-behaviour(passage_reporter).

-include("constants.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start/1, start/2]).
-export([stop/1]).
-export([which_reporters/0]).
-export([get_id/1]).

-export_type([reporter_id/0]).
-export_type([start_option/0, start_options/0]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type reporter_id() :: atom().
%% Reporter identifier.

-type start_options() :: [start_option()].
%% Options for {@link start/2}.

-type start_option() :: jaeger_passage_reporter_udp:start_option()
                      | jaeger_passage_reporter_http:start_option()
                      | {protocol, udp | http}
                      | {default_service_name, atom()}
                      | {process_tags, passage:tags()}.

%% Common reporter options
%% <ul>
%%   <li><b>protocol</b>: Communication protocol used to connect to jaeger. The value is used to select reporter module. Possible values are: `udp' | `http'. The default value is `udp'.</li>
%%   <li><b>default_service_name</b>: The default service name. If a reporting span has `location.application' tag, the value is used as the service name instead of this. The default value is `ReporterId'.</li>
%%   <li><b>process_tags</b>: The tags of the reporting process. The default value is `#{}'.</li>
%% </ul>
%% UDP reporter specific options
%% <ul>
%%   <li><b>thrift_format</b>: The format for encoding thrift messages. The default value is `compact'.</li>
%%   <li><b>agent_host</b>: The hostname of the jaeger agent. The default value is `"127.0.0.1"'.</li>
%%   <li><b>agent_port</b>: The port of the jaeger agent. The default values for the thrift format `compact' and `binary' are `6831' and `6832' respectively.</li>
%% </ul>
%% HTTP reporter specific options
%% <ul>
%%   <li><b>endpoint</b>: The jaeger endpoint URL for sending thrift messages. The default value is `http://127.0.0.1:14268'.</li>
%% </ul>

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start(ReporterId, [])
-spec start(reporter_id()) -> {ok, passage_reporter:reporter()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start(ReporterId) ->
    start(ReporterId, []).

%% @doc Starts a reporter process.
-spec start(reporter_id(), start_options()) -> {ok, Reporter} | {error, Reason} when
      Reporter :: passage_reporter:reporter(),
      Reason :: {already_started, pid()} | term().
start(ReporterId, Options) ->
    Args = [ReporterId, Options],
    is_atom(ReporterId) orelse error(badarg, Args),
    is_list(Options) orelse error(badarg, Args),
    ReporterModule = case proplists:get_value(protocol, Options, udp) of
        udp -> jaeger_passage_reporter_udp;
        http -> jaeger_passage_reporter_http;
        _ -> error(badarg, Args)
    end,
    ReporterOptions = proplists:delete(protocol, Options),
    case jaeger_passage_reporter_sup:start_child(ReporterId, ReporterModule, ReporterOptions) of
        {error, Reason} -> {error, Reason};
        {ok, _Pid}      -> {ok, passage_reporter:new(?MODULE, ReporterId)}
    end.

%% @doc Stops the reporter process.
%%
%% If the reporter which has the identifier `ReporterId' has not been started,
%% it will be simply ignored.
-spec stop(reporter_id()) -> ok.
stop(ReporterId) ->
    jaeger_passage_reporter_sup:stop_child(ReporterId).

%% @doc Returns the list of the running reporters.
-spec which_reporters() -> [reporter_id()].
which_reporters() ->
    jaeger_passage_reporter_sup:which_children().

%% @doc Returns the identifier of `Reporter'.
%%
%% Note that if `Reporter' is one which has not been created by {@link start/2},
%% this function will crash.
-spec get_id(passage_reporter:reporter()) -> reporter_id().
get_id(Reporter) ->
    ?MODULE = passage_reporter:get_module(Reporter),
    passage_reporter:get_state(Reporter).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
%% @private
report(ReporterId, Span) ->
    Server = jaeger_passage_local_ns:reporter_name(ReporterId),
    gen_server:cast(Server, {report, Span}).
