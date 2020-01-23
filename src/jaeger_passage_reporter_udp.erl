%% @doc A reporter that sends the spans to an jaeger agent using UDP.
%%
%% To start a reporter process, please use {@link jaeger_passage_reporter:start/1} or {@link jaeger_passage_reporter:start/2}.
%%
%% === Examples ===
%%
%% ```
%% %% Starts `example_reporter'
%% {ok, Reporter} = jaeger_passage_reporter:start(example_reporter, [{protocol, udp}]).
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
-module(jaeger_passage_reporter_udp).

-behaviour(gen_server).

-include("constants.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export_type([start_option/0, start_options/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/2]).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          socket               :: gen_udp:socket(),
          thrift_format        :: thrift_protocol:format(),
          agent_host           :: inet:hostname(),
          agent_port           :: inet:port_number(),
          default_service_name :: atom(),
          process_tags         :: passage:tags()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------

-type start_options() :: [start_option()].
%% Options for {@link jaeger_passage_reporter:start/2}.

-type start_option() :: {default_service_name, atom()}
                      | {process_tags, passage:tags()}
                      | {thrift_format, thrift_protocol:format()}
                      | {agent_host, inet:hostname()}
                      | {agent_port, inet:port_number()}.
%% <ul>
%%   <li><b>default_service_name</b>: The default service name. If a reporting span has `location.application' tag, the value is used as the service name instead of this. The default value is `ReporterId'.</li>
%%   <li><b>process_tags</b>: The tags of the reporting process. The default value is `#{}'.</li>
%%   <li><b>thrift_format</b>: The format for encoding thrift messages. The default value is `compact'.</li>
%%   <li><b>agent_host</b>: The hostname of the jaeger agent. The default value is `"127.0.0.1"'.</li>
%%   <li><b>agent_port</b>: The port of the jaeger agent. The default values for the thrift format `compact' and `binary' are `6831' and `6832' respectively.</li>
%% </ul>

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec start_link(jaeger_passage_reporter:reporter_id(), start_options()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(ReporterId, Options) ->
    Name = jaeger_passage_local_ns:reporter_name(ReporterId),
    gen_server:start_link(Name, ?MODULE, {ReporterId, Options}, []).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------
%% @private
init({ReporterId, Options}) ->
    Format = proplists:get_value(thrift_format, Options, compact),
    DefaultPort =
        case Format of
            compact -> 6831;
            binary  -> 6832
        end,
    AgentHost = proplists:get_value(agent_host, Options, "127.0.0.1"),
    AgentPort = proplists:get_value(agent_port, Options, DefaultPort),
    DefaultServiceName = proplists:get_value(default_service_name, Options, ReporterId),
    Tags0 = proplists:get_value(process_tags, Options, #{}),

    {ok, Hostname} = inet:gethostname(),
    {ok, Version} = application:get_key(vsn),
    Tags1 =
        maps:merge(
          Tags0,
          #{
            ?JAEGER_CLIENT_VERSION_TAG_KEY => list_to_binary(["jaeger_passage-", Version]),
            ?TRACER_HOSTNAME_TAG_KEY => list_to_binary(Hostname),
            'erlang.node' => node()
           }),
    {ok, Socket} = gen_udp:open(0),
    State =
        #?STATE{
            socket        = Socket,
            thrift_format = Format,
            agent_host    = AgentHost,
            agent_port    = AgentPort,
            default_service_name  = DefaultServiceName,
            process_tags  = Tags1
           },
    {ok, State}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({report, Span}, State) ->
    handle_report(Span, State);
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec handle_report(passage_span:span(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_report(Span, State = #?STATE{default_service_name = DefaultName, process_tags = Tags}) ->
    Name = maps:get('location.application', passage_span:get_tags(Span), DefaultName),
    Message = jaeger_passage_thrift:make_emit_batch_message(Name, Tags, [Span]),
    Encoded = thrift_protocol:encode_message(Message, State#?STATE.thrift_format),
    ok = gen_udp:send(State#?STATE.socket, State#?STATE.agent_host, State#?STATE.agent_port, Encoded),
    {noreply, State}.
