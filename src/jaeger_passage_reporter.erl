-module(jaeger_passage_reporter).

-behaviour(passage_reporter).
-behaviour(gen_server).

-include("constants.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start/1, start/2]).
-export([stop/1]).
-export([which_reporters/0]).

-export_type([reporter_id/0]).
-export_type([start_option/0, start_options/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([start_link/2]).

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback API
%%------------------------------------------------------------------------------
-export([report/2]).

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
          socket        :: gen_udp:socket(),
          thrift_format :: thrift_protocol:format(),
          agent_host    :: inet:hostname(),
          agent_port    :: inet:port_number(),
          service_name  :: atom(),
          service_tags  :: passage:tags()
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-type reporter_id() :: atom().

-type start_options() :: [start_option()].

-type start_option() :: {thrift_format, thrift_protocol:format()}
                      | {agent_host, inet:hostname()}
                      | {agent_port, inet:port_number()}
                      | {service_name, atom()}
                      | {service_tags, passage:tags()}.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
-spec start_link(reporter_id(), start_options()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(ReporterId, Options) ->
    Name = jaeger_passage_local_ns:reporter_name(ReporterId),
    gen_server:start_link(Name, ?MODULE, {ReporterId, Options}, []).

%%------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------
%% @equiv start(ReporterId, [])
-spec start(reporter_id()) -> {ok, passage_reporter:reporter()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start(ReporterId) ->
    start(ReporterId, []).

-spec start(reporter_id(), start_options()) -> {ok, passage_reporter:reporter()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start(ReporterId, Options) ->
    case jaeger_passage_reporter_sup:start_child(ReporterId, Options) of
        {error, Reason} -> {error, Reason};
        {ok, _Pid}      -> {ok, passage_reporter:new(?MODULE, ReporterId)}
    end.

-spec stop(reporter_id()) -> ok.
stop(ReporterId) ->
    jaeger_passage_reporter_sup:stop_child(ReporterId).

-spec which_reporters() -> [reporter_id()].
which_reporters() ->
    jaeger_passage_reporter_sup:which_children().

%%------------------------------------------------------------------------------
%% 'passage_reporter' Callback Functions
%%------------------------------------------------------------------------------
%% @private
report(ReporterId, Span) ->
    Server = jaeger_passage_local_ns:reporter_name(ReporterId),
    gen_server:cast(Server, {report, Span}).

%%------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------
%% @private
init({ReporterId, Options}) ->
    Format = proplists:get_value(thrift_protocol, Options, compact),
    DefaultPort =
        case Format of
            compact -> 6831;
            binary  -> 6832
        end,
    AgentHost = proplists:get_value(agent_host, Options, "127.0.0.1"),
    AgentPort = proplists:get_value(agent_port, Options, DefaultPort),
    ServiceName = proplists:get_value(service_name, Options, ReporterId),
    Tags0 = proplists:get_value(service_tags, Options, #{}),

    {ok, Hostname} = inet:gethostname(),
    {ok, Version} = application:get_key(vsn),
    Tags1 =
        maps:merge(
          Tags0,
          #{
            ?JAEGER_CLIENT_VERSION_TAG_KEY => list_to_binary(["jaeger_passage-", Version]),
            ?TRACER_HOSTNAME_TAG_KEY => list_to_binary(Hostname)
           }),
    {ok, Socket} = gen_udp:open(0),
    State =
        #?STATE{
            socket        = Socket,
            thrift_format = Format,
            agent_host    = AgentHost,
            agent_port    = AgentPort,
            service_name  = ServiceName,
            service_tags  = Tags1
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
handle_report(Span, State = #?STATE{service_name = Name, service_tags = Tags}) ->
    Message = jaeger_passage_thrift:make_emit_batch_message(Name, Tags, [Span]),
    Encoded = thrift_protocol:encode_message(Message, State#?STATE.thrift_format),
    ok = gen_udp:send(State#?STATE.socket, State#?STATE.agent_host, State#?STATE.agent_port, Encoded),
    {noreply, State}.
