%% @doc A reporter that sends the spans to an jaeger agent
%%
%% === Examples ===
%%
%% ```
%% %% Starts `example_reporter'
%% {ok, Reporter} = jaeger_passage_reporter:start(example_reporter, [{protocol, http}]).
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
-module(jaeger_passage_reporter_http).

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
          endpoint             :: string(),
          options              :: start_options(),
          http_client          :: http_client(),
          default_service_name :: atom(),
          process_tags         :: passage:tags()
        }).

-define(CONTENT_TYPE, {"Content-Type", "application/x-thrift"}).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------

-type start_options() :: [start_option()].
%% Options for {@link start/2}.

-type start_option() :: {endpoint, string()}
                      | {http_client, http_client()}
                      | {default_service_name, atom()}
                      | {process_tags, passage:tags()}.
%% <ul>
%%   <li><b>endpoint</b>: The jaeger endpoint URL for sending thrift messages. The default value is `http://127.0.0.1:14268'.</li>
%%   <li><b>http_client</b>: The callback to call to send span to jaeger. The httpc client is used by default.</li>
%%   <li><b>default_service_name</b>: The default service name. If a reporting span has `location.application' tag, the value is used as the service name instead of this. The default value is `ReporterId'.</li>
%%   <li><b>process_tags</b>: The tags of the reporting process. The default value is `#{}'.</li>
%% </ul>
%% Example of a http_client calback
%% Client = fun(Url, Method, Headers, Body, ReporterOptions) ->
%%    User = proplists:get_value(user, ReporterOptions),
%%    Password = proplists:get_value(password, ReporterOptions),
%%    ibrowse:send_req(Url, Headers, Method, Body, [{basic_auth, {User,  Password}}])
%% end.


-type http_client() :: fun((
        Url    :: string(),
        Method :: post,
        Headers :: [{string(), string()}],
        Body :: string() | binary(),
        ReporterOptions :: start_options()) ->
    ok).

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
    Endpoint = proplists:get_value(endpoint, Options, "http://127.0.0.1:14268"),
    EndpointURL = Endpoint ++ "/api/traces",

    HttpClient = proplists:get_value(http_client, Options, fun httpc_client/5),
    is_function(HttpClient, 5) orelse error(badarg, [ReporterId, Options]),

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
    State =
        #?STATE{
            endpoint              = EndpointURL,
            http_client           = HttpClient,
            options               = Options,
            default_service_name  = DefaultServiceName,
            process_tags          = Tags1
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
handle_report(Span, State = #?STATE{default_service_name = DefaultName, process_tags = Tags, endpoint = URI, http_client = HttpClient, options = Options}) ->
    Name = maps:get('location.application', passage_span:get_tags(Span), DefaultName),
    Message = jaeger_passage_thrift:make_batch(Name, Tags, [Span]),
    Encoded = thrift_protocol:encode_struct(Message, binary),
    Headers = [?CONTENT_TYPE],
    HttpClient(URI, post, Headers, Encoded, Options),
    {noreply, State}.

-spec httpc_client(
        Url    :: string(),
        Method :: post,
        Headers :: [{string(), string()}],
        Body :: string() | binary(),
        ReporterOptions :: start_options()) ->
    ok.

httpc_client(Url, Method, _Headers, Body, _ReporterOptions) ->
    httpc:request(Method, {Url, [], "application/x-thrift", Body}, [], []),
    ok.
