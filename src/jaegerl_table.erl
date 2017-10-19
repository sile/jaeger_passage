-module(jaegerl_table).

-export([get_tracer_info/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type tracer_info() ::
        #{
           pid => pid(),
           sampler => module()
         }.

-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          table :: ets:tab()
        }).

-spec get_tracer_info(jaegerl:tracer_id()) -> {ok, tracer_info()} | error.
get_tracer_info(TracerId) ->
    try ets:lookup(?MODULE, {tracer_info, TracerId}) of
        [{_, Info}] -> {ok, Info};
        _           -> error
    catch
        error:badarg -> error
    end.

%% @private
init([]) ->
    Table = ets:new(?MODULE, [named_table, protected, {read_concurrency, true}]),
    {ok, #?STATE{table = Table}}.

%% @private
handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast(Request, State) ->
    {stop, {unknown_cast, Request}, State}.

%% @private
handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
