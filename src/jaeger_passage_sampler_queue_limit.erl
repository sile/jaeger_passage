%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @private
-module(jaeger_passage_sampler_queue_limit).

-behaviour(passage_sampler).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([new/3]).

%%------------------------------------------------------------------------------
%% 'passage_sampler' Callback API
%%------------------------------------------------------------------------------
-export([is_sampled/3]).

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
-spec new(BaseSampler, ReporterId, non_neg_integer()) -> passage_sampler:sampler() when
      BaseSampler :: passage_sampler:sampler(),
      ReporterId :: jaeger_passage_reporter:reporter_id().
new(BaseSampler, Reporter, MaxReporterQueueLen) ->
    passage_sampler:new(?MODULE, {BaseSampler, Reporter, MaxReporterQueueLen}).

%%------------------------------------------------------------------------------
%% 'passage_sampler' Callback Functions
%%------------------------------------------------------------------------------
%% @private
is_sampled({BaseSampler, ReporterId, MaxQueueLen}, OperationName, Tags) ->
    case jaeger_passage_local_ns:whereis_reporter(ReporterId) of
        undefined -> false;
        Pid       ->
            case erlang:process_info(Pid, message_queue_len) of
                {_, N} ->
                    (N =< MaxQueueLen) andalso
                        passage_sampler:is_sampled(BaseSampler, OperationName, Tags);
                _ ->
                    false
            end
    end.
