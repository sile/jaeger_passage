%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Span context for Jaeger
%%
%% === References ===
%%
%% <ul>
%% <li><a href="https://github.com/jaegertracing/jaeger-client-go/blob/v2.9.0/README.md">jaeger-client-go/README.md</a></li>
%% <li><a href="https://github.com/uber/jaeger-client-go/tree/v2.9.0/context.go">context.go</a></li>
%% <li><a href="https://github.com/uber/jaeger-client-go/tree/v2.9.0/propagation.go">propagation.go</a></li>
%% </ul>

-module(jaeger_passage_span_context).

-behaviour(passage_span_context).

-include("constants.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export_type([state/0]).

%%------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------
-export([get_trace_id/1]).
-export([get_span_id/1]).
-export([get_parent_span_id/1]).
-export([get_debug_id/1]).
-export([get_flags/1]).

%%------------------------------------------------------------------------------
%% 'passage_span_context' Callback API
%%------------------------------------------------------------------------------
-export([make_span_context_state/1, inject_span_context/4, extract_span_context/3]).

%%------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------
-define(FLAG_SAMPLED, 2#01).
-define(FLAG_DEBUG, 2#10).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          trace_id = 0       :: trace_id(),
          span_id = 0        :: span_id(),
          parent_span_id = 0 :: span_id(),
          is_sampled = false :: boolean(),
          debug_id           :: binary() | undefined
        }).

%%------------------------------------------------------------------------------
%% Exported Types
%%------------------------------------------------------------------------------
-opaque state() :: #?STATE{}.
%% The state of a jaeger span context.

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type trace_id() :: 0..16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF.
%% Tracer identifier (128-bit unsigned integer).
%%
%% This represents globally unique ID of the trace.
%% Usually generated as a random number.

-type span_id() :: 0..16#FFFFFFFFFFFFFFFF.
%% Span identifier (64-bit unsigned integer).
%%
%% This represents span ID that must be unique within its trace,
%% but does not have to be globally unique.
%%
%% Note that the ID `0' is used for representing invalid spans.

%%------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------
%% @private
-spec get_trace_id(passage_span_context:context()) -> trace_id().
get_trace_id(Context) ->
    (passage_span_context:get_state(Context))#?STATE.trace_id.

%% @private
-spec get_span_id(passage_span_context:context()) -> span_id().
get_span_id(Context) ->
    (passage_span_context:get_state(Context))#?STATE.span_id.

-spec get_parent_span_id(passage_span_context:context()) -> span_id().
get_parent_span_id(Context) ->
    (passage_span_context:get_state(Context))#?STATE.parent_span_id.

%% @private
-spec get_debug_id(passage_span_context:context()) -> {ok, binary()} | eror.
get_debug_id(Context) ->
    State = passage_span_context:get_state(Context),
    case State#?STATE.debug_id of
        undefined -> error;
        DebugId   -> {ok, DebugId}
    end.

%% @private
-spec get_flags(passage_span_context:context()) -> 0..16#FFFFFFFF.
get_flags(Context) ->
    State = passage_span_context:get_state(Context),
    get_flags_from_state(State).

%%------------------------------------------------------------------------------
%% 'passage_span_context' Callback Functions
%%------------------------------------------------------------------------------
%% @private
make_span_context_state([]) ->
    #?STATE{
        trace_id   = rand:uniform(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF),
        span_id    = rand:uniform(16#FFFFFFFFFFFFFFFF),
        is_sampled = true
       };
make_span_context_state([{_, Ref} | _]) ->
    #?STATE{trace_id = TraceId, span_id = ParentId} =
        passage_span_context:get_state(passage_span:get_context(Ref)),
    #?STATE{
        trace_id       = TraceId,
        span_id        = rand:uniform(16#FFFFFFFFFFFFFFFF),
        parent_span_id = ParentId,
        is_sampled     = true
       }.

%% @private
inject_span_context(Context, binary, InjectFun, Carrier) ->
    Bin0 = encode_state(passage_span_context:get_state(Context)),

    Items = passage_span_context:get_baggage_items(Context),
    Bin1 = <<Bin0/binary, (maps:size(Items)):32>>,
    Bin2 = encode_baggage_items(Bin1, maps:to_list(Items)),
    InjectFun(<<"binary">>, Bin2, Carrier);
inject_span_context(Context, Format, InjectFun, Carrier) ->
    State = passage_span_context:get_state(Context),
    MapFun =
        case Format of
            text_map ->
                fun({K, V}) -> {?TRACE_BAGGAGE_HEADER(K), V} end;
            http_header ->
                fun({K, V}) -> {?TRACE_BAGGAGE_HEADER(K), escape(V)} end
        end,
    Items0 = lists:map(MapFun, maps:to_list(passage_span_context:get_baggage_items(Context))),
    Items1 = [{?TRACER_STATE_HEADER_NAME, state_to_string(State)} | Items0],
    lists:foldl(fun ({K, V}, Acc) -> InjectFun(K, V, Acc) end, Carrier, Items1).

%% @private
extract_span_context(binary, IterateFun, Carrier) ->
    GetBinary =
        fun GetBinary (Curr) ->
                case IterateFun(Curr) of
                    error                    -> error;
                    {ok, <<"binary">>, V, _} -> {ok, V};
                    {ok, _, _, Next}         -> GetBinary(Next)
                end
        end,
    case GetBinary(Carrier) of
        error      -> error;
        {ok, Bin0} ->
            {State, Bin1} = decode_state(Bin0),
            BaggageItems = decode_baggage_items(Bin1),
            {ok, passage_span_context:make(State, BaggageItems)}
    end;
extract_span_context(Format, IterateFun, Carrier) ->
    DecodeValue =
        case Format of
            text_map -> fun (V) -> V end;
            http_header -> fun unescape/1
        end,
    Map = extract_to_map(IterateFun, Carrier, DecodeValue, #{}),
    case maps:is_key(state, Map) orelse maps:is_key(debug_id, Map) of
        false -> error;
        true  ->
            State0 = maps:get(state, Map, #?STATE{}),
            State1 = State0#?STATE{debug_id = maps:get(debug_id, Map, undefined)},
            BaggageItems = maps:filter(fun(K, _) -> is_binary(K) end, Map),
            {ok, passage_span_context:make(State1, BaggageItems)}
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------
-spec extract_to_map(passage_span_context:iterate_fun(),
                     passage_span_context:carrier(),
                     fun ((binary()) -> binary()),
                     Map) -> Map when
      Map :: #{state => #?STATE{},
               debug_id => binary(),
               binary() => binary()}.
extract_to_map(IterateFun, Carrier0, DecodeValue, Map0) ->
    case IterateFun(Carrier0) of
        error                 -> Map0;
        {ok, K, V, Carrier1} ->
            Map1 =
                case K of
                    ?TRACER_STATE_HEADER_NAME ->
                        maps:put(state, state_from_string(DecodeValue(V)), Map0);
                    ?JAEGER_DEBUG_HEADER ->
                        maps:put(debug_id, DecodeValue(V), Map0);
                    ?JAEGER_BAGGAGE_HEADER ->
                        Pairs0 =
                            binary:split(V, [<<",">>, <<" ">>], [global, trim, trim_all]),
                        Pairs1 =
                            [{Key, Value} ||
                                [Key, Value] <-
                                    lists:map(
                                      fun (P) -> binary:split(P, <<"=">>) end,
                                      Pairs0)],
                        maps:merge(Map0, maps:from_list(Pairs1));
                    ?TRACE_BAGGAGE_HEADER(Name) ->
                        maps:put(Name, DecodeValue(V), Map0);
                    _ -> Map0
                end,
            extract_to_map(IterateFun, Carrier1, DecodeValue, Map1)
    end.

-spec get_flags_from_state(#?STATE{}) -> 0..16#FFFFFFFF.
get_flags_from_state(State) ->
    FlagSampled =
        case State#?STATE.is_sampled of
            false -> 0;
            true  -> ?FLAG_SAMPLED
        end,
    FlagDebug =
        case State#?STATE.debug_id of
            undefined -> 0;
            _         -> ?FLAG_DEBUG
        end,
    FlagSampled + FlagDebug.

-spec state_to_string(#?STATE{}) -> binary().
state_to_string(State) ->
    #?STATE{trace_id = TraceId, span_id = SpanId, parent_span_id = ParentId} = State,
    Flags = get_flags_from_state(State),
    list_to_binary(io_lib:format("~.16b:~.16b:~.16b:~.16b",
                                 [TraceId, SpanId, ParentId, Flags])).

-spec state_from_string(binary()) -> #?STATE{}.
state_from_string(Str) ->
    {ok, [TraceId, SpanId, ParentSpanId, Flags], _} =
        io_lib:fread("~16u:~16u:~16u:~16u", binary_to_list(Str)),
    #?STATE{
        trace_id = TraceId,
        span_id = SpanId,
        parent_span_id = ParentSpanId,
        is_sampled = (Flags band ?FLAG_SAMPLED) =/= 0
       }.

-spec encode_state(#?STATE{}) -> binary().
encode_state(State) ->
    #?STATE{trace_id = TraceId, span_id = SpanId, parent_span_id = ParentId} = State,
    Flags = get_flags_from_state(State),
    <<TraceId:128, SpanId:64, ParentId:64, Flags:32>>.

-spec decode_state(binary()) -> {#?STATE{}, binary()}.
decode_state(<<TraceId:128, SpanId:64, ParentId:64, Flags:32, Bin/binary>>) ->
    State =
        #?STATE{
            trace_id = TraceId,
            span_id = SpanId,
            parent_span_id = ParentId,
            is_sampled = (Flags band ?FLAG_SAMPLED) =/= 0
           },
    {State, Bin}.

-spec encode_baggage_items(binary(), Items) -> binary() when
      Items :: [{passage:baggage_item_name(), passage:baggage_item_value()}].
encode_baggage_items(Bin, []) ->
    Bin;
encode_baggage_items(Bin, [{K, V} | Items]) ->
    encode_baggage_items(
      <<Bin/binary, (byte_size(K)):32, K/binary, (byte_size(V)):32, V/binary>>,
      Items).

-spec decode_baggage_items(binary()) -> passage:baggage_items().
decode_baggage_items(<<Count:32, Bin/binary>>) ->
    decode_baggage_items(Bin, Count, #{}).

-spec decode_baggage_items(binary(), non_neg_integer(), passage:baggage_items()) ->
                                  passage:baggage_items().
decode_baggage_items(_, 0, Items) ->
    Items;
decode_baggage_items(<<KeySize:32, Key:KeySize/binary,
                       ValueSize:32, Value:ValueSize/binary, Bin/binary>>, Count, Items) ->
    decode_baggage_items(Bin, Count - 1, maps:put(Key, Value, Items)).

-spec escape(binary()) -> binary().
escape(Bin) ->
    list_to_binary(http_uri:encode(binary_to_list(Bin))).

-spec unescape(binary()) -> binary().
unescape(Bin) ->
    list_to_binary(http_uri:decode(binary_to_list(Bin))).
