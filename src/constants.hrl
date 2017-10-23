%% https://github.com/uber/jaeger-client-go/tree/v2.9.0/constants.go

-define(JAEGER_CLIENT_VERSION_TAG_KEY, 'jaeger.version').
-define(JAEGER_DEBUG_HEADER, <<"jaeger-debug-id">>).
-define(JAEGER_BAGGAGE_HEADER, <<"jaeger-baggage">>).
-define(TRACER_HOSTNAME_TAG_KEY, 'hostname').
-define(TRACER_IP_TAG_KEY, 'ip').
-define(TRACER_STATE_HEADER_NAME, <<"uber-trace-id">>).
-define(TRACE_BAGGAGE_HEADER(Suffix), <<"uberctx-", Suffix/binary>>).
