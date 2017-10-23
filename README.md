jaeger_passage
==============

[![hex.pm version](https://img.shields.io/hexpm/v/jaeger_passage.svg)](https://hex.pm/packages/jaeger_passage)
[![Build Status](https://travis-ci.org/sile/jaeger_passage.svg?branch=master)](https://travis-ci.org/sile/jaeger_passage)
[![Code Coverage](https://codecov.io/gh/sile/jaeger_passage/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/jaeger_passage/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

[Jaeger](https://uber.github.io/jaeger/) client library for Erlang.

This is an extension library of [passage](https://github.com/sile/passage).

[Documentation](https://hexdocs.pm/jaeger_passage/)

A Running Example
-----------------

Starts Jaeger in the background:

```console
$ docker run -d -p6831:6831/udp -p6832:6832/udp -p16686:16686 jaegertracing/all-in-one:latest
```

Starts Erlang Shell:
```erlang
$ rebar3 shell

% Starts `example_repoter` and registers `example_tracer`
> Context = jaeger_passage_span_context.
> Sampler = passage_sampler_all:new().
> {ok, Reporter} = jaeger_passage_reporter:start(example_repoter).
> ok = passage_tracer_registry:register(example_tracer, Context, Sampler, Reporter).

% Starts a root span.
> RootSpan = passage:start_root_span(example_root, example_tracer).

% Starts a child span.
> ChildSpan = passage:start_span(example_child, {child_of, RootSpan}).

% Finishes the spans
> passage:finish_span(ChildSpan).
> passage:finish_span(RootSpan).
```

Browses the tracing result:
```console
$ firefox http://localhost:16686/
```

References
-----------

- [OpenTracing](http://opentracing.io/)
- [Jaeger](https://uber.github.io/jaeger/)
- [jaeger-client-go/README.md](https://github.com/jaegertracing/jaeger-client-go/blob/v2.9.0/README.md)
