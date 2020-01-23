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

% Starts `example_tracer`
> Sampler = passage_sampler_all:new().
> ok = jaeger_passage:start_tracer(example_tracer, Sampler).

% Starts a root span.
> RootSpan = passage:start_span(example_root, [{tracer, example_tracer}]).

% Starts a child span.
> ChildSpan = passage:start_span(example_child, [{child_of, RootSpan}]).

% Finishes the spans
> passage:finish_span(ChildSpan).
> passage:finish_span(RootSpan).
```

Browses the tracing result:
```console
$ firefox http://localhost:16686/
```

Selecting reporter
------------------

By default 'compact' jaeger.thrift over UDP reporter is used. However it is
possible to select different reporter. Bellow is a configuration matrics for
available options:

| protocol | thrift_protocol | jaeger port | description                      |
|----------|-----------------|-------------|----------------------------------|
| udp      | compact         | 6831        | accept jaeger.thrift over compact thrift protocol (default) |
| udp      | binary          | 6832        | accept jaeger.thrift over binary thrift protocol |
| http     | N/A             | 14268       | accept jaeger.thrift directly from clients |

The HTTP version is beneficial if you don't have jaeger agents deployed or your
spans are greater than max udp packet size (65Kb).
Otherwise it is better to use default.

References
-----------

- [OpenTracing](http://opentracing.io/)
- [Jaeger](https://uber.github.io/jaeger/)
- [jaeger-client-go/README.md](https://github.com/jaegertracing/jaeger-client-go/blob/v2.9.0/README.md)
- [Jaeger Client Library](https://github.com/jaegertracing/jaeger/blob/master/docs/client_libraries.md)
- [Jaeger default ports](https://www.jaegertracing.io/docs/1.8/getting-started/)