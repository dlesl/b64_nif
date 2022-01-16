b64_nif
=====

Erlang's `base64` module is implemented in Erlang and rather slow. This library
provides a very fast alternative using [aklomp's
base64](https://github.com/aklomp/base64).

There is another library, [b64fast](https://github.com/zuckschwerdt/b64fast),
which takes a similar approach. This library behaves slightly differently in that it:

* Validates input while decoding (raises `bagarg`)
* Uses dirty schedulers for large inputs rather than a streaming approach
* Only supports Base64
* Is even faster thanks to [base64](https://github.com/aklomp/base64) (on my machine at least)

Use
---

Add it to your `rebar.config`, for example `{deps, [{b64_nif, "0.1.0"}]}.`

Use `b64_nif:encode/1` and `b64_nif:decode/1` as drop-in replacements for
`base64:encode/1` and `base64:decode/1`.

Benchmarks
----------

Run `./bench.sh`

```
== Testing with 100 B ==
fun base64:encode/1: 1000000 iterations in 3047 ms: 328191 it/sec
fun b64fast:encode64/1: 1000000 iterations in 935 ms: 1069518 it/sec
fun b64_nif:encode/1: 1000000 iterations in 298 ms: 3355704 it/sec
fun base64:decode/1: 1000000 iterations in 2951 ms: 338868 it/sec
fun b64fast:decode64/1: 1000000 iterations in 908 ms: 1101321 it/sec
fun b64_nif:decode/1: 1000000 iterations in 378 ms: 2645502 it/sec

== Testing with 1 MB ==
fun base64:encode/1: 1000 iterations in 30087 ms: 33 it/sec
fun b64fast:encode64/1: 1000 iterations in 1096 ms: 912 it/sec
fun b64_nif:encode/1: 1000 iterations in 120 ms: 8333 it/sec
fun base64:decode/1: 1000 iterations in 28293 ms: 35 it/sec
fun b64fast:decode64/1: 1000 iterations in 969 ms: 1031 it/sec
fun b64_nif:decode/1: 1000 iterations in 133 ms: 7518 it/sec
```
