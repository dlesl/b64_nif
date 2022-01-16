-module(b64_nif_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
  [begin
     Data = crypto:strong_rand_bytes(rand:uniform(1000)),
     ?assertEqual(base64:encode(Data), b64_nif:encode(Data))
   end || _ <- [lists:seq(1, 1000)]].

decode_test() ->
  [begin
     Data = crypto:strong_rand_bytes(rand:uniform(1000)),
     ?assertEqual(b64_nif:decode(base64:encode(Data)), Data)
   end || _ <- [lists:seq(1, 1000)]].

empty_test() ->
  <<>> = b64_nif:encode(<<>>),
  <<>> = b64_nif:decode(<<>>).

bad_input_test() ->
  ?assertException(error, badarg, b64_nif:decode(<<"Base64">>)).

iolist_test() ->
  <<"I am an iolist">> = base64:decode(b64_nif:encode(["I ", <<"am">>, [[[" an iolist"]]]])),
  <<5,171,30,235>> = b64_nif:decode(["Base", [[<<"64==">>]]]).

bad_iolist_test() ->
  ?assertException(error, badarg, b64_nif:encode("😈")),
  ?assertException(error, badarg, b64_nif:decode("😈")).

bad_input_memory_leak_test() ->
  %% 100 MB of data
  Input = binary:copy(<<0>>, 100_000_000),
  %% Encode it 500 times. If we have a memory leak this would allocate 50 GB of memory.
  [begin
     ?assertException(error, badarg, b64_nif:decode(Input)),
     %% discard the return value (important!)
     ok
   end || _ <- lists:seq(1, 500)].

good_input_memory_leak_test_() ->
  {timeout, 120, fun test_good_input_memory_leak/0}.

test_good_input_memory_leak() ->
  %% 100 MB of data
  Input = binary:copy(<<0>>, 100_000_000),
  %% Roundtrip it 500 times. If we have a memory leak this would allocate 50 GB of memory.
  [begin
     Input = b64_nif:decode(b64_nif:encode(Input)),
     %% discard the return value (important!)
     ok
   end || _ <- lists:seq(1, 500)].

