#include <erl_nif.h>
#include <libbase64.h>

#define DECODE_MARGIN 64

#define MAX_NONDIRTY_SIZE 200000 // takes about 700 microsec to encode on my
                                 // machine, for both encode and decode

void consume_timeslice(ErlNifEnv* env, size_t size) {
  int percent = size * 100 / MAX_NONDIRTY_SIZE;
  if (percent > 100) {
    percent = 100;
  }
  if (percent > 0) {
    enif_consume_timeslice(env, percent);
  }
}

static ERL_NIF_TERM encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input, output;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      return enif_make_badarg(env);
    }
    if (input.size > MAX_NONDIRTY_SIZE && enif_thread_type() != ERL_NIF_THR_DIRTY_CPU_SCHEDULER) {
      return enif_schedule_nif(env, "encode_dirty", ERL_NIF_DIRTY_JOB_CPU_BOUND, &encode, argc, argv);
    }
    // margin taken from https://github.com/aklomp/base64/blob/master/bin/base64.c
    // but we could probably get away with less?
    if (!enif_alloc_binary(5 * input.size / 3, &output)) {
      // FIXME: badarg isn't really accurate (probably the input is too big for
      // the available memory, but that's not really invalid input)
      return enif_make_badarg(env);
    }
    size_t encoded;
    base64_encode((char*)input.data, input.size, (char*)output.data, &encoded, 0);
    if (!enif_realloc_binary(&output, encoded)) {
      enif_release_binary(&output);
      return enif_make_badarg(env);
    }
    consume_timeslice(env, input.size);
    return enif_make_binary(env, &output);
}

static ERL_NIF_TERM decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input, output;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
      return enif_make_badarg(env);
    }
    if (input.size > MAX_NONDIRTY_SIZE && enif_thread_type() != ERL_NIF_THR_DIRTY_CPU_SCHEDULER) {
      return enif_schedule_nif(env, "decode_dirty", ERL_NIF_DIRTY_JOB_CPU_BOUND, &decode, argc, argv);
    }
    if (!enif_alloc_binary(3 * input.size / 4 + DECODE_MARGIN, &output)) {
      return enif_make_badarg(env);
    }
    size_t decoded;
    if (!base64_decode((char*)input.data, input.size, (char*)output.data, &decoded, 0)) {
      return enif_make_badarg(env);
    }
    if (!enif_realloc_binary(&output, decoded)) {
      enif_release_binary(&output);
      return enif_make_badarg(env);
    }
    consume_timeslice(env, input.size);
    return enif_make_binary(env, &output);
}

static ErlNifFunc nif_funcs[] = {
    {"encode", 1, encode},
    {"decode", 1, decode}
};

ERL_NIF_INIT(b64_nif, nif_funcs, NULL, NULL, NULL, NULL)
