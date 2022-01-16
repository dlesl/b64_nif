# ERLANG_ROOT_DIR and ERLANG_ERTS_VER set by rebar3
ERTS_INCLUDE := $(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include

all: ../priv/b64_nif.so

clean:
	rm -f ../priv/b64_nif.so
	$(MAKE) -C base64 clean

base64/lib/libbase64.o:
	AVX2_CFLAGS=-mavx2 SSSE3_CFLAGS=-mssse3 SSE41_CFLAGS=-msse4.1 SSE42_CFLAGS=-msse4.2 AVX_CFLAGS=-mavx $(MAKE) -C base64

../priv/b64_nif.so: b64_nif.c base64/lib/libbase64.o
	$(CC) -O3 -o $@ -fpic -shared -I $(ERTS_INCLUDE) -I base64/include $^