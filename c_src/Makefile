# ERLANG_ROOT_DIR and ERLANG_ERTS_VER set by rebar3
ERTS_INCLUDE := $(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include

BASE64 := base64_vendored

OBJCOPY ?= true # not available by default on mac, but not needed

CFLAGS += -fPIC

ifeq ($(shell uname -s), Darwin)
	SO_LD_FLAGS := -undefined dynamic_lookup -dynamiclib
else
	SO_LD_FLAGS := -fpic -shared
endif

all: ../priv/b64_nif.so

clean:
	rm -f ../priv/b64_nif.so
	$(MAKE) -C $(BASE64) clean

$(BASE64)/lib/libbase64.o:
	AVX2_CFLAGS=-mavx2 SSSE3_CFLAGS=-mssse3 SSE41_CFLAGS=-msse4.1 SSE42_CFLAGS=-msse4.2 AVX_CFLAGS=-mavx \
		$(MAKE) -C $(BASE64) OBJCOPY=$(OBJCOPY)

../priv/b64_nif.so: b64_nif.c $(BASE64)/lib/libbase64.o
	$(CC) -O3 -o $@ $(SO_LD_FLAGS) -I $(ERTS_INCLUDE) -I $(BASE64)/include $^
