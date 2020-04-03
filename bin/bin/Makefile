progs := xrquery

CC := gcc
CFLAGS := -Wall -Wextra -Werror

ifdef DBG
	CFLAGS += -g
endif

.PHONY: all
all: $(progs)

xrquery: LDFLAGS += -lxcb -lxcb-xrm

%: %.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm $(progs)
