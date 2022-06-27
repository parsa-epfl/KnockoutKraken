IDIR =.
CC=gcc
CFLAGS=-I$(IDIR)

ODIR=obj

objects=test_cache_page_evict test_fclose test_fopen test_fork test_fread test_getpid test_memsvc_segfault_full test_mmap test_mprotect_segfault test_mprotect_success test_mremap test_mumap test_puts test_sleep test_read_inst_page_buffer test_read_inst_page_func loader

_DEPS = devteroflex.h
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

all: dir $(objects) close test_multithreading

close:
	echo "chmod +x no_loader/*"
	echo "chmod +x with_loader/*"

dir:
	if [ ! -d "no_loader" ]; then mkdir no_loader; fi
	if [ ! -d "with_loader" ]; then mkdir with_loader; fi
	$(CC) -o loader -Wall loader.c $< $(CFLAGS)

$(objects): %: %.c $(DEPS) 
	$(CC) $(CFLAGS) -Wall -g -O0 -o no_loader/$@ $< 
	$(CC) $(CFLAGS) -Wall -g -O0 -DCONFIG_FOR_LOADER -o with_loader/$@ $< 

test_multithreading: test_multithreading.c $(DEPS)
	$(CC) $(CFLAGS) -Wall -lpthread -g -O0 -o no_loader/$@ $<
	$(CC) $(CFLAGS) -Wall -lpthread -g -O0 -DCONFIG_FOR_LOADER -o with_loader/$@ $< 

.PHONY: clean

clean:
	rm -f no_loader/* with_loader/* *~ core $(INCDIR)/*~ loader
	rmdir no_loader with_loader

