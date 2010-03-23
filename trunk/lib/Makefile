
ACKBASE=$(CURDIR)/obj-ack
GNUBASE=$(CURDIR)/obj-gnu

OBJDIR=.

SUBDIRS = ansi \
	curses \
	dummy \
	editline \
	end \
	ip \
	math \
	other \
	posix \
	regex \
	stdio \
	syscall \
	syslib \
	util \
	sysutil \
	timers \
	i386 \
	zlib-1.2.3 \
	ack
	

include Makefile.ack.inc

install:: 
	install -o bin $(ACKBASE)/*.[ao] /usr/lib/i386
