JFLAGS = -g -d
SRCPATH = ./src
BINPATH = $(SRCPATH)/bin
CLASSPATH = ".:$(BINPATH)"
MAIN = Test

all: compile run clean

compile:
	@-mkdir -p $(BINPATH)
	@javac $(JFLAGS) $(BINPATH) $(SRCPATH)/*.java

run: compile $(BINPATH)
	@java -cp $(CLASSPATH) $(MAIN)

clean:
	@-rm $(BINPATH)/*
	@-rmdir $(BINPATH)

.PHONY: all compile run clean
