SHELL=cmd.exe
EXEC=xadrez.rkt

all: execute

execute:
	@racket $(EXEC)
