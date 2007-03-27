#CC = cl /nologo /MD 
#O = obj
#CHAIN = msvc

#CC = gcc -D_CYGWIN_
#O = o
#CHAIN=cygwin

#CC = gcc -D_MINGW_ -mno-cygwin
#O = o
#CHAIN = mingw

FLEXLINK = ../flexlink.exe -chain $(CHAIN)

.PHONY: demo
demo: dump.exe plug1.dll plug2.dll
	./dump.exe plug1.dll plug2.dll

dump.exe: dump.$(O)
	$(FLEXLINK) -exe -o dump.exe ../flexdll_$(CHAIN).$(O) dump.$(O)

dump.$(O): dump.c
	$(CC) -I.. -c dump.c

plug1.$(O): plug1.c
	$(CC) -c plug1.c

plug2.$(O): plug2.c
	$(CC) -c plug2.c

plug1.dll: plug1.$(O)
	$(FLEXLINK) -o plug1.dll plug1.$(O)

plug2.dll: plug2.$(O)
	$(FLEXLINK) -o plug2.dll plug2.$(O)

plug12.dll: plug1.$(O) plug2.$(O)
	$(FLEXLINK) -o plug12.dll plug1.$(O) plug2.$(O)


clean:
	rm -f *.o *.obj *.dll *.exe *~