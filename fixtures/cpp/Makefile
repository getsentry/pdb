# This is an NMAKE Makefile

all: clean foo.exe
	
clean:
	del *.obj *.pdb *.exe

# compiler:
#   /Od	Disables optimization
#   /GR	Enables run-time type information (RTTI)
#   /Zi Produces a program database (PDB) that contains type information and symbolic debugging information for use with the debugger. The symbolic debugging information includes the names and types of variables, as well as functions and line numbers.
# linker:
#   /DEBUG	Creates debugging information
foo.exe: foo.cpp
	cl /Od /GR /Zi foo.cpp /link /debug:full /out:foo.exe
	del *.obj *.ilk
