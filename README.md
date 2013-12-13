FlexDLL: an implementation of a dlopen-like API for Windows

Homepage: http://alain.frisch.fr/flexdll.html

# Introduction


Under Windows, DLL (<a
href="http://en.wikipedia.org/wiki/Dynamic-link_library">Dynamically-Linked
Libraries</a>) are generally used to improve code
modularity and sharing. A DLL can be loaded automatically when the
program is loaded (if it requires the DLL). The program can also
explicitly request Windows to load a DLL at any moment during runtime,
using the <a
href="http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/loadlibrary.asp"><code>LoadLibrary</code></a>
function from the Win32 API.

This naturally suggests to use DLLs as a plugin mechanism. For
instance, a web server could load extensions modules stored in DLLs at
runtime. But Windows does not really make it easy to implement plugins
that way. The reason is that when you try to create a DLL from a set
of object files, the linker needs to resolve all the symbols, which
leads to the very problem solved by FlexDLL:

<b>
Windows DLL cannot refer to symbols defined in the main
application or in previously loaded DLLs.
</b>

Some usual solutions exist, but they are not very
flexible. A notable exception is the <a
href="http://edll.sourceforge.net/">edll</a> library (its homepage
also describes the usual solutions), which follows a rather
drastic approach; indeed, edll implements a new dynamic linker which
can directly load object files (without creating a Windows DLL).

FlexDLL is another solution to the same problem. Contrary
to edll, it relies on the native static and dynamic
linkers.  Also, it works both with the Microsoft environment (MS
linker, Visual Studio compilers) and with Cygwin (GNU linker and
compilers, in Cygwin or MinGW mode). Actually, FlexDLL implements
mostly the usual <a
href="http://www.opengroup.org/onlinepubs/009695399/functions/dlopen.html"><code>dlopen</code></a>
POSIX API, without trying to be fully conformant though (e.g. it does
not respect the official priority ordering for symbol
resolution). This should make it easy to port applications developped
for Unix.


# About

FlexDLL is distributed under the terms of a zlib/libpng open source
license. The copyright holder is the Institut National de Recherche en
Informatique et en Automatique (INRIA).  The project was started when
I (= Alain Frisch) was working at INRIA.  I'm now working
for <a href="http://www.lexifi.com">LexiFi</a>, which is kind enough
to let me continue my work on FlexDLL. My office mate at INRIA,
Jean-Baptiste Tristan, coined the name FlexDLL.
</p>

The runtime support library is written in C. The
<code>flexlink</code> wrapper is implemented in the wonderful
<a href="http://caml.inria.fr">Objective Caml</a> language.


# Supported toolchain

MSVC: the 32-bit C compiler from Microsoft.

MSVC64: the 64-bit C compiler from Microsoft.

CYGWIN: the 32-bit gcc compiler shipped with Cygwin.

MINGW: the 32-bit gcc compiler from the Mingw64 project, packaged in Cygwin (as i686-w64-mingw32-gcc).

MINGW64: the 64-bit gcc compiler from the Mingw64 project, packaged in Cygwin (as x86_64-w64-mingw32-gcc).

LD: an internal linker to produce .dll (only).
