labrea
======

A rather buggy optimising brainf--k compiler. A successor to
[tarpit](https://github.com/jdpage/tarpit).

Compiling
---------

The distribution includes a shell script which invokes the labrea.scm file under
the Chicken Scheme interpreter with the appropriate arguments.

Do "make" to compile the programs in the examples directory. Both of the example
programs were taken from the [Wikipedia
article](http://en.wikipedia.org/w/index.php?title=Brainfuck&oldid=470402810).

Extending the Compiler
----------------------

### Adding additional platforms

To add an additional platform, at the very least you'd need to add a code
generator. These are defined in stage ω, and take the IR and convert it to a
kind of list-structured assembly representation.

If none of the supported assemblers (at this time, YASM, NASM, and GAS are
supported) targets your desired platform, you may need to add a new assembly
formatter which produces files suitable for your assembler.

Currently supported platforms are:

* xnu-amd64 -- 64-bit Intel Macs; 10.7+; some 10.6
* sysv-amd64 -- AMD64 SysV-compatible operating systems, such as Linux and
  FreeBSD (needs testing).
* ir -- Intermediate representation.

Support is planned for:

* sysv-ia32 -- x86 SysV-compatible operating systems, such as Linux and
  FreeBSD.
* sysv-arm6 -- ARMv6 SysV-compatible operating systems, such as Linux on the
  Raspberry Pi.
* winnt-ia32 -- Windows Native API-compatible operating systems.
* winnt-amd64 -- Windows Native API-compatible operating systems.
* dos16-8086 -- DOS-compatible operating systems.
* libc -- ANSI C output.

License
-------

Copyright (c) 2014 Jonathan David Page

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
