# box80
Z80 Virtual Machine - this is very much a work in progress and is purely a learning exercise for the author.

### Synopsis

Box80 is a multi-platform virtual machine designed to execute Z80 code in a 64K code space. The intended usage is for running CP/M 2.2.

### Development status

What's working:
* Some of the Z80 instructions
* The terminal can output characters, clear the screen, etc.
* The terminal can take character input
* The SIO can trigger interrupts

What's not working
* About 50% of the basic Z80 instructions are missing
* Almost all of the extended (CB FD etc.) instructions are missing
* The SIO is a very basic implementation, it will send and receive and let you know if a character is waiting; that's about it..

### Development environment

The development environment is Lazarus/FPC version 2.x onwards. This is a multi-platform PASCAL IDE which operates under Windows, Linux, MacOS, etc.
