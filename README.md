# Box80 V0.0
Box80 - A cross-platform Virtual Machine to run Z80 code and CP/M.

### Description
Box80 is a multi-platform virtual machine designed to execute Z80 code in a 64K code space. The intended usage is for running CP/M 2.2 and debugging assembly language apps developed for that operating system.
The software emulates the hardware of [CP/M on breadboard by Grant Searle](http://searle.x10host.com/cpm/index.html) and caters for the following features:
* Z80-SIO used to control serial input and output. This is redirected to the Windows screen and keyboard, there is no real serial linl
* CF card interface to allow storage of the CP/M operating system, CP/M commands, and user installed software and data
* The ROM disable feature is not implemented as it is not actually used (the monitor is loaded into RAM automatically by the VM)

### Dependencies
To use a basic CP/M operating system, this software requires the following components:

1. Monitor image to allow the initial boot
2. BIOS image to support the following
	* Z80-ZIO using ports $00..$03
	* Compact Flash IDE interface using ports $10..$17
3. CP/M image containing:
    * BDOS (Basic Disk Operating System)
    * CP/M CCP (Console Command Processor)

They are not included with this distribution as they have their own licensing terms which can be found on the website and in the source code
for the different components. The documents section contains dependencies.pdf which explains how to download and assemble the four modules listed
above (not implemented).

### Development Environment
To modify and compile this software, you will need [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) 2.1.0 or later. It has been
tested on Windows. There are no "OS specific" twists, so it should be possible to recompile on other hosts
which are supported by the Lazarus ecosystem in 32 and 64 bit flavours, including:

* Android
* FreeBSD
* iOS
* Linux
* macOS
* Raspberry Pi
* WinCE
* Windows

It may be possible to emulate different hardware by altering the port addresses.

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

### Licence
Box80 is licensed under the GNU GPL Licence V3 or later. External components (monitor, BIOS, CP/M) are not included with this distribution
and have their own licensing and distribution terms. 

### Contact
The author is Duncan Munro  
Twitter: @duncanamps  
Email: duncan@duncanamps.com  
