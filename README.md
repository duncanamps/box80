## Box80 V0.0
**One liner:** Box80 - A cross-platform open-source Virtual Machine to run and debug Z80 code (very much work in progress)

![screen_dump_windows_800](https://github.com/duncanamps/box80/assets/6016794/a90fa355-68c8-4fba-a334-490dccfcb2f5)

## Description
Box80 is a multi-platform virtual machine designed to execute Z80 code in a 64K code space. The main objective is to have a rich and capable cross-platform environment for debugging of CP/M assembly language programs, rather than to serve as Yet Another Emulator for CP/M.

## Features
**Note:** Many of these are not yet implemented! Please see the development status further down the page for an update on the status
* General
    * Open Source
    * Dual thread model; one for the user interface and one for the Z80 processor and SIO
    * Multi-platform design - can be used on many different target 32/64 bit machines
* Hardware emulation is based on the [CP/M on breadboard by Grant Searle](http://searle.x10host.com/cpm/index.html) and caters for the following features:
    * Z80-SIO used to control serial input and output. This is redirected to the Windows screen and keyboard, there is no real serial link
    * CF card interface to allow storage of the CP/M operating system, CP/M commands, and user installed software and data (not implemented)
    * The ROM disable feature is not implemented as it is not actually used (the monitor is loaded into RAM automatically by the VM)
    * Integrated terminal screen and keyboard emulation
    * Emulates a range of processor speeds from the original Z80 2.5MHz up to 100MHz (depending on host hardware capability). Also a bonus 32768Hz mode thrown in
* Debug features
    * Disassembler
    * Memory viewer
    * Watch window with different variable types
    * Conditional breakpoints
    * Traps for illegal instructions, unimplemented ports, etc.
    * Can use debug files created by XA80 cross assembler to access labels and step through source lines
    * Can direct load files into VM memory from host operating system

## Dependencies
To use a basic CP/M operating system, this software requires the following components:

1. Monitor image to allow the initial boot. I'm favouring the Grant Searle version as it's popular with many retro-Z80 designs
2. BIOS image to support the following
	* Z80-ZIO using ports $00..$03
	* Compact Flash IDE interface using ports $10..$17
3. A language or operating system, one of the following:
    * Microsoft BASIC image
    * CP/M image containing:
        * BDOS (Basic Disk Operating System)
        * CP/M CCP (Console Command Processor)
    * Something else

None of the items above can be included with this distribution as they have their own licensing terms. Terms for use of the software can be found on the respective websites and in the source code for the different components. The documents section contains dependencies.pdf which explains how to download and assemble the modules listed
above (not implemented).

## Development Environment
Box80 is written in [Free PASCAL](https://www.freepascal.org/) hosted under the [Lazarus IDE](https://www.lazarus-ide.org/). To modify and compile this software, you will need [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) 2.1.0 or later. It has been
tested on Windows. There are no "OS specific" twists, so it should be possible to recompile on other hosts
which are supported by the Lazarus ecosystem in 32 and 64 bit flavours, including:

* Android
* FreeBSD
* iOS
* Linux (known to have problems at this time)
* macOS
* Raspberry Pi
* WinCE
* Windows

You will need at least a 32 bit Operating System, and some sort of multi-threaded environment.

## Development status
### V0.0 current development
| Item                          | Complete  | Notes                                  |
| ----                          | --------: | -----                                  |
| Dual thread Z80 core          | 100%      | Windows OK ~~Linux causing fault~~     |
| Standard Z80 instructions     | 100%      | Needs to be run against a test suite   |
| All Z80 instructions          | 100%      | Standard, doesn't include undocumented |
| Terminal output               | 50%       | Vanilla, some ctrl codes, needs ANSI   |
| Interrupt processing          | 33%       | IM2 is complete, trigger by SIO        |
| SIO                           | 10%       | Very basic implementation, but works   |
| CF card interface             | TBD       | Just about to start on this            |
| Breakpoints                   | TBD       |                                        |
| Disassembler                  | TBD       |                                        |
| Watch window                  | TBD       |                                        |
| Use of debug files            | TBD       | XA80 needs to create these first :)    |
| Documentation                 | TBD       |                                        |
### V0.1 future development
| Item                          | Complete  | Notes                                  |
| ----                          | --------: | -----                                  |
| Undocumented instructions     | TBD       | 489 undocumented instructions to add   |
| Create IDE                    | TBD       | Integrate Box80, XA80 and an IDE      |

## History
The Box80 application is part of a small, but growing ecosystem I've been putting together.

40+ years ago, I wrote
assemblers in BASIC, then moved to writing them in assembler itself, for machines like the Apple II (6502) and 
Sinclair ZX81 (Z80). I didn't understand the concept of parsers and the processing was crude. But they worked
and I learnt a lot from them.

As part of my journey, I've been making more of an effort to understand the science of how these things
operate and in 2020 wrote [LaCoGen](https://github.com/duncanamps/lacogen1), an open-source **La**zarus **Co**mpiler **Gen**erator.
It was basically my own home-brewed version of LEX and YACC, the famous Lexer/LALR1 compiler generator tools
from the C fraternity, to enable me to have this facility on my favourite Lazarus environment. It was written from
the ground up using first principles and knowledge I gained from the [Dragon book](https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools).

Using this tool, I was able to write [XA80](https://github.com/duncanamps/xa80), an open-source cross-platform
assembler for x80 processors; 8080, 8085, Z80 and Z180. It contains an opcode compiler which was, itself, developed
with LaCoGen.

I own a number of Z80 based SBCs; the RC2014, the SC131 and have recently acquired the AgonLight 2 SBC
from Olimex. This is a 24 bit EZ80 processor and will undoubtedly result in enhancements to XA80 to cover 24 bit
activities. But that's for another day...

The development cycle can be time consuming: Write Z80 assembly language on the PC. Assemble it. Download to the target device. Run on the target device. Figure out why it won't work....

For now, it seemed like a good idea to write a simulator to mimic one of the SBCs so that I could eventually integrate
this all into an IDE with a Z80 virtual machine, assembler, debugger, etc. This will make it so much easier to work the
bugs out of my code, and will drastically shorten the development cycle.

That's the history so far; XA80 is working well and in fact was used to assemble the monitor, CP/M, MS BASIC, etc. Box80 is only part complete but is showing promising results so far; you can run MS BASIC on it, CP/M next.

## Licence and acknowledgements
All trademarks are acknowledged as belonging to their respective owners.  

This software product is not commissioned or endorsed by the semiconductor manufacturers.  

Box80 is licensed under the GNU GPL Licence V3 or later.  

External components which are referenced in this text (monitor, BIOS, CP/M, BASIC) are not included with this distribution
and have their own licensing and distribution terms. 

## Contact
The author is Duncan Munro  
Twitter: @duncanamps  
Email: duncan@duncanamps.com  
