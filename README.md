# Box80 V0.0
**One liner:** Box80 - A cross-platform open-source Virtual Machine to run and debug Z80 code.

## Description
Box80 is a multi-platform virtual machine designed to execute Z80 code in a 64K code space. The intended usage is for running CP/M 2.2 and debugging assembly language apps developed for that operating system.  

The main objective is to have a rich and capable cross-platform environment for debugging of CP/M assembly language programs, rather than to serve as Yet Another Emulator for CP/M.

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

1. Monitor image to allow the initial boot
2. BIOS image to support the following
	* Z80-ZIO using ports $00..$03
	* Compact Flash IDE interface using ports $10..$17
3. A language or operating system, one or both of the following:
    * Microsoft BASIC
    * CP/M image containing:
        * BDOS (Basic Disk Operating System)
        * CP/M CCP (Console Command Processor)

None of the items above are included with this distribution as they have their own licensing terms which can be found on the website and in the source code
for the different components. The documents section contains dependencies.pdf which explains how to download and assemble the four modules listed
above (not implemented).

## Development Environment
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


## Development status
| Item                          | Complete  | Notes                                 |
| ----                          | --------:  | -----                                 |
| Dual thread Z80 core      | <span style="color:darkblue">100%</span>      |                                       |
| Standard Z80 instructions     | <span style="color:darkgreen">96%</span>       | Needs to be run against a test suite  |
| All Z80 instructions          | <span style="color:darkorange">46%</span>       | Very little done on CB/DD/ED/FD       |
| Terminal output               | <span style="color:darkorange">50%</span>       | Vanilla, some ctrl codes, needs ANSI  |
| Interrupt processing          | <span style="color:darkorange">33%</span>       | IM2 is complete, trigger by SIO       |
| SIO                           | <span style="color:darkred">10%</span>   | Very basic implementation             |
| CF card interface             | TBD | |
| Breakpoints | TBD |  |
| Disassembler                  | TBD       |                                       |
| Watch window                  | TBD       |                                       |
| Use of debug files | TBD | XA80 needs to create these first :) |
| Documentation | TBD | |

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
