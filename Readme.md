### Overview

[![Build Status](https://www.donchess.net)](https://www.donchess.net)

DON is a free UCI chess engine. It is not a complete chess program
and requires some UCI-compatible GUI (e.g. XBoard with Polyglot,
eboard, Arena, Sigma Chess, Shredder, Chess Partner or Fritz)
in order to be used comfortably. Read the documentation for GUI
of your choice for information about how to use engine with it.

### Features

DON uses bitboard representations, and is an alfa-beta searcher.

DON supports up to 512 cores. The engine defaults to one search thread,
so it is therefore recommended to inspect the value of the 'Threads'
UCI parameter, to make sure it equals the # of CPU cores on your computer.

DON supports up to 1 TB (1024 GB) (1048576 MB) of hash memory.

DON has support for Large Memory Pages when user has permission of using it
other-wise use Default Memory.

DON has support for 32 or 64-bit CPUs, the hardware ABM/BMI instruction,
big-endian machines such as Power PC, and other platforms.

DON has support for Polyglot book.
For information about how to create such books, consult the Polyglot documentation.
The book file can be selected by setting the *Book File* UCI parameter.

DON has support for Syzygybases.

### Files

This distribution of DON consists of the following files:

  * Readme.md, the file you are currently reading.

  * Copying.txt, a text file containing the GNU General Public License.

  * src, a subdirectory containing the full source code, including a Makefile
    that can be used to compile DON on Unix-like systems.

### Syzygybases

**Configuration**

Syzygybases are configured using the UCI options:
"SyzygyPath", "SyzygyProbeDepth", "SyzygyLimitPiece" and "SyzygyUseRule50".

"SyzygyPath" option should be set to the directory or directories that contain
the .rtbw and .rtbz files. Multiple directories should be separated by
";" on Windows and by ":" on Unix-based operating systems.
**Do not use spaces around the ";" or ":".**

Example: `C:\tablebases\wdl345;C:\tablebases\wdl6;D:\tablebases\dtz345;D:\tablebases\dtz6`

It is recommended to store .rtbw files on an SSD. There is no loss in
storing the .rtbz files on a regular HD.

"SyzygyProbeDepth" option set the lets the engine probe depth.
Set this option to a higher value if you experience too much
slowdown (in terms of nps) due to TB probing.

"SyzygyLimitPiece" option normally should be left at its default value.

"SyzygyUseRule50" option set the drawn by the 50-move rule to count as win or loss / draw.
'true' -> draw
'false' -> win or lose
This may be useful for correspondence games (because of tablebase adjudication).

**What to expect**
If the engine is searching a position that is not in the tablebases (e.g.
a position with 7 pieces), it will access the tablebases during the search.
If the engine reports a very large score (typically 123.xx), this means
that it has found a winning line into a tablebase position.

If the engine is given a position to search that is in the tablebases, it
will use the tablebases at the beginning of the search to preselect all
good moves, i.e. all moves that preserve the win or preserve the draw while
taking into account the 50-move rule.
It will then perform a search only on those moves. **The engine will not move
immediately**, unless there is only a single good move. **The engine likely
will not report a mate score even if the position is known to be won.**

It is therefore clear that behaviour is not identical to what one might
be used to with Nalimov tablebases. There are technical reasons for this
difference, the main technical reason being that Nalimov tablebases use the
DTM metric (distance-to-mate), while Syzygybases use a variation of the
DTZ metric (distance-to-zero, zero meaning any move that resets the 50-move
counter). This special metric is one of the reasons that Syzygybases are
more compact than Nalimov tablebases, while still storing all information
needed for optimal play and in addition being able to take into account
the 50-move rule.

### Compiling it yourself

On Unix-like systems, it should be possible to compile DON
directly from the source code with the included Makefile.

In general it is recommended to run `make help` to see a list of make
targets with corresponding descriptions. When not using the Makefile to
compile (for instance with Microsoft MSVC) you need to manually
set/unset some switches in the compiler command line;
see file *Platform.h* for a quick reference.


### Terms of use

DON is free, and distributed under the **GNU General Public License** (GPL).
Essentially, this means that you are free to do almost exactly what
you want with the program, including distributing it among your friends,
making it available for download from your web site, selling it
(either by itself or as part of some bigger software package), or
using it as the starting point for a software project of your own.

The only real limitation is that whenever you distribute DON in some way,
you must always include the full source code, or a pointer to where the
source code can be found. If you make any changes to the source code,
these changes must also be made available under the GPL.

For full details, read the copy of the GPL found in the file named *Copying.txt*
