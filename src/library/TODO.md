
TODO
====

This file contains todo-items for the CubedOS support library.

+ **Tests**. More tests are needed for some of the components of this library.

+ **Compression Algorithms**. The `Compressors` package is a placeholder. We should implement
  some real compression algorithms. Possibilities include: CCSDS 121.0-B-* (lossless), CCSDS
  122.0-B-* (image), ZIP, LZW, etc.

+ **Cryptography**. Although confidentiality is not often used in space systems (yes?),
  authentication is important so spacecraft will ignore rogue commands. Which standards are
  appropriate to implement?

+ **Error Detection**. There is a single CRC implementation in the library. Other polynomials
  should be implemented. Other error detection methods should be implemented.

+ **Error Correction**. This is close related to error detection and is very important in space
  missions due to the very long latency of space communications. We absolutely should have some
  support for error correction algorithms.

+ **Heap Subprograms**. The heap-manipulating subprograms in package `Sorters` need to be
  implemented. They will ultimately be used to priority-sort the mailboxes.

+ **XDR Float**. The encoding/decoding of floating point types using XDR is not implemented.
  Eric Edlund wrote an implementation that we could start with, but SPARK has issues with it, so
  it will need review, at least.

+ **Space Packets**. Are the `Space_Packets` and `TC_Frames` packages finished?

+ **Radio Protocols**. X.25? Anything else?

+ **Network Protocols**. IP? IPv6? UDP? 6LoWPAN? Note that the DTN protocols are being developed
  in a separate repository.
