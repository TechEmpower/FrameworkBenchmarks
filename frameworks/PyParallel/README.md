# PyParallel Framework

PyParallel is an experimental fork of the CPython 3.3 code base geared toward
optimally exploiting underlying hardware as efficiently as possible.  It
features an even more experimental, high-performance asynchronous I/O TCP/IP
stack built around Windows completion ports, synchronization primitives,
thread pools and overlapped I/O.

PyParallel is developed by Trent Nelson (@tpn) and is currently hosted at
[Bitbucket](https://bitbucket.org/tpn/pyparallel).

The following Speakerdeck presentation provides more information behind how
PyParallel is able to run multiple interpreter threads in parallel:
[PyParallel: How we removed the GIL and exploited all cores](https://speakerdeck.com/trent/pyparallel-how-we-removed-the-gil-and-exploited-all-cores).

PyParallel has in-built support for the JSON and plaintext TEFB tests.
