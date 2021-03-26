# faf
FaF is a Linux webserver written in Rust. It has a single goal: to demonstrate the upper bound of possible single-node performance. It is meant as a living reference project and may have cutting edge dependencies. Being a reference project, documentation and simplicity are essential and will be maintained to the best of my ability.

[FaF Repo](https://github.com/errantmind/faf)

## Requirements and How-To

FaF requires:
* linux x86_64
* nightly Rust
* [clang-12 and lld-12](https://apt.llvm.org/) to be installed and available in PATH. The version (i.e. 12) may change over time as Rust updates its LLVM version


To use FaF for your own purposes, edit the callback in `epoll_callback.rs`

Build and run FaF with the following commands. Remove `-Cprofile-use=/faf/merged.profdata` below if you don't know how to use [PGO](https://doc.rust-lang.org/rustc/profile-guided-optimization.html) with Rust or have not generated and merged profiles for yourself. If you neglect this step the build will fail or you will have degraded performance. In my tests, PGO doesn't add all that much performance in this project (~1%) so you won't be missing out on much
```
# Build (from FaF project directory):
RUSTFLAGS="-Ctarget-cpu=native -Clinker=/usr/bin/clang-12 -Clink-arg=-fuse-ld=lld-12 -Clink-arg=-flto=thin \
   -Clto=thin -Cembed-bitcode=yes -Copt-level=3 -Ccodegen-units=1 -Cforce-frame-pointers=n -Cprofile-use=/faf/merged.profdata" \
   /root/.cargo/bin/cargo build --verbose --release -Z build-std --target=x86_64-unknown-linux-gnu \
   && strip --strip-all target/x86_64-unknown-linux-gnu/release/faf

# Run
./target/x86_64-unknown-linux-gnu/release/faf
```

## Design Principles
1. Speed. Optimize for serving small to moderate payloads to a large number of concurrent connections. Speed will be balanced against over-specialization, like rewriting the entire project in hand-optimized assembly. Speed optimizations are constrained to unsafe Rust, unless the results of some alternative approach are overwhelmingly convincing
2. Elegance as Simplicity. Consistency in the architecture, project structure, idioms, and style. Some of these idioms and styling are particular to my taste. 'Right tool for the right job' approach when choosing data structures and algorithms
3. Memory Safety. This being third is generally at odds with the Rust community, but if you have read all the text above you will understand. Memory safety is prioritized in-so-far as it has no decernable effect on performance. It should also be implemented in such a way that doesn't add excessive complexity / indirection in the project

(in that order)

## Decisions
* Use epoll for event loop as io_uring is not fully stabilized (as of 2021/03/24) and TechEmpower's environment isn't using the mainline kernel. Strive to minimize system calls
* Use separate epoll instances on separate threads, one per CPU core, to handle all the 'work' (read / writes)
   * Messing with priority and affinity of these threads negatively impacted performance (20+% reduction)
   * Numa is not an issue in the current test environment so less gains to be had with thread affinity
* Minimize dependencies, even if that means rewriting or stripping other projects
   * Some dependencies are less optimized than they could be, which turns out to be most of them. A simple example of this is not marking functions as `#[inline]`, which, in Rust, ends up preventing them from being inlined across crate boundaries
   * Some dependencies have 'nasty hacks' which break performance optimizations (like LTO)
* Instead of generating the date header once per second on a separate thread and then worrying about locks/mutexes, optimize the date function itself to the point a separate thread is no longer needed
   * I succeeded in reducing this function to ~20ns which is acceptable and potentially even faster than putting it on a separate thread once you account for locks
* Nothing should be converted to a string that doesn't need to be. Put another way, everything that can remain in byte (u8) sequence should stay that way. For example, why do most frameworks convert the date to a string and then back to a byte sequence? This is unnecessary work

## Contributions
Contributions are welcome, but please discuss before submitting a pull request. If a discussion leads to a pull request, please reference the \#issue in the pull request. Unsolicited pull requests will not be reviewed nor merged.

Any and all contributions, unless otherwise specified, will be licensed under the license specified by this project (below). All contribitors, by submitting contributions as a pull request (or in any other form), agree to these terms.


## License
Please see the LICENSE file in the root directory of the project. The gist is, the code is licensed under the AGPL, however project sponsors can alternatively license the code under <ALTERNATIVE_LICENSE>, which is lawfully applicable while they maintain their sponsorship. If sponsorship lapses, their license immediately reverts back to AGPL.

Why this license structure? Sustainability. It takes time and effort to maintain and improve this project and, like it or not, sustainable projects usually require funding. That said, I will never gate any features. All contributions (from sponsor and non-sponsors), now and forever, will be available in this repository under the AGPL. This feels like the best of both worlds
