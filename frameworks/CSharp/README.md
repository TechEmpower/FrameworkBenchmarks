# C# (CSharp) Frameworks

The information below contains information specific to C#. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).

## Infrastructure Software Versions

* [Mono C# compiler latest version](http://www.mono-project.com/docs/about-mono/languages/csharp/)

## Adding a New C# Framework

### Different Mono Versions

While we have not currently run into the need to have multiple 
simultaneous Mono installations, it is [possible](http://www.mono-project.com/docs/compiling-mono/parallel-mono-environments/)

### Debugging Mono + NuGet

Mono understands an environment variable `MONO_LOG_LEVEL=debug` 
that is helpful for checking that 
mono is properly working e.g. loading necessary DLL's. 

Most NuGet commands understand a `-Verbosity` flag, which is 
great because the error messages can be completely
mystifying when working with Mono too. Use this would enable 
all the debugging you can: 

    $ MONO_LOG_LEVEL=debug mono NuGet2.exe update -Verbosity "detailed" -self

For example, aspnet was constantly failing with this message: 
    
    Could not connect to the feed specified at 'https://www.nuget.org/api/v2/'. Please verify that the package source (located in the Package Manager Settings) is valid and ensure your network connectivity.`. 

Using `-Verbosity "detailed"` shows that the real error is 
actually a Mono library problem, as so: 

    System.InvalidOperationException: Could not connect to the feed specified at 'https://www.nuget.org/api/v2/'. Please verify that the package source (located in the Package Manager Settings) is valid and ensure your network connectivity. ---> System.Net.WebException: libMonoPosixHelper.so ---> System.DllNotFoundException: libMonoPosixHelper.so
      at (wrapper managed-to-native) System.IO.Compression.DeflateStreamNative:CreateZStream (System.IO.Compression.CompressionMode,bool,System.IO.Compression.DeflateStreamNative/UnmanagedReadOrWrite,intptr)
      <snip>

More helpful info is [here](http://www.mono-project.com/docs/advanced/pinvoke/dllnotfoundexception/), [here](http://docs.nuget.org/docs/reference/command-line-reference)

### Limited Travis-CI Verification 

Because the verification system uses the linux-only Travis-CI
service, verifying Windows-only tests has to be done manually
and is very time consuming. 

Consider including an additional test, likely based on Mono and 
FastCGI (e.g. xsp), that can run on Linux when submitting a new
framework. This will drastically speed up our ability to merge
in your pull request. 

## Get Help

### C# Experts

_There aren't any C# experts listed, yet. If you're an expert, add yourself!_

### C# Community

_We don't have any community links added. Add some to help further guide 
future contirbutors._

### Resources

_If you stumble upon some helpful links or discussions, add them 
for easy reference for future contributors._
