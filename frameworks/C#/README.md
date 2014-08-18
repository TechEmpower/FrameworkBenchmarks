# Limited Travis-CI Verification 

Because the verification system uses the linux-only Travis-CI
service, verifying Windows-only tests has to be done manually
and is very time consuming. 

Consider including an additional test, likely based on Mono and 
FastCGI (e.g. xsp), that can run on Linux when submitting a new
framework. This will drastically speed up our ability to merge
in your pull request. 

# Different Mono Versions

While we have not currently run into the need to have multiple 
simultaneous Mono installations, it is [possible](http://www.mono-project.com/docs/compiling-mono/parallel-mono-environments/)
