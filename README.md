## Summary

The `ghc-xcode` program makes it easier to develop 
graphical Haskell applications on OS X and iOS.
It integrates into the XCode build system as a custom build
phase.  The programmer uses `foreign export` 
declarations to export a C API for their Haskell
code.  The `ghc-xcode` program ensures that all of the
necessary object files will be compiled and linked together 
with the rest of the XCode project.

This package has only been tested with XCode-4.1.  It also 
currently requires ghc-7.2 or later.

## Installation

This package may be installed by calling the following code from within the
ghc-xcode directory:

    cabal update; cabal install 

## Instructions

1. Copy the necessary Haskell modules into the directory that contains the
`.xcodeproj` file.  We will call this the "XCode directory".

2. `cd` into the XCode directory.  Run:

        ghc-xcode [args] [modules]

    where:

         - [args] is a list of compiler flags (e.g.: `-O2`, `-threaded` or `-XScopedTypeVariables`)
         - [modules] is a list of paths to the modules which contain `foreign export` declarations

3. The script will do the following :
    - Perform an initial compile of the [modules] as well as any
other module files which they `import`.  
    - Generate a C file named `_hs_module_init.c` which calls
      `hs_init`, `hs_exit` and `hs_add_root` in C constructors/destructors.
    - Print a list of instructions for how to
    add the Haskell source code to the XCode project.  For example:

            Compiling...
            Succeeded.
            You will need to make the following changes in XCode:
              * Add Header Search Paths:
                  /Library/Frameworks/GHC.framework/Versions/7.2.1-x86_64/usr/lib/ghc-7.2.1/include
              * Add Other Linker Flags:
                  -liconv
                  -Wl,-no_compact_unwind,-no_pie
              * Add the following files to XCode:
                  _hs_module_init.c
                  FibTest_stub.h
              * Add a "Run Script" build phase which occurs before the
                "Compile Sources" phase and calls: 
                  /Users/judah/.cabal/bin/ghc-xcode haskell/FibTest.hs
            
    Follow those instructions.

4. Try building the XCode project.  The `ghc-xcode` build phase will rebuild
the modules and their dependencies as necessary.  It will also tell
XCode's link step to include the 
necessary module and package object files.
    
    If it fails, check the build logs; the most likely culprit is a missing C
    library or framework.  These may be added in the XCode target's Build
    Settings.

This project is still somewhat experimental, so let me know if you run into
any problems getting it set up.

## TODOs
- Only compile the Haskell files; don't load them.  (The GHC API makes it
  easier to compile-and-load than to just compile, but the former seems
  less efficient when no files need recompilation.)
- Display all necessary linker flags like `-liconv` which are specified in the
  Haskell packages.  (This is currently hard-coded.)
- XCode recompiles every file every time (including the precompiled header);
  why does that happen and how can we prevent it?
- Print a status message for each Haskell module as we compile it.
