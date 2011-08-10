## Plan for ghc-xcode:

Invoke like:

    ghc-xcode -threaded -XScopedTypeVariables -O2 ... Foo.hs Bar.hs

where Foo.hs, Bar.hs, etc. have foreign export calls in them.

## If not run as part of an Xcode build

1. Mention that it's not run as part of an xcode build.
2. Mention the `_stub.h` files that need to be added, plus `module_init.c`
3. and also the linker flags that need to be added (just -liconv, for now?
   better to pull from package...
4. Mention the -I that's necessary for `HsFFI.h`
5. Print a list of the object files that will be used

## If run as part of an Xcode build

1. Compile all modules passed in, plus other modules which they depend on.
   (Don't needlessly recompile.)
2. Generate a module_init.c file which calls hs_init and hs_add_root.
3. Append `LINK_FILE_LIST_...` with all of the necessary object files.

## TODOs

- Actuall implement all of the above
- Print a status message for each module as we compile it, and
  make it more clear which module each error message belongs to.
- Only compile the Haskell files; don't load them.  (The GHC API makes it
  easier to compile-and-load than to just compile, but the former seems
  less efficient when no files need recompilation.)
- Handle `-threaded` on the command-line.
