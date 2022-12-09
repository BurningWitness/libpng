### This library is unfinished (and arguably impossible to finish)

Since `libpng` uses `setjmp`/`longjmp` for error handling and there is no way to escape this, either every single call that **may** throw an error needs to be wrapped at the FFI level or these bindings need to be mid-level, wrapping entire C sequences.

Alternative library: [libspng](https://github.com/BurningWitness/libspng).

# libpng

Bindings to the [libpng](http://www.libpng.org/pub/png/libpng.html) C library (version 1.6.37).

You can find the manual [here](http://www.libpng.org/pub/png/libpng-1.4.0-manual.pdf).
