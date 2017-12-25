# ppm2pgm

A small hello-world like program to convert between image formats written in Haskell.

Build:

```
$ stack build
```

Run:

```
$ stack exec -- ppm2pgm-exe img/inp.ppm img/outp.pgm
```

Build with profiling enabled:

```
$ stack build --profile
```

Run with profiling enabled:

```
stack exec -- ppm2pgm-exe img/inp.ppm img/outp.pgm +RTS -p
```

This will generate the `ppm2pgm-exe.prof` file.
