# ppm2pgm

Build:

```
$ stack build
```

Run:

```
$ stack exec -- ppm2pgm-exe ~/Pictures/8-bit_mario.ppm
```

Build with profiling enabled:

```
$ stack build --profile
```

Run with profiling enabled:

```
stack exec -- ppm2pgm-exe ~/Pictures/8-bit_mario.ppm +RTS -p
```

This will generate the `ppm2pgm-exe.prof` file.
