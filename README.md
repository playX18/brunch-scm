# brunch-scm 

Simple benchmarking library based on Rust's [`brunch`](https://github.com/Blobfolio/brunch/) crate. Works on any R7RS compatible system and does not require any dependencies.

# Example usage:
```scm
(import (brunch))

(benches (my-benches "out.csv")
    (bench "foo" 
        (begin 
            (let loop ([i 0])
                (if (< i 1000) (loop (+ i 1)))))))

```

Or also take a look at `simple.scm` in `examples/` folder.
