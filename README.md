# Viper

Viper programming language.

Just a toy compiler to understand how compilers work.

```vp
pub fn main() : int32
{
    io::init();
    let err : io::IOError;
    io::write(io::stdout, "hello\n", 6, &err);
    return 0;
}
```

## Building

You need GNU Make and a C99 compiler such as `gcc` or `clang`.

```bash
git clone https://github.com/RevengerWizard/viper && cd viper
make
```

This builds the compiler as `vxc`/`vxc.exe` in the repository root.

## Compiling

Use `vxc check` to validate a source file without producing any file:

```bash
vxc check test/hello.vp
```

Use `vxc comp` to compile a Viper source file to an object file:

```bash
vxc comp test/hello.vp
```

To build a runnable program, link the generated object with the standard library objects and set the entry point to `main`:

```bash
vxc comp test/hello.vp -o hello.obj
ld hello.obj std/*.obj -o out.exe --subsystem=console -e main -lkernel32
./out
```

## Examples

The `example/` directory contains OpenGL samples. If `vxc` and `ld` are available on `PATH`, they can be built from that directory with:

```bash
cd example
make cube
make triangle
```

The example Makefile finds the `std` object files relative to the `vxc` executable and links the platform libraries needed by the samples.

## Tests

Tests live under `test/` and are ordinary `.vp` files with `//!` directives describing the expected stage, output, and exit status.

```bash
python test.py
```

The test runner compiles in parallel, uses per-test temporary directories under `test/tmp`, links run tests with `ld -e main`, and compares the result against the directives in each test file.

## License

Licenced under MIT License. [Copy of the license can be found here](https://github.com/RevengerWizard/viper/blob/master/LICENSE)
