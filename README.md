# Viper

Viper programming language.

Just a toy compiler to understand how compilers work.

```
fn main() : int32
{
    return 42;
}
```

## Building

You will need a C99 compiler (gcc or clang) and make. 

```bash
git clone https://github.com/RevengerWizard/viper && cd viper
make
```

You can run the compiler using `vxc`, followed by the "mode" `comp` and the name of the file ending with the `.vp` extension.

You may also need a linker like `ld` to link the object files:

```bash
vxc comp hello.vp
ld hello.obj -o out.exe --subsystem=console -e main
./out
```

## License

Licenced under MIT License. [Copy of the license can be found here](https://github.com/RevengerWizard/viper/blob/master/LICENSE)