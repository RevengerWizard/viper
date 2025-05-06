# Viper

Viper programming language.

Just a toy compiler to understand how compilers work.

```
fn main() : int32
{
    return 42
}
```

## Building

You will need a C99 compiler (gcc or clang) and make

```bash
git clone https://github.com/RevengerWizard/viper && cd viper
make
```

Run it using `vxc`, followed by the name of the file ending with the `.vp` extension:

```bash
vxc hello.vp
```

## License

Licenced under MIT License. [Copy of the license can be found here](https://github.com/RevengerWizard/viper/blob/master/LICENSE)