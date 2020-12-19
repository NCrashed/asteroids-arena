# Windows build

## Adding SDL2

Download VC development archive: https://www.libsdl.org/download-2.0.php

Adjust build.zig at:
```
// Adjust to your binary installation of SDL2
if (builtin.os.tag == std.Target.Os.Tag.windows) {
    exe.addIncludeDir("C:\\SDL2-2.0.12\\include"); // I moved all .h files to SDL2 subfolder
    exe.addLibPath("C:\\SDL2-2.0.12\\lib\\x64");
}
```
And copy `SDL2.dll` to current folder.
