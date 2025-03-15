# G-NAV web client
This is the source code of the G-NAV client WebAssembly module that runs on the internet browser at user's side. To compile this project you need _AdaWebPack_ installed in a subdirectory named `awp`. Then you run `gprbuild gnav.gpr`.

It is recommended to use the G-NAV web server to test the module during development, bacause it can serve the dynamic data (traffic and meteo). However, the app can also be supported using another web server.

The simplest way to debug the app is by tracing messages to the console and launching the web development tools on the browser (most of them provide this function).

> [!TIP]
> To make your life easier you can just use the released version of _AdaWebPack_. You will need LLVM 14.0 for this.
> Here you will find a modified version of the `adawebpack.mjs` file, which is temporarily necessary to include extra features and overcome issues.

