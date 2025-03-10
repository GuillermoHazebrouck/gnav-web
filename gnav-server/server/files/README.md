# G-NAV web server folder

This folder contains the full dataset for the operational web server, including the data and the web infrastructure. This comprises:

- The HTML file
- The WASM excecution module
- The service worker
- The PWA manifest
- The AdaWebPack glue code

> [!IMPORTANT]
> The compiled web client WebAssembly module 'main.wasm' needs to be in this folder. Normally, the Ada compiler is instructed to do that automatically via the gpr file.

Note that the 'index.html' file contains the JavaScript code to load and excecute the G-NAV wasm module.
