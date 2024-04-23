# G-NAV
G-NAV is an electronic flight instrument system (EFIS) for soaring that runs as a progressive web application (PWA) in most mobile web browsers.
G-NAV is almost enterily written in Ada and it is compiled to WASM through AdaWebPack. Furthermote, G-NAV fully relies on WebGL for graphics.
There is also a native version of the software that you can find [here](https://github.com/GuillermoHazebrouck/gnav).

<img src="./docs/gnav_1.jpg" width="400">

Visit the project [website](https://sites.google.com/view/g-nav/news) for a demo and more infomation.

## Operation
To operate G-NAV, please read the user's manual located in the docs folder. Be aware that the project is still under development.

## Compiling
G-NAV web consists of two modules: the web application itself and the data compiler. For the web app you will need AdaWebPack. The copilation procedure is similar to that of the examples you will find there.

For the data compiler you will need a native Ada FSF compiler.

## Releases
Keep an eye on the [releases](https://github.com/GuillermoHazebrouck/gnav-web/releases), there will be precompiled versions in the future.

## Credits
This software is made possible thanks to:
- Guillermo Hazebrouck for everything in this repo (G-NAV)
- Vadim Godunko and Maxim Reznik for [AdaWebPack](https://github.com/godunko/adawebpack)
- The Ada FSF community
