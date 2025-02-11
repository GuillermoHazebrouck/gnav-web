# G-NAV 
<img src="./docs/g-nav-logo-small.png" width="80">
G-NAV is an electronic flight instrument system (EFIS) for soaring that runs as a progressive web application (PWA) in most mobile web browsers (Chrome, Safari and others).
The application can be installed locally in the client browser and it starts up and runs offline like a native app.

G-NAV is almost entirely written in Ada and it is compiled to [WASM](https://webassembly.org/) through [AdaWebPack](https://github.com/godunko/adawebpack). Furthermore, G-NAV fully relies on [WebGL](https://www.khronos.org/webgl/) for graphics.

<img src="./docs/gnav_1.jpg" width="400">

> [!NOTE]
> The repository is behind the published version. There will be an update this year, jumping directly to version 3C.

Visit the project [website](https://go-gliding.app) for demo servers and more infomation.

There is also a native version of the software that you can find [here](https://github.com/GuillermoHazebrouck/gnav). The applications are quite similar, although there will be more and more discrepancies as this project evolves.

## Operation
G-NAV provides navigation functions, real time data feed and awareness alerts.

To operate G-NAV, please read the user's manual located in the `docs` folder. This document covers the user functions and the web system administration. Be aware that the project is still under development.

> [!CAUTION]
> G-NAV is not a certified IFR equipment! You should never use this device as a primary source of navigation or collision avoidance.

## System architecture
This web version of G-NAV consist of a client application that makes HTTP requests to a specialized server. The server has access to the necessary static files (terrain, airspaces, references, etc.) and it also connects to different online data sources to provide real time traffic and meteo information.

For the traffic data, the server is able to connect to the OGN APRS servers via TCP, parse the text messages on the fly and feed an internal stack. This information residing in main memory is then used to generate a highly compressed and customized response for each client, containing only tracks around the provided location. If the follow function is activated, then the server records the provided location in an IGC file that can be retrieved afterwards using the assigned squawk code.

For the meteo data, the server can be configured to obtain metar messages from different stations via HTTP requests. The server automatically checks for updates and periodically generates a compressed response for the clients. The client then dynamically selects the closest station during the flight and can automatically feed the current wind and QNH values.

## Compiling
G-NAV web consists of three modules: the web application itself, the web server and the data compiler. For the web app you will need AdaWebPack. The compilation procedure is similar to that of the examples you will find there.
For the data compiler you will need a native Ada FSF compiler.
For the web server you will additionally need a basic structure of the AdaWebServer.

> [!TIP]
> To make your life easier you can just use the released version of AdaWebPack. You will need LLVM 14.0 for this.
> Also, in the `src/patch/` directory you will find a modified version of `adawebpack.mjs`, which is temporarily necessary to include new features and overcome issues.

## Releases
Keep an eye on the [releases](https://github.com/GuillermoHazebrouck/gnav-web/releases), there will be precompiled versions in the future.
The provision of data is not part of the project yet, you will need to collect your own dataset to build a functional system.

## Credits
This software is made possible thanks to:
- Guillermo Hazebrouck for everything in this repo (G-NAV)
- Vadim Godunko and Maxim Reznik for [AdaWebPack](https://github.com/godunko/adawebpack)
- The Ada FSF community
