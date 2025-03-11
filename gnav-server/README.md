# G-NAV Web server

This is the full G-NAV web server system capable of handling HTTP/2 server-client communications, consuming real time tracks from the OGN APRS servers and fetching updated METAR data from a set of https servers.

The configuration of the G-NAV server is done from the gnav.ini file. You will have to set:

- The TCP port
- The reference position of the system
- The OGN APRS URL and port
- The track coverage range
- The set of METAR station (position, name and URL)

The server file runs from the ./server folder. Make sure you add there the appropriate key and certificate files (refer to the AdaWebServer documentation).

> [!IMPORTANT]
> Note that we run on a customized AdaWebServer. The source code is a fork from version 24, where we have simplified the structure and removed the modules that where not necessary for this project.
