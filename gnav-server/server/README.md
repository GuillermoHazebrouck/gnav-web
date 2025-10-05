# G-NAV web server structure
This directory contains the operational G-NAV server structure.
The `aws.ini` file is optional and it overrides some default setup variables of the AdaWebServer.

The `gnav.ini` file is compulsory and it should be adapted with the local configuration. This file must contain:

- The TCP port number for the HTTPS connection.
- The reference latitude and longitude position (used to subscribe to OGN tracks).
- The OGN APRS server URL.
- The OGN APRS server port number.
- The OGN required coverage radius.
- The defition and source URL's of METAR stations.
- The definition of local FLARM devices to be recorded.
- The definition of local users to be recorded.
