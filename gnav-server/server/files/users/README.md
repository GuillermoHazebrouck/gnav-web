# G-NAV users data storage
This folder is automatically filled with IGC files constructed while recording the data from registered user's requests and local FLARM devices.

Local users and FLARM devices must be registred on the `gnav.ini` file (located at the server's root directory) and are loaded at startup. For example:

`LOCAL_DEVICE=DDA9E1,OO-ABC,AB`: the FLARM ID is always a 6-characters hexadecimal word, the registration a 6-characters uppercased alphanumeric word (eventually spaced using a strip), and the tail mark a 2-characters uppercased alphanumeric word.

`LOCAL_USER=GUEST123`: local users are registered using an 8-characters uppercased alphanumeric word. This word must be entered on the client system as the SQUAUWK code. Since version 5B, non-registered users are not recorded.
