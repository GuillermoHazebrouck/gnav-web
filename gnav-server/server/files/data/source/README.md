# G-NAV web client
This folder contains the original raw data for your system. You will need _gnav_crunch_ to compile this data into a binary format for the client application.
gnav_crunch will also check the consistency of the data, so that loading-time errors can be avoided.
Make sure that there is a version of _gnav_crunch_ in this folder.

## Compiling the aircraft data
Aicraft data must be loaded using the aircraft.dat file in the `aircraft` folder. You can use the Scilab scrips to generate the polynomial coefficients for the polar curves. This will also return the CL range and generate interesting plots about the aircraft performance. To compile the data with _gnav_crunch_, you need to enter the AIRCRAFT keyword:

```
cd aircraft
./../gnav_crunch AIRCRAFT
mv aircraft.bin ../../
```

## Compiling the airspace data
Airspaces must be declared in a standard open air file named airspaces.air in the `airspaces` folder. You need to enter the AIRSPACE keyword along with the latitudinal and longitudinal coordinates of the map bounds:

```
cd airspaces
./../gnav_crunch AIRSPACE S=49:12:59N W=002:08:37E N=51:49:06N E=006:45:51E
cp airspaces.bin ../../
```

## Compiling the reference data
You can use the cities.csv and the airport.csv files in the `reference` folder to extract reference data worldwide. You need to enter the REFERENCES keyword along with the latitudinal and longitudinal coordinates of the map bounds and the minimum population of the cities you want to include:

```
cd reference
./../gnav_crunch REFERENCES S=49:12:59N W=002:08:37E N=51:49:06N E=006:45:51E MIN_POPULATION=12000
cp reference.bin ../../
```

gnav_crunch will also search for the reference.dat and radio.dat files. The entries on the radio.dat file are mapped to the airfields found on the airport.csv file by ICAO codes. This is necessary to locate the radio stations.

## Compiling the terrain data
Terrain data must come in the form of an ESRI grid file. You can use, for example, OpenTopography to obtains high resolution terrain data worldwide.
You need to enter the TERRAIN keyword along with the latitudinal and longitudinal coordinates of the map bounds and the grid size:

```
cd terrain
./../gnav_crunch TERRAIN S=49:12:59N W=002:08:37E N=51:49:06N E=006:45:51E
cp terrain_*.bin ../../
```
