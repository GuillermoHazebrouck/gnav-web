echo "rebuilding gnav data compiler"
#gnatmake "src/main.adb" -D obj
gprbuild data_compiler.gpr
mv obj/main ../gnav_crunch
