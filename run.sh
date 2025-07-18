#!/bin/bash
shadow-cljs compile app
echo "
----- Compiling Done -----
"
node target/mfmc.js "$@"
