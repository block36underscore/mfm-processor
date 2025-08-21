#!/bin/bash
shadow-cljs compile app shiki
echo "
----- Compiling Done -----
"
node target/mfmc.js "$@"
