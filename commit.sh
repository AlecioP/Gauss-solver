#!/bin/sh

if [ "$#" -ne 1 ]; then
    echo "Illegal number of parameters"
fi
/Applications/Mathematica.app/Contents/MacOS/wolframscript Export.wls
git add *.wl 
git add *.m 
git add Gauss.nb 
git add Gauss.txt 
git add Export.wls 
git add README.md
git commit -m"$1"
git push origin master
exit
