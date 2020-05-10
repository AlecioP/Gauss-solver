#!/bin/sh

if [ "$#" -ne 1 ]; then
    echo "Illegal number of parameters"
fi
/Applications/Mathematica.app/Contents/MacOS/wolframscript Export.wls
git add *.wl *.m Gauss.nb Gauss.txt Export.wls README.md
git commit -m"$1"
git push origin master
exit
