#!/bin/bash


# Thunderbird from https://archive.mozilla.org/pub/thunderbird/releases/31.0/linux-x86_64/en-US/

ffSizes="16 22 24 32 48 64 128 256"
tbSizes="16 22 24 32 48 256"

echo " "
echo "copy to dir comands! (root)"
echo " "

for size in ${ffSizes}
do
    echo "cp ${PWD}/ff/default${size}.png /usr/lib64/firefox/browser/chrome/icons/default/default${size}.png"
done

# /usr/share/icons/hicolor/ magical default theme?
for size in ${ffSizes}
do
    echo "xdg-icon-resource install --novendor --size ${size} ${PWD}/ff/default${size}.png firefox"
done
echo "cp ${PWD}/ff/default48.png /usr/share/pixmaps/firefox.png"


for size in ${tbSizes}
do
    echo "cp ${PWD}/tb/default${size}.png /usr/lib64/thunderbird/chrome/icons/default/default${size}.png"
done

# /usr/share/icons/hicolor/ magical default theme?
for size in ${tbSizes}
do
    echo "xdg-icon-resource install --novendor --size ${size} ${PWD}/tb/default${size}.png thunderbird"
done
echo "cp ${PWD}/tb/default48.png /usr/share/pixmaps/thunderbird.png"



# hint
# equery files www-client/firefox
