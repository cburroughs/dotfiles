#!/bin/bash


# https://1000logos.net/mozilla-firefox-logo/
# https://en.wikipedia.org/wiki/Firefox#Branding_and_visual_identity
#https://web.archive.org/web/20130425030923/https://www.mozilla.org/en-US/styleguide/identity/firefox/branding/

LOGO="firefox_logo-only_RGB_trim.png"
# ^^ Manually cut out some whitespace around edges and made actually square


allSizes="16 22 24 32 48 64 128 256"
ffDirSizes="16 32 48 64 128 "

for size in ${allSizes}
do
    convert -resize ${size}x${size} ${LOGO} default${size}.png
done

for size in ${allSizes}
do
    optipng "default${size}.png"
done


echo " "
echo "copy to FF dir comands! (root)"
echo " "
for size in ${ffDirSizes}
do
    echo "cp default${size}.png /usr/lib64/firefox/browser/chrome/icons/default/default${size}.png"
done


# /usr/share/icons/hicolor/ magical default theme
echo " "
echo "install icon commands (root)"
echo " "
for size in ${allSizes}
do
    echo "xdg-icon-resource install --novendor --size ${size} default${size}.png firefox"
done
echo "cp default48.png /usr/share/pixmaps/firefox.png"



# hint
# equery files www-client/firefox
