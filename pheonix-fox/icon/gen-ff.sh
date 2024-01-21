#!/bin/bash


# https://1000logos.net/mozilla-firefox-logo/
# https://en.wikipedia.org/wiki/Firefox#Branding_and_visual_identity
#https://web.archive.org/web/20130425030923/https://www.mozilla.org/en-US/styleguide/identity/firefox/branding/

LOGO="orig/firefox_logo-only_RGB_trim.png"
# ^^ Manually cut out some whitespace around edges and made actually square


allSizes="16 22 24 32 48 64 128 256"
ffDirSizes="16 32 48 64 128 "

mkdir -p ff
for size in ${allSizes}
do
    convert -resize ${size}x${size} ${LOGO} ff/default${size}.png
done

for size in ${allSizes}
do
    optipng "ff/default${size}.png"
done
