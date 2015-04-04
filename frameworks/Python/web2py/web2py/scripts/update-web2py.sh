#!/bin/bash
#
#  update-web2py.sh
#  2009-12-16
#
#  install in web2py/.. or web2py/ or web2py/scripts as update-web2py.sh
#  make executable: chmod +x web2py.sh
#
#  save a snapshot of current web2py/ as web2py/../web2py-version.zip
#  download the current stable version of web2py
#  unzip downloaded version over web2py/
#
TARGET=web2py

if [ ! -d $TARGET ]; then
        # in case we're in web2py/
        if [ -f ../$TARGET/VERSION ]; then
                cd ..
        # in case we're in web2py/scripts
        elif [ -f ../../$TARGET/VERSION ]; then
                cd ../..
        fi
fi
read a VERSION c < $TARGET/VERSION
SAVE=$TARGET-$VERSION
URL=http://www.web2py.com/examples/static/web2py_src.zip

ZIP=`basename $URL`
SAVED=""

#  Save a zip archive of the current version,
#  but don't overwrite a previous save of the same version.
#
if [ -f $SAVE.zip ]; then
        echo "Remove or rename $SAVE.zip first" >&2
        exit 1
fi
if [ -d $TARGET ]; then
        echo -n ">>Save old version: " >&2
        cat $TARGET/VERSION >&2
        zip -q -r $SAVE.zip $TARGET
        SAVED=$SAVE.zip
fi
#
#  Download the new version.
#
echo ">>Download latest web2py release:" >&2
curl -O $URL
#
#  Unzip into web2py/
#
unzip -q -o $ZIP
rm $ZIP
echo -n ">>New version: " >&2
cat $TARGET/VERSION >&2
if [ "$SAVED" != "" ]; then
        echo ">>Old version saved as $SAVED"
fi
