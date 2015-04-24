
export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 apache

cd $TROOT/webware rm -fr Webware
wget downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz -O Webware.tar.gz
tar -xf Webware.tar.gz
cp -r app/ Webware/
