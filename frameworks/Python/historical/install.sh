
export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip

mkdir -p $IROOT/.pip_cache
export PIP_DOWNLOAD_CACHE=$IROOT/.pip_cache

fw_depends python2 apache

$PY2_PIP install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/webware/requirements.txt

cd $TROOT/webware 
rm -fr Webware Webware-1.1.1
rm Webware-1.1.1.tar.gz
wget downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz
tar -xf Webware-1.1.1.tar.gz
cp -r app/ Webware-1.1.1/
