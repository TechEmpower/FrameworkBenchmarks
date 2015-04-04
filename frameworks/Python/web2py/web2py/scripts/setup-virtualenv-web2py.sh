#
# Author:                        Christopher Steel
# Organization:        Voice of Access
# Date:                                2010-11-24
# License:                        Same as Web2py, MIT / GNU
# Email:                        Christopher DOT Steel AT Voice of Access DOT org
#
# This script will :
#        download and install virtualenv
#        start a virtual environment
#        move into the virtual environment
#        download and install latest stable version of web2py
#        start web2py in the virtual environment
#
# To disactivate the virtual environment, shut down web2py
# and type 'disactivate' at the command line.
#
# Testing:
#        OS X
#        should work on POSIX systems
#
# Usage:
#        create a directory to hold your virtual environments, for example
#                /home/user_name/virtual_environments
#        place this script in the directory and make it executable
#                chmod +x web2py-install-virtualenv.sh
        customize the variables below to meet your needs
#        execute from terminal
#                ./web2py-install-virtualenv.sh
#        relax...

################ VARIABLES
# Change to reflect version changes etc.
#
# name for your virtual environment
ENV=VIRTUAL_ENV
# version to install
APP_NAME=virtualenv
VER=1.5.1
DIR=${APP_NAME}-${VER}
EXT=tar.gz
ARCHIVE=${APP_NAME}-${VER}.${EXT}
# md5 sum, see end of url from pypi
MD5_SUM=3daa1f449d5d2ee03099484cecb1c2b7
################
#
echo 'downloading' ${ARCHIVE}
echo '================================'
echo `wget http://pypi.python.org/packages/source/v/virtualenv/${ARCHIVE}`
md5 ${ARCHIVE}
echo 'MD5 ('${ARCHIVE}') =' ${MD5_SUM}
echo 'unarchive' ${ARCHIVE}
echo '================================='
tar xvfz ${ARCHIVE}

echo ' comparing md5 sums'
echo '================================='
md5 ${ARCHIVE}
echo 'MD5 ('${ARCHIVE}') =' ${MD5_SUM}

#echo 'installing compatibility modules'
#echo '================================'
#virtualenv/bin/easy_install -U pysqlite hashlib

#echo 'Installing distribute'
#echo '====================='
#echo 'Creating Environment'
#echo '====================='
#echo `python ./${DIRAPP_NAME}-${VER}/virtualenv.py --distribute ${ENV}`

echo 'Start virtual environment'
echo '========================='
virtualenv --no-site-packages ${ENV}

RUN_THIS='source ${ENV}/bin/activate'
`echo source ${ENV}/bin/activate`

echo 'Moving into virtual environment directory'
echo '========================================='
cd ${ENV}

echo 'downloading web2py'
echo '=================='
wget http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip
cd web2py

echo 'to deactivate your virtual environment'
echo 'shutdown web2py and then type "deactivate"'
echo '=========================================='
read -p "Press any key to start web2py…"


echo 'starting web2py'
echo '==============='
../bin/python2.5  web2py.py
