# install virtualenv
easy_install virtualenv
python virtualenv.py w2env
# install missing modules
w2env/bin/easy_install -U pysqlite hashlib
# donwload web2py and unpack
wget http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip
cd web2py
# start web2py using command-line script
w2env/bin/python web2py.py -i 0.0.0.0 -p 8123 -a 'adminpasswd'
