hello
=====

Install
-------

#. Follow pythonz install doc
#. pythonz install 3.4.2
#. cd /opt
#. Git clone your app here
#. cd /opt/hello/
#. /usr/local/pythonz/pythons/CPython-3.4.2/bin/pyvenv pyvenv
#. . pyvenv/bin/activate
#. pip install -r requirements.txt
#. cd /etc/init.d/ && ln -s /opt/hello/etc/init.d/hello
#. cd /etc/default/ && ln -s /opt/hello/etc/default/hello
#. cd /etc/monit/conf.d/ && ln -s /opt/hello/etc/monit/conf.d/hello
#. update-rc.d hello defaults
#. cp -a /opt/hello/etc/hello /etc/
#. Adapt rsyslog and lograte
#. service hello start
#. service monit restart
