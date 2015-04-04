read -p "Please choose a Web2Py Administrator password:" passwd
echo "web: python web2py.py -a '$passwd' -i 0.0.0.0 -p \$PORT" > Procfile
mkdir vendor
pip install --download vendor -r requirements.txt
read -p "Please choose a CF application name:" appname
cf push $appname -b https://github.com/cloudfoundry/buildpack-python.git
