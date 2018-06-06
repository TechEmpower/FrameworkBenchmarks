
FROM ravendb/ravendb:ubuntu-latest 

ENV RAVEN_ARGS='' RAVEN_SETTINGS='{}' RAVEN_Setup_Mode='None' RAVEN_Logs_Mode='None' RAVEN_ServerUrl='http://0.0.0.0:8080' RAVEN_Security_UnsecuredAccessAllowed='PrivateNetwork' RAVEN_DataDir='RavenData' RAVEN_ServerUrl_Tcp='38888' RAVEN_AUTO_INSTALL_CA='true' RAVEN_IN_DOCKER='true' RAVEN_License_Eula_Accepted='true' RAVEN_License='{"Id": "baf49237-9991-44c3-ab0b-f05071db57f0","Name": "TechEmpower","Keys": ["tPsfEbpY71VT3mNJTMKDQ0SgZ","9VphQ7IVmdBshLNRwrmyUhpfA","oD+x83TYNuu0ffrptiBVxb4Pk","0wwjtLN45Lt3gomRE1oSrBtKu","IA6KJAHB0ZXOx33frYdgwNrgr","ibUhf5w5pXR6VnZmdjUXukw4w","XWGzvXkDg7oRbYmn3ZjGpABUE","oBSYoSQMqKywtLi8wJzFDJEQJ","Yn5N"]}'

COPY TechEmpower-World.ravendbdump TechEmpower-Fortunes.ravendbdump /datadumps/
COPY settings.json /opt/RavenDB/Server/
COPY run-raven.sh /opt/RavenDB/run-raven.sh

CMD /opt/RavenDB/run-raven.sh