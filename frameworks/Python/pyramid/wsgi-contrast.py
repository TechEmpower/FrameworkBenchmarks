from paste.deploy import loadapp

app = loadapp('config:contrast.ini', relative_to='.')
