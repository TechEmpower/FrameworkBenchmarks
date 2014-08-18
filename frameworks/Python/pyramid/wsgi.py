from paste.deploy import loadapp

app = loadapp('config:production.ini', relative_to='.')
