from django.conf.urls import url
from world.views import plaintext, json, db, dbs, fortunes, update

urlpatterns = [
    url(r'^plaintext$', plaintext),
    url(r'^json$', json),
    url(r'^db$', db),
    url(r'^dbs$', dbs),
    url(r'^fortunes$', fortunes),
    url(r'^update$', update),
]
