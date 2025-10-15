from django.urls import path
from world.views import plaintext, json, db, dbs, fortunes, update

urlpatterns = [
    path("plaintext", plaintext),
    path("json", json),
    path("db", db),
    path("dbs", dbs),
    path("fortunes", fortunes),
    path("update", update),
]
