from django.urls import re_path
from ..world.views import plaintext, json

urlpatterns = [
    re_path(r'^plaintext$', plaintext),
    re_path(r'^json$', json)
]
