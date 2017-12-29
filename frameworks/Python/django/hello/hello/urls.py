from django.conf.urls import url

from world.views import plaintext, json, db, dbs, fortunes, update

# Uncomment the next two lines to enable the admin:
# from django.contrib import admin
# admin.autodiscover()

urlpatterns = [
    # Examples:
    # url(r'^$', 'hello.views.home', name='home'),
    # url(r'^hello/', include('hello.foo.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # url(r'^admin/', include(admin.site.urls)),
    url(r'^plaintext$', plaintext),
    url(r'^json$', json),
    url(r'^db$', db),
    url(r'^dbs$', dbs),
    url(r'^fortunes$', fortunes),
    url(r'^update$', update),
]
