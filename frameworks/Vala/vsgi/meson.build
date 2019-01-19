project('VSGI TechEmpower Benchmark', 'c', 'vala')

glib_dep = dependency('glib-2.0')
gobject_dep = dependency('gobject-2.0')
gio_dep = dependency('glib-2.0')
soup_dep = dependency('libsoup-2.4')
vsgi_dep = dependency('vsgi-0.3')
json_glib_dep = dependency('json-glib-1.0')

executable('app', 'app.vala',
           dependencies: [glib_dep, gobject_dep, gio_dep, soup_dep, vsgi_dep, json_glib_dep])
