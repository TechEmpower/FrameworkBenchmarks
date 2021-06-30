
module_includes = ['/usr/local/include','../../src/modules/common','../../src/modules/cache','../../src/modules/cache/memory','../../src/modules/cache/redis',
	'../../src/modules/cache/memcached','../../src/modules/client-util','../../src/modules/http','../../src/modules/http/http11','../../src/modules/http/http20',
	'../../src/modules/ioc','../../src/modules/jobs','../../src/modules/reflection','../../src/modules/sdorm','../../src/modules/sdorm/sql','../../src/modules/sdorm/sql/libpq',
	'../../src/modules/client-util/ssl','../../src/modules/sdorm/mongo','../../src/modules/sdorm/mongo/raw','../../src/modules/search','../../src/modules/serialization',
	'../../src/modules/serialization/xml','../../src/modules/serialization/json','../../src/modules/server-util','../../src/modules/ssl','../../src/modules/threads',
	'../../src/framework','include']
module_includes += global_includes
module_libs = [global_libs]

c = run_command(meson_grabber, 'src/')
module_sources = c.stdout().strip().split('\n')

shared_library('te-benchmark-um-pq', module_sources, include_directories: module_includes, dependencies: module_libs, link_with: [libffeadmodules, libffeadframework],
	install: true, install_dir: bin_dir+'/lib')