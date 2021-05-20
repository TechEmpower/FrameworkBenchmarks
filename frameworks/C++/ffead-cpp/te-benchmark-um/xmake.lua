add_includedirs("include/")

local bindir = "$(projectdir)/ffead-cpp-6.0-bin"

target("te-benchmark-um")
	set_languages("c++17")
	add_deps("ffead-framework")
	add_options(getOptions())
	set_kind("shared")
	on_load(setIncludes)
	add_files("src/*.cpp")
	set_installdir(bindir)
