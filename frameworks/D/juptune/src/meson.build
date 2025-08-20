#### Project configuration ####

project('juptune-tfb', 'd')

#### Sources ####

srcs = files(
    './main.d',
    './tests/common.d',
    './tests/json.d',
    './tests/plaintext.d',
)

#### Dependencies ####

juptune_dep = dependency('juptune')

dep = declare_dependency(
    include_directories: include_directories('.'),
    sources: srcs,
    dependencies: [juptune_dep],
)

#### Executables ####

main_exe = executable('juptune-tfb', dependencies: [dep])