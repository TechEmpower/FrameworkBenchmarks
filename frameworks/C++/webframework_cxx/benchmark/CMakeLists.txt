cmake_minimum_required(VERSION 3.27.0)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(benchmark)

add_executable(
	${PROJECT_NAME}
	src/main.cpp
	src/PlainText.cpp
	src/Json.cpp
	src/Database.cpp
	src/Queries.cpp
	src/Updates.cpp
	src/Fortunes.cpp
	src/CachedQueries.cpp
)

if (UNIX)
	target_link_options(
		${PROJECT_NAME} PRIVATE
		-rdynamic
	)
elseif (WIN32)
	add_definitions(-DNOMINMAX)
endif()

target_include_directories(
	${PROJECT_NAME} PRIVATE
	include
)

list(APPEND CMAKE_MODULE_PATH $ENV{WEB_FRAMEWORK_SDK})

find_package(WebFramework REQUIRED)

target_link_libraries(
	${PROJECT_NAME} PRIVATE
	WebFramework::CXX_API
)

install(TARGETS ${PROJECT_NAME} DESTINATION .)
install(DIRECTORY settings/ DESTINATION .)
install(FILES config.json DESTINATION .)
