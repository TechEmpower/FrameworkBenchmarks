cmake_minimum_required(VERSION 3.27.0)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(benchmark)

add_executable(
	${PROJECT_NAME}
	src/main.c
	src/PlainText.c
	src/Json.c
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
	WebFramework::CC_API
)

install(TARGETS ${PROJECT_NAME} DESTINATION .)
install(DIRECTORY settings/ DESTINATION .)
install(FILES config.json DESTINATION .)
