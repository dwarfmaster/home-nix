cmake_minimum_required(VERSION {{cookiecutter.cmake_minimal_version}})

project(test_{{cookiecutter.project_name}})

find_package(GTest REQUIRED)

add_executable(${PROJECT_NAME})

target_sources(${PROJECT_NAME} 
    PRIVATE
        src/test_{{cookiecutter.project_name}}.cpp
)

set_target_properties(${PROJECT_NAME} PROPERTIES
    CXX_STANDARD {{cookiecutter.cpp_standard}}
    CXX_EXTENSIONS OFF
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
)

target_include_directories(${PROJECT_NAME}
    PUBLIC
        ${GTest_INCLUDE_DIR}
        $<INSTALL_INTERFACE:inc>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/inc>
)

target_link_directories(${PROJECT_NAME}
    PUBLIC
        ${GTest_LIB_DIRS}
)

target_link_libraries(${PROJECT_NAME}
    PUBLIC
{%- if cookiecutter.binary_type != "exe" %}
        {{cookiecutter.project_name}}
{%- endif %}
        ${GTest_LIBRARY_LIST}
        $<$<NOT:$<BOOL:${WIN32}>>:pthread>
)

include(GoogleTest)
gtest_discover_tests(${PROJECT_NAME})
