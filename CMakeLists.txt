cmake_minimum_required(VERSION 3.19)
project(fzf-native C)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE
	STRING "Choose the type of build." FORCE)
endif()

find_program(EMACS_PROGRAM emacs)
if(EMACS_PROGRAM)
  get_filename_component(EMACS_PROGRAM ${EMACS_PROGRAM} REALPATH)
  get_filename_component(EMACS_PROGRAM_DIR ${EMACS_PROGRAM} DIRECTORY)
  get_filename_component(EMACS_PROGRAM_DIR ${EMACS_PROGRAM_DIR} DIRECTORY)
endif()

if(NOT DEFINED EMACS_INCLUDE_DIR)
  set(EMACS_INCLUDE_DIR "")
endif()

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

set(PLATFORM_DIR ${CMAKE_SYSTEM_NAME})

if(NOT DEFINED FZF_NATIVE_MODULE_OUTPUT_DIR)
  set(FZF_NATIVE_MODULE_OUTPUT_DIR bin/${PLATFORM_DIR})
endif()

add_library(fzf-native-module MODULE fzf-native-module.c fzf.c fzf.h)
set_target_properties(fzf-native-module PROPERTIES
  C_STANDARD 99
  POSITION_INDEPENDENT_CODE ON
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/${FZF_NATIVE_MODULE_OUTPUT_DIR})
target_include_directories(fzf-native-module PRIVATE ${EMACS_INCLUDE_DIR})
target_link_libraries(fzf-native-module PRIVATE Threads::Threads)
