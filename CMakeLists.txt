cmake_minimum_required(VERSION 3.19)
project(fzf-native C)

# Check compilers
MESSAGE( STATUS ">>>>>>>> ${CMAKE_CXX_COMPILER_ID}")

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

# See https://gitlab.kitware.com/cmake/community/-/wikis/doc/tutorials/How-To-Write-Platform-Checks
set(PLATFORM_DIR ${CMAKE_HOST_SYSTEM_NAME})

if(NOT DEFINED FZF_NATIVE_MODULE_OUTPUT_DIR)
  set(FZF_NATIVE_MODULE_OUTPUT_DIR bin/${PLATFORM_DIR})
endif()

# To build shared libraries in Windows, we set CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS to TRUE.
# See https://cmake.org/cmake/help/v3.4/variable/CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS.html
# See https://blog.kitware.com/create-dlls-on-windows-without-declspec-using-new-cmake-export-all-feature/
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  # Assume compiler MSVC.
  add_library(fzf-native-module SHARED fzf-native-module.c fzf.c fzf.h)
else()
  add_library(fzf-native-module MODULE fzf-native-module.c fzf.c fzf.h)
endif()

set_target_properties(fzf-native-module PROPERTIES
  C_STANDARD 99
  POSITION_INDEPENDENT_CODE ON
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/${FZF_NATIVE_MODULE_OUTPUT_DIR})
target_include_directories(fzf-native-module PUBLIC ${EMACS_INCLUDE_DIR})

# Set output directory for the library.
# See https://github.com/RoukaVici/LibRoukaVici
set(OUTPUT_DIR ${CMAKE_SOURCE_DIR}/${FZF_NATIVE_MODULE_OUTPUT_DIR})
set_target_properties(fzf-native-module
PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${OUTPUT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${OUTPUT_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${OUTPUT_DIR}
)

# See https://gernotklingler.com/blog/creating-using-shared-libraries-different-compilers-different-operating-systems/
include(GenerateExportHeader)
# generates the export header fzf-native-module_EXPORTS.h automatically.
generate_export_header(fzf-native-module)
