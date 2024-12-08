cmake_minimum_required(VERSION 3.19)
project(fzf-native C)

# Check compilers
message(STATUS ">>>>>>>> ${CMAKE_CXX_COMPILER_ID}")
message(STATUS ">>>>>>>> ${CMAKE_HOST_SYSTEM_PROCESSOR}")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(
    STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Choose the type of build." FORCE)
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

# See
# https://gitlab.kitware.com/cmake/community/-/wikis/doc/tutorials/How-To-Write-Platform-Checks
set(PLATFORM_DIR ${CMAKE_HOST_SYSTEM_NAME})

if(NOT DEFINED FZF_NATIVE_MODULE_OUTPUT_DIR)
  if((CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin") AND (CMAKE_HOST_SYSTEM_PROCESSOR
                                                     STREQUAL "arm64"))
    # Assume Apple Silicon.
    set(FZF_NATIVE_MODULE_OUTPUT_DIR bin/${PLATFORM_DIR}/arm64)
  else()
    set(FZF_NATIVE_MODULE_OUTPUT_DIR bin/${PLATFORM_DIR})
  endif()
endif()

# To build shared libraries in Windows, we set CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS
# to TRUE. See
# https://cmake.org/cmake/help/v3.4/variable/CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS.html
# See
# https://blog.kitware.com/create-dlls-on-windows-without-declspec-using-new-cmake-export-all-feature/
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)

# Enable experimental C11 atomics for MSVC
# https://developercommunity.visualstudio.com/t/C11-atomic-types-are-broken-in-CL/10560673?sort=newest
# Needed for #include <stdatomic.h>
if(MSVC)
  # Add /experimental:c11atomics for MSVC (C11 atomic operations)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /experimental:c11atomics")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /experimental:c11atomics")

  # Ensure the compiler uses C11 standard (MSVC may default to an older
  # standard)
  add_compile_options(/std:c11)

  # Optionally, ensure that C11 is used for C and C++ separately
  set(CMAKE_C_STANDARD 11)
  set(CMAKE_CXX_STANDARD 11)
endif()

# Add pthreads-w32 on Windows
if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  # Path to the pthreads-w32 directory
  set(PTHREADS_W32_DIR
      "${CMAKE_SOURCE_DIR}/pthreads-w32-2-9-1-release/Pre-built.2")

  # Determine architecture
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(ARCH_DIR "x64")
  else()
    set(ARCH_DIR "x86")
  endif()

  # Assume compiler MSVC. Link the appropriate library Use pthreadVC2.lib for
  # Visual Studio, pthreadGC2.lib for MinGW Conditionally select the correct
  # library based on the compiler
  if(MSVC)
    message(STATUS "Setting ${PTHREADS_W32_DIR}/lib/${ARCH_DIR}/pthreadVC2.lib")
    set(PTHREADS_W32_LIB "${PTHREADS_W32_DIR}/lib/${ARCH_DIR}/pthreadVC2.lib")
  elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    message(
      STATUS "Setting ${PTHREADS_W32_DIR}/lib/${ARCH_DIR}/libpthreadGC2.a")
    set(PTHREADS_W32_LIB "${PTHREADS_W32_DIR}/lib/${ARCH_DIR}/libpthreadGC2.a")
  else()
    message(FATAL_ERROR "Unsupported compiler: ${CMAKE_CXX_COMPILER_ID}")
  endif()

  add_library(fzf-native-module SHARED fzf-native-module.c fzf.c fzf.h)
  # Include the pthreads-w32 directory for Windows
  target_include_directories(fzf-native-module
                             PRIVATE ${PTHREADS_W32_DIR}/include)
  # Link pthreads to fzf-native-module
  target_link_libraries(fzf-native-module PRIVATE ${PTHREADS_W32_LIB})
else()
  add_library(fzf-native-module MODULE fzf-native-module.c fzf.c fzf.h)
endif()

# Set library output directory
set(OUTPUT_DIR ${CMAKE_SOURCE_DIR}/${FZF_NATIVE_MODULE_OUTPUT_DIR})
set_target_properties(
  fzf-native-module
  PROPERTIES C_STANDARD 11
             POSITION_INDEPENDENT_CODE ON
             PREFIX ""
             LIBRARY_OUTPUT_DIRECTORY ${OUTPUT_DIR})
target_include_directories(fzf-native-module PUBLIC ${EMACS_INCLUDE_DIR})

# Set output directory for the library. See
# https://github.com/RoukaVici/LibRoukaVici
set_target_properties(
  fzf-native-module
  PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${OUTPUT_DIR}
             LIBRARY_OUTPUT_DIRECTORY ${OUTPUT_DIR}
             RUNTIME_OUTPUT_DIRECTORY ${OUTPUT_DIR})

# See
# https://gernotklingler.com/blog/creating-using-shared-libraries-different-compilers-different-operating-systems/
include(GenerateExportHeader)
# generates the export header fzf-native-module_EXPORTS.h automatically.
generate_export_header(fzf-native-module)
