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

# Option to enable AddressSanitizer (catches segfaults, heap overflows,
# use-after-free, etc.)
option(ENABLE_ASAN "Enable AddressSanitizer" OFF)

# Option to enable UndefinedBehaviorSanitizer
option(ENABLE_UBSAN "Enable UndefinedBehaviorSanitizer" OFF)

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

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  # Assume compiler MSVC.
  add_library(fzf-native-module SHARED fzf-native-module.c fzf.c fzf.h)
else()
  add_library(fzf-native-module MODULE fzf-native-module.c fzf.c fzf.h)
endif()

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

# --- Debug / Sanitizer flags (Linux/GCC/Clang only) ---
if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")

  # Full debug info: -g3 includes macro definitions, -fno-omit-frame-pointer
  # keeps stack frames intact for better backtraces, -O0 disables optimizations
  # so gdb sees exactly what the source says.
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(
      fzf-native-module
      PRIVATE -g3 -O0 -fno-omit-frame-pointer
              -fno-optimize-sibling-calls # improves call-stack accuracy in gdb
              -Wall -Wextra)
    target_link_options(fzf-native-module PRIVATE -g -fno-omit-frame-pointer)
    message(STATUS "Debug build: full symbols, no optimization")
  endif()

  # AddressSanitizer: detects heap/stack overflows, use-after-free,
  # use-after-return, and gives a precise crash report with source locations.
  # Enable with: cmake -DENABLE_ASAN=ON -DCMAKE_BUILD_TYPE=Debug ..
  if(ENABLE_ASAN)
    target_compile_options(fzf-native-module PRIVATE -fsanitize=address
                                                     -fno-omit-frame-pointer -g)
    target_link_options(fzf-native-module PRIVATE -fsanitize=address)
    message(STATUS "AddressSanitizer enabled")
  endif()

  # UndefinedBehaviorSanitizer: catches signed overflow, null dereference,
  # misaligned access, out-of-bounds shifts, etc. Enable with: cmake
  # -DENABLE_UBSAN=ON -DCMAKE_BUILD_TYPE=Debug ..
  if(ENABLE_UBSAN)
    target_compile_options(fzf-native-module PRIVATE -fsanitize=undefined
                                                     -fno-omit-frame-pointer -g)
    target_link_options(fzf-native-module PRIVATE -fsanitize=undefined)
    message(STATUS "UndefinedBehaviorSanitizer enabled")
  endif()

endif()
# --- End debug / sanitizer flags ---

# See
# https://gernotklingler.com/blog/creating-using-shared-libraries-different-compilers-different-operating-systems/
include(GenerateExportHeader)
# generates the export header fzf-native-module_EXPORTS.h automatically.
generate_export_header(fzf-native-module)
