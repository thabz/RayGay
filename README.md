[![Tests](https://github.com/thabz/RayGay/actions/workflows/tests.yml/badge.svg)](https://github.com/thabz/RayGay/actions/workflows/tests.yml)

# Introduction

An opensource raytracer written in C++ with an embedded Scheme parser for scene files.

The sourcecode is released under the GPL. See the file COPYING.

# Get hacking

RayGay builds with CMake. The recommended generator is Ninja.

On macOS with Homebrew:

    brew install cmake ninja pkg-config

On Ubuntu/Debian:

    sudo apt-get install cmake ninja-build g++ libjpeg-dev libpng-dev pkg-config

The C++ source code is in `src`. Compile with:

    cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
    cmake --build build

The unit-tests are in `test`. Run them with

    ctest --test-dir build --output-on-failure

The `scenes` folder contains examples scenes. Render a scene with:

    ./build/raygay scenes/csg.scm out.png

Documentation written in DocBook is in `docs`. Build the HTML documentation with

    cd docs
    xsltproc --stringparam html.stylesheet reference.css http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl reference.docbook

Reformat code using clang-format

    brew install clang-format
    clang-format -i --style=LLVM src/**/*.cpp src/**/*.h

See `INSTALL` for optional CMake flags, MPI builds, and more detailed dependency notes.
