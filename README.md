[![Build Status](https://travis-ci.org/thabz/RayGay.svg?branch=master)](https://travis-ci.org/thabz/RayGay)

# Introduction

An opensource raytracer written in C++ with an embedded Scheme parser for scene files.

The sourcecode is released under the GPL. See the file COPYING.

# Get hacking

On a Mac you need the following to build.

`brew install autoconf automake libtool`

The C++ source code is in `src`. Compile with

    ./autogen
    ./configure
    make
    sudo make install

The unit-tests are in `test`. Run them with

    make check

The `scenes` folder contains examples scenes. Render a scene with:

    raygay scenes/csg.scm out.png

Documentation written in Docbook is in `docs`. Build the HTML documentation with

    cd docs
    make chunked-portable

Reformat code using clang-format

    brew install clang-format
    clang-format -i --style=LLVM src/**/*.cpp src/**/*.h
