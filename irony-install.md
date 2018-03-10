# Irony Mode on OS X

## First download the pre-compiled package of clang+llvm-6.0.0-x86_64-apple-darwin.tar.xz from http://llvm.org/releases/download.html specifically meant for Mac OSX

```bash
http --download --verify=no http://releases.llvm.org/6.0.0/clang+llvm-6.0.0-x86_64-apple-darwin.tar.xz
tar -xvf clang+llvm-6.0.0-x86_64-apple-darwin.tar.xz
ll clang+llvm-6.0.0-x86_64-apple-darwin/
```

## Now prepare the environment for running cmake inside the irony package to generate the necessary build

> Create a directory `build` inside the `irony` folder and go into that.

```bash
 apple  ⋯  packages  elpa  irony-20180308.1256  mkdir build                                         master
 apple  ⋯  packages  elpa  irony-20180308.1256  cd build                                            master
 apple  ⋯  elpa  irony-20180308.1256  build 
```

## Run the `CMAKE` for build, using the below command

```bash
$cmake \
-DCMAKE_PREFIX_PATH=/opt/software/clang+llvm-6.0.0-x86_64-apple-darwin/ \
-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON \
-DLIBCLANG_LIBRARY=/opt/software/clang+llvm-6.0.0-x86_64-apple-darwin/lib/libclang.dylib \
-DCMAKE_INSTALL_PREFIX=~/.emacs.d/irony/ \
~/.emacs.d/packages/elpa/irony-20180308.1256/server
```

```bash
apple  ...elpa/irony-20161106.830/build   master ✘ ✖ ✹ ✭ 
$ cmake -DCMAKE_PREFIX_PATH=/tmp/clang+llvm-3.9.0-x86_64-apple-darwin/
-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON
-DLIBCLANG_LIBRARY=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib
-DCMAKE_INSTALL_PREFIX=/Users/sampathsingamsetty/.emacs.d/irony/
/Users/sampathsingamsetty/.emacs.d/packages/elpa/irony-20161106.830/server

-- The C compiler identification is AppleClang 8.0.0.8000042
-- The CXX compiler identification is AppleClang 8.0.0.8000042
-- Check for working C compiler: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc
-- Check for working C compiler: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc -- works
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Detecting C compile features
-- Detecting C compile features - done
-- Check for working CXX compiler: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++
-- Check for working CXX compiler: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ -- works
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Setting build type to 'Release' as none was specified
-- Performing Test HAS_STDCXX11
-- Performing Test HAS_STDCXX11 - Success
-- Performing Test HAS_CXX11_STDLIB
-- Performing Test HAS_CXX11_STDLIB - Success
-- C++11 compiler option(s): -std=c++11
-- Found LibClang: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib
-- Detecting libclang builtin headers directory
-- Detecting libclang builtin headers directory -- success
-- Irony package version is '0.2.2-cvs'
-- Found emacs: /Applications/emacs.app/Contents/MacOS/Emacs
-- Configuring done
-- Generating done
-- Build files have been written to: /Users/sampathsingamsetty/.emacs.d/packages/elpa/irony-20161106.830/build

```

- below files are generated after the `cmake` run

```bash
 apple  ⋯  elpa  irony-20180308.1256  build  ls -trl                                                master
total 112
drwxr-xr-x   3 sampathsingamsetty  staff     96 Mar 10 13:05 Testing
-rw-r--r--   1 sampathsingamsetty  staff   2970 Mar 10 13:05 DartConfiguration.tcl
-rw-r--r--   1 sampathsingamsetty  staff  18506 Mar 10 13:05 CMakeCache.txt
-rw-r--r--   1 sampathsingamsetty  staff  20734 Mar 10 13:05 Makefile
-rw-r--r--   1 sampathsingamsetty  staff   1936 Mar 10 13:05 cmake_install.cmake
-rw-r--r--   1 sampathsingamsetty  staff    411 Mar 10 13:05 CTestTestfile.cmake
drwxr-xr-x   2 sampathsingamsetty  staff     64 Mar 10 13:05 bin
drwxr-xr-x   7 sampathsingamsetty  staff    224 Mar 10 13:05 src
drwxr-xr-x   6 sampathsingamsetty  staff    192 Mar 10 13:05 docs
drwxr-xr-x   7 sampathsingamsetty  staff    224 Mar 10 13:05 test
drwxr-xr-x  43 sampathsingamsetty  staff   1376 Mar 10 13:05 CMakeFiles

 apple  ⋯  elpa  irony-20180308.1256  build  ls -trl src                                            master
total 112
-rw-r--r--  1 sampathsingamsetty  staff  32555 Mar 10 13:05 irony.el
-rw-r--r--  1 sampathsingamsetty  staff  14291 Mar 10 13:05 Makefile
-rw-r--r--  1 sampathsingamsetty  staff   2114 Mar 10 13:05 cmake_install.cmake
-rw-r--r--  1 sampathsingamsetty  staff    372 Mar 10 13:05 CTestTestfile.cmake
drwxr-xr-x  5 sampathsingamsetty  staff    160 Mar 10 13:05 CMakeFiles
```

## Now build the `irony` server with the below command

>```bash
> $cmake --build . --use-stderr --config Release --target install
>```

```bash
 apple  ⋯  elpa  irony-20180308.1256  build  cmake --build . --use-stderr --config Release --target install
Scanning dependencies of target irony-server
[ 14%] Building CXX object src/CMakeFiles/irony-server.dir/support/CommandLineParser.cpp.o
[ 28%] Building CXX object src/CMakeFiles/irony-server.dir/support/TemporaryFile.cpp.o
[ 42%] Building CXX object src/CMakeFiles/irony-server.dir/Command.cpp.o
[ 57%] Building CXX object src/CMakeFiles/irony-server.dir/Irony.cpp.o
[ 71%] Building CXX object src/CMakeFiles/irony-server.dir/TUManager.cpp.o
[ 85%] Building CXX object src/CMakeFiles/irony-server.dir/main.cpp.o
[100%] Linking CXX executable ../bin/irony-server
[100%] Built target irony-server
Install the project...
-- Install configuration: "Release"
-- Installing: ~/.emacs.d/irony/bin/irony-server
```

## Check the irony-server version

```bash
./bin/irony-server -v
irony-server version 1.2.0
clang version 6.0.0 (tags/RELEASE_600/final)
```
