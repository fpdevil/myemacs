# Irony Mode on OS X

>Download the pre-compiled package of clang+llvm-3.5.0-macosx-apple-darwin.tar.xz from  http://llvm.org/releases/download.html

```sh
http -download http://llvm.org/releases/3.9.0/clang+llvm-3.9.0-x86_64-apple-darwin.tar.xz
tar -xvf wnload
ll /tmp/clang+llvm-3.9.0-x86_64-apple-darwin
```

> run cmake in the irony package

```sh
 13:24:24  ...packages/elpa/irony-20161106.830   master ✘ ✖ ✹ ✭ 
$ mkdir build

 13:24:40  ...packages/elpa/irony-20161106.830   master ✘ ✖ ✹ ✭ 
$ cd build

 13:24:43  ...elpa/irony-20161106.830/build   master ✘ ✖ ✹ ✭ 
cmake -DCMAKE_PREFIX_PATH=/tmp/clang+llvm-3.9.0-x86_64-apple-darwin/ -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DLIBCLANG_LIBRARY=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib -DCMAKE_INSTALL_PREFIX=/Users/sampathsingamsetty/.emacs.d/irony/ /Users/sampathsingamsetty/.emacs.d/packages/elpa/irony-20161106.830/server

 13:24:43  ...elpa/irony-20161106.830/build   master ✘ ✖ ✹ ✭ 
$ cmake -DCMAKE_PREFIX_PATH=/tmp/clang+llvm-3.9.0-x86_64-apple-darwin/ -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DLIBCLANG_LIBRARY=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib -DCMAKE_INSTALL_PREFIX=/Users/sampathsingamsetty/.emacs.d/irony/ /Users/sampathsingamsetty/.emacs.d/packages/elpa/irony-20161106.830/server
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

 13:24:50  ...elpa/irony-20161106.830/build   master ✘ ✖ ✹ ✭ 
$ ll
total 104
-rw-r--r--   1 sampathsingamsetty  staff    18K Dec 14 13:24 CMakeCache.txt
drwxr-xr-x  43 sampathsingamsetty  staff   1.4K Dec 14 13:24 CMakeFiles
-rw-r--r--   1 sampathsingamsetty  staff   407B Dec 14 13:24 CTestTestfile.cmake
-rw-r--r--   1 sampathsingamsetty  staff   2.9K Dec 14 13:24 DartConfiguration.tcl
-rw-r--r--   1 sampathsingamsetty  staff    20K Dec 14 13:24 Makefile
drwxr-xr-x   3 sampathsingamsetty  staff   102B Dec 14 13:24 Testing
drwxr-xr-x   2 sampathsingamsetty  staff    68B Dec 14 13:24 bin
-rw-r--r--   1 sampathsingamsetty  staff   1.7K Dec 14 13:24 cmake_install.cmake
drwxr-xr-x   6 sampathsingamsetty  staff   204B Dec 14 13:24 docs
drwxr-xr-x   7 sampathsingamsetty  staff   238B Dec 14 13:24 src
drwxr-xr-x   7 sampathsingamsetty  staff   238B Dec 14 13:24 test

 13:24:52  ...elpa/irony-20161106.830/build   master ✘ ✖ ✹ ✭ 
$ ll src
total 112
drwxr-xr-x  5 sampathsingamsetty  staff   170B Dec 14 13:24 CMakeFiles
-rw-r--r--  1 sampathsingamsetty  staff   368B Dec 14 13:24 CTestTestfile.cmake
-rw-r--r--  1 sampathsingamsetty  staff    13K Dec 14 13:24 Makefile
-rw-r--r--  1 sampathsingamsetty  staff   2.0K Dec 14 13:24 cmake_install.cmake
-rw-r--r--  1 sampathsingamsetty  staff    29K Dec 14 13:24 irony.el

 13:25:02  ...elpa/irony-20161106.830/build   master ✘ ✖ ✹ ✭ 
$ cmake --build . --use-stderr --config Release --target install
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
-- Installing: /Users/sampathsingamsetty/.emacs.d/irony/bin/irony-server
```

```bash
 16:55:00  ~/.emacs.d   master ✘ ✖ ✹ ✭ 
$ ./irony/bin/irony-server -v
irony-server version 0.2.2-cvs
Apple LLVM version 8.0.0 (clang-800.0.42.1)
```