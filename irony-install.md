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
(~/.emacs.d/elpa/irony-20180519.422)$ mkdir
(~/.emacs.d/elpa/irony-20180519.422)$ cd build
~/.emacs.d/elpa/irony-20180519.422/build
```

## Run the `CMAKE` for build, using the below command

```bash
$cmake \
 -DCMAKE_PREFIX_PATH=/opt/software/clang+llvm-10.0.0-x86_64-apple-darwin \
 -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON \-DLIBCLANG_LIBRARY=/opt/software/clang+llvm-10.0.0-x86_64-apple-darwin/lib/libclang.dylib \
 -DLIBCLANG_INCLUDE_DIR=/opt/software/clang+llvm-10.0.0-x86_64-apple-darwin/include \
 -DCMAKE_INSTALL_PREFIX=~/.emacs.d/irony/ \
 ~/.emacs.d/elpa/irony-20200130.849/server
```


```bash
(~/.emacs.d/elpa/irony-20200130.849/build)$
$cmake \
 -DCMAKE_PREFIX_PATH=/opt/software/clang+llvm-10.0.0-x86_64-apple-darwin \
 -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON \-DLIBCLANG_LIBRARY=/opt/software/clang+llvm-10.0.0-x86_64-apple-darwin/lib/libclang.dylib \
 -DLIBCLANG_INCLUDE_DIR=/opt/software/clang+llvm-10.0.0-x86_64-apple-darwin/include \
 -DCMAKE_INSTALL_PREFIX=~/.emacs.d/irony/ \
 ~/.emacs.d/elpa/irony-20200130.849/server
-- The C compiler identification is AppleClang 11.0.3.11030032
-- The CXX compiler identification is AppleClang 11.0.3.11030032
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Check for working C compiler: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc - skipped
-- Detecting C compile features
-- Detecting C compile features - done
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Check for working CXX compiler: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++ - skipped
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Irony package version is '1.4.0'
-- Found emacs: /Applications/emacs.app/Contents/MacOS/Emacs
-- Configuring done
-- Generating done
CMake Warning:
  Manually-specified variables were not used by the project:

    LIBCLANG_INCLUDE_DIR
    LIBCLANG_LIBRARY


-- Build files have been written to: /Users/sampathsingamsetty/aquamacs.d/elpa/irony-20200130.849/build
```

- below files are generated after the `cmake` run

```bash
(~/.emacs.d/elpa/irony-20200130.849/build)$ ls -ltra

total 112
drwxr-xr-x  25 sampathsingamsetty  staff    800 Jun 22 17:39 ..
drwxr-xr-x   3 sampathsingamsetty  staff     96 Jun 22 17:43 Testing
-rw-r--r--   1 sampathsingamsetty  staff   2940 Jun 22 17:43 DartConfiguration.tcl
-rw-r--r--   1 sampathsingamsetty  staff  19038 Jun 22 17:43 CMakeCache.txt
-rw-r--r--   1 sampathsingamsetty  staff  20684 Jun 22 17:43 Makefile
-rw-r--r--   1 sampathsingamsetty  staff   1886 Jun 22 17:43 cmake_install.cmake
-rw-r--r--   1 sampathsingamsetty  staff    391 Jun 22 17:43 CTestTestfile.cmake
drwxr-xr-x   2 sampathsingamsetty  staff     64 Jun 22 17:43 bin
drwxr-xr-x   7 sampathsingamsetty  staff    224 Jun 22 17:43 src
drwxr-xr-x   6 sampathsingamsetty  staff    192 Jun 22 17:43 docs
drwxr-xr-x   7 sampathsingamsetty  staff    224 Jun 22 17:43 test
drwxr-xr-x  43 sampathsingamsetty  staff   1376 Jun 22 17:43 CMakeFiles
drwxr-xr-x  13 sampathsingamsetty  staff    416 Jun 22 17:43 .


(~/.emacs.d/elpa/irony-20200130.849/build)$
$ ls -ltra src
total 120
-rw-r--r--   1 sampathsingamsetty  staff  32826 Jun 22 17:43 irony.el
-rw-r--r--   1 sampathsingamsetty  staff  13971 Jun 22 17:43 Makefile
-rw-r--r--   1 sampathsingamsetty  staff   2094 Jun 22 17:43 cmake_install.cmake
-rw-r--r--   1 sampathsingamsetty  staff    352 Jun 22 17:43 CTestTestfile.cmake
drwxr-xr-x   7 sampathsingamsetty  staff    224 Jun 22 17:43 .
drwxr-xr-x   5 sampathsingamsetty  staff    160 Jun 22 17:43 CMakeFiles
drwxr-xr-x  14 sampathsingamsetty  staff    448 Jun 22 17:45 ..
```

## Now build the `irony` server with the below command from within the build directory

>```bash
> $cmake --build . --use-stderr --config Release --target install
>```

```bash
(~/.emacs.d/elpa/irony-20200130.849/build)$

$ cmake --build . --use-stderr --config Release --target install
Scanning dependencies of target irony-server
[ 12%] Building CXX object src/CMakeFiles/irony-server.dir/support/CommandLineParser.cpp.o
[ 25%] Building CXX object src/CMakeFiles/irony-server.dir/support/TemporaryFile.cpp.o
[ 37%] Building CXX object src/CMakeFiles/irony-server.dir/Command.cpp.o
[ 50%] Building CXX object src/CMakeFiles/irony-server.dir/CompDBCache.cpp.o
[ 62%] Building CXX object src/CMakeFiles/irony-server.dir/Irony.cpp.o
[ 75%] Building CXX object src/CMakeFiles/irony-server.dir/TUManager.cpp.o
[ 87%] Building CXX object src/CMakeFiles/irony-server.dir/main.cpp.o
[100%] Linking CXX executable ../bin/irony-server
[100%] Built target irony-server
Install the project...
-- Install configuration: ""
-- Installing: /Users/sampathsingamsetty/.emacs.d/irony/bin/irony-server
```

## Check the irony-server version

```bash
~/.emacs.d/irony/bin/irony-server -v
irony-server version 1.4.0
clang version 10.0.0
```
