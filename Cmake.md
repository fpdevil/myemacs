# Simple c++ project with a compilation database using cmake

Irony requires a compilation database. To create one do the following:

* create a project directory structure eg: my-project
* check cmake version with `cmake --version`
* under the project directory root, create a simple `CMakeLists.txt` file with the below contents
```bash
 	set(MIN_VER_CMAKE 2.8.12.2)
	cmake_minimum_required(VERSION "${MIN_VER_CMAKE}")
	project (my-project)
```
* run the command `cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1` inorder to generate a compilation databse
 ```
  $ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
-- The C compiler identification is AppleClang 9.0.0.9000038
-- The CXX compiler identification is AppleClang 9.0.0.9000038
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
-- Configuring done
-- Generating done
-- Build files have been written to: ~/my-project
 ```

### Important references:

 * [cmake-tutorial](https://cmake.org/cmake-tutorial/) for a good understanding of the whole Cmake process
 * [Emacs c++-development](https://tuhdo.github.io/c-ide.html) for the awesome Emacs IDE configuration
