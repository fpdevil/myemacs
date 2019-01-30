#### YouCompleteMe

>*Set the DYLIB Path first*

```bash
$ export EXTRA_CMAKE_ARGS="-DPATH_TO_LLVM_ROOT=/opt/software/clang+llvm-7.0.0-x86_64-apple-darwin"

or

$ export EXTRA_CMAKE_ARGS="-DEXTERNAL_LIBCLANG_PATH=/Library/Developer/CommandLineTools/usr/lib/libclang.dylib"
```

>*Run the `install.py` script with `python3`*

```bash
$ python3 install.py --clang-completer --system-libclang --gocode-completer --tern-completer
```

```bash
Searching Python 3.5 libraries...
Found Python library: /usr/local/opt/python3/Frameworks/Python.framework/Versions/3.5/lib/python3.5/config-3.5m/libpython3.5.dylib
Found Python headers folder: /usr/local/Cellar/python3/3.5.2_3/Frameworks/Python.framework/Versions/3.5/include/python3.5m
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
Your C++ compiler supports C++11, compiling in that mode.
-- Found PythonLibs: /usr/local/opt/python3/Frameworks/Python.framework/Versions/3.5/lib/python3.5/config-3.5m/libpython3.5.dylib (found suitable version "3.5.2", minimum required is "3.3")
Using libclang to provide semantic completion for C/C++/ObjC
Using external libclang: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib
-- Found PythonInterp: /usr/local/bin/python3.5 (found version "3.5.2")
-- Looking for pthread.h
-- Looking for pthread.h - found
-- Looking for pthread_create
-- Looking for pthread_create - found
-- Found Threads: TRUE
-- Configuring done
-- Generating done
-- Build files have been written to: /private/var/folders/4f/2fn9d26d03jcj8_r2vlxbbqm0000gn/T/ycm_build_h34h2j40
Scanning dependencies of target BoostParts
[  0%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/atomic/src/lockpool.cpp.o
[  0%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/chrono/src/process_cpu_clocks.cpp.o
[  1%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/chrono/src/chrono.cpp.o
[  3%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/date_time/src/gregorian/date_generators.cpp.o
[  3%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/chrono/src/thread_clock.cpp.o
[  5%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/date_time/src/gregorian/greg_month.cpp.o
[  5%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/date_time/src/gregorian/gregorian_types.cpp.o
[  5%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/date_time/src/gregorian/greg_weekday.cpp.o
[  5%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/date_time/src/posix_time/posix_time_types.cpp.o
[  7%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/exception/src/clone_current_exception_non_intrusive.cpp.o
[  7%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/codecvt_error_category.cpp.o
[  8%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/operations.cpp.o
[  9%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/path.cpp.o
[  9%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/path_traits.cpp.o
[ 10%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/portability.cpp.o
[ 11%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/unique_path.cpp.o
[ 11%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/utf8_codecvt_facet.cpp.o
[ 13%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/filesystem/src/windows_file_codecvt.cpp.o
[ 14%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/converter/arg_to_python_base.cpp.o
[ 14%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/converter/builtin_converters.cpp.o
[ 15%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/converter/from_python.cpp.o
[ 15%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/converter/registry.cpp.o
[ 16%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/converter/type_id.cpp.o
[ 17%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/dict.cpp.o
[ 17%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/errors.cpp.o
[ 19%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/exec.cpp.o
[ 20%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/import.cpp.o
[ 20%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/list.cpp.o
[ 21%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/long.cpp.o
[ 21%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/module.cpp.o
[ 22%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/numeric.cpp.o
[ 23%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/class.cpp.o
[ 23%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/enum.cpp.o
[ 25%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/function.cpp.o
[ 26%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/function_doc_signature.cpp.o
[ 26%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/inheritance.cpp.o
[ 27%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/iterator.cpp.o
[ 28%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/life_support.cpp.o
[ 28%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/pickle_support.cpp.o
[ 29%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object/stl_iterator.cpp.o
[ 29%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object_operators.cpp.o
[ 30%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/object_protocol.cpp.o
[ 32%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/slice.cpp.o
[ 32%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/str.cpp.o
[ 33%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/tuple.cpp.o
[ 34%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/python/src/wrapper.cpp.o
[ 34%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/c_regex_traits.cpp.o
[ 35%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/cpp_regex_traits.cpp.o
[ 36%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/fileiter.cpp.o
[ 36%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/cregex.cpp.o
[ 38%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/icu.cpp.o
[ 38%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/instances.cpp.o
[ 39%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/posix_api.cpp.o
[ 40%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/regex.cpp.o
[ 40%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/regex_debug.cpp.o
[ 41%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/regex_raw_buffer.cpp.o
[ 42%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/regex_traits_defaults.cpp.o
[ 42%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/static_mutex.cpp.o
[ 44%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/usinstances.cpp.o
[ 44%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/w32_regex_traits.cpp.o
[ 45%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/wc_regex_traits.cpp.o
[ 46%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/wide_posix_api.cpp.o
[ 46%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/regex/src/winstances.cpp.o
[ 47%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/archive_exception.cpp.o
[ 48%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_archive.cpp.o
[ 48%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_iarchive.cpp.o
[ 50%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_iserializer.cpp.o
[ 51%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_oarchive.cpp.o
[ 51%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_oserializer.cpp.o
[ 52%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_pointer_iserializer.cpp.o
[ 52%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_pointer_oserializer.cpp.o
[ 53%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_serializer_map.cpp.o
[ 54%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_text_iprimitive.cpp.o
[ 54%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_text_oprimitive.cpp.o
[ 55%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_text_wiprimitive.cpp.o
[ 57%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_text_woprimitive.cpp.o
[ 57%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/basic_xml_archive.cpp.o
[ 58%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/binary_iarchive.cpp.o
[ 59%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/binary_oarchive.cpp.o
[ 59%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/binary_wiarchive.cpp.o
[ 60%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/binary_woarchive.cpp.o
[ 60%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/codecvt_null.cpp.o
[ 61%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/extended_type_info.cpp.o
[ 63%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/extended_type_info_no_rtti.cpp.o
[ 63%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/extended_type_info_typeid.cpp.o
[ 64%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/polymorphic_iarchive.cpp.o
[ 65%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/polymorphic_oarchive.cpp.o
[ 65%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/singleton.cpp.o
[ 66%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/stl_port.cpp.o
[ 66%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/text_iarchive.cpp.o
[ 67%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/text_oarchive.cpp.o
[ 69%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/text_wiarchive.cpp.o
[ 69%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/text_woarchive.cpp.o
[ 70%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/utf8_codecvt_facet.cpp.o
[ 71%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/void_cast.cpp.o
[ 71%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/xml_archive_exception.cpp.o
[ 72%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/xml_grammar.cpp.o
[ 73%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/xml_iarchive.cpp.o
[ 73%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/xml_oarchive.cpp.o
[ 75%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/xml_wgrammar.cpp.o
[ 75%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/serialization/src/xml_wiarchive.cpp.o
[ 76%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/smart_ptr/src/sp_collector.cpp.o
[ 77%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/smart_ptr/src/sp_debug_hooks.cpp.o
[ 77%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/system/src/error_code.cpp.o
[ 78%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/timer/src/auto_timers_construction.cpp.o
[ 79%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/timer/src/cpu_timer.cpp.o
[ 79%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/thread/src/future.cpp.o
[ 80%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/thread/src/pthread/once.cpp.o
[ 80%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/thread/src/pthread/once_atomic.cpp.o
[ 82%] Building CXX object BoostParts/CMakeFiles/BoostParts.dir/libs/thread/src/pthread/thread.cpp.o
[ 83%] Linking CXX static library libBoostParts.a
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(gregorian_types.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(posix_time_types.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(windows_file_codecvt.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(icu.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(regex_debug.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(usinstances.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(w32_regex_traits.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(stl_port.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(sp_collector.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(sp_debug_hooks.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(gregorian_types.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(posix_time_types.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(windows_file_codecvt.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(icu.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(regex_debug.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(usinstances.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(w32_regex_traits.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(stl_port.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(sp_collector.cpp.o) has no symbols
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ranlib: file: libBoostParts.a(sp_debug_hooks.cpp.o) has no symbols
[ 83%] Built target BoostParts
Scanning dependencies of target ycm_core
[ 83%] Building CXX object ycm/CMakeFiles/ycm_core.dir/Candidate.cpp.o
[ 84%] Building CXX object ycm/CMakeFiles/ycm_core.dir/CandidateRepository.cpp.o
[ 85%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/ClangCompleter.cpp.o
[ 86%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/ClangHelpers.cpp.o
[ 86%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/ClangUtils.cpp.o
[ 86%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/CompletionData.cpp.o
[ 88%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/CompilationDatabase.cpp.o
[ 89%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/Documentation.cpp.o
[ 89%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/Range.cpp.o
[ 90%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/TranslationUnit.cpp.o
[ 91%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ClangCompleter/TranslationUnitStore.cpp.o
[ 91%] Building CXX object ycm/CMakeFiles/ycm_core.dir/CustomAssert.cpp.o
[ 92%] Building CXX object ycm/CMakeFiles/ycm_core.dir/IdentifierCompleter.cpp.o
[ 94%] Building CXX object ycm/CMakeFiles/ycm_core.dir/IdentifierDatabase.cpp.o
[ 94%] Building CXX object ycm/CMakeFiles/ycm_core.dir/IdentifierUtils.cpp.o
[ 95%] Building CXX object ycm/CMakeFiles/ycm_core.dir/LetterNode.cpp.o
[ 95%] Building CXX object ycm/CMakeFiles/ycm_core.dir/LetterNodeListMap.cpp.o
[ 96%] Building CXX object ycm/CMakeFiles/ycm_core.dir/PythonSupport.cpp.o
[ 97%] Building CXX object ycm/CMakeFiles/ycm_core.dir/Result.cpp.o
[ 97%] Building CXX object ycm/CMakeFiles/ycm_core.dir/Utils.cpp.o
[ 98%] Building CXX object ycm/CMakeFiles/ycm_core.dir/versioning.cpp.o
[100%] Building CXX object ycm/CMakeFiles/ycm_core.dir/ycm_core.cpp.o
[100%] Linking CXX shared library /Users/sampathsingamsetty/sw/programming/python/YouCompleteMe/third_party/ycmd/ycm_core.so
[100%] Built target ycm_core
npm WARN tern_runtime No repository field.
npm WARN tern_runtime No license field.
```