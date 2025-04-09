This directory should hold the Mac libraries

Names can vary a but of course but e.g.

- libggml-base.dylib

- libggml-blas.dylib
- libggml-cpu.dylib
- libggml-metal.dylib
- libggml.dylib
- libwhisper.1.7.5.dylib
- libwhisper.1.dylib
- libwhisper.coreml.dylib
- libwhisper.dylib

You can build the libraries on Mac. I do it something like this...

`cmake -B build.coreml -DWHISPER_COREML=1 -DBUILD_SHARED_LIBS=ON -DWHISPER_SDL2=ON`
`cmake --build build.coreml -j --config Release`

`mkdir lib`
`mkdir lib/coreml`
`cp build.coreml/ggml/src/ggml-blas/libggml-blas.dylib lib/coreml`
`cp build.coreml/ggml/src/libggml.dylib lib/coreml`
`cp build.coreml/ggml/src/libggml-base.dylib lib/coreml`
`cp build.coreml/ggml/src/ggml-metal/libggml-metal.dylib lib/coreml`
`cp build.coreml/ggml/src/libggml-cpu.dylib lib/coreml`
`cp build.coreml/src/libwhisper.1.7.5.dylib lib/coreml`
`cp build.coreml/src/libwhisper.dylib lib/coreml`
`cp build.coreml/src/libwhisper.coreml.dylib lib/coreml`
`cp build.coreml/src/libwhisper.1.dylib lib/coreml`

Then SFTP the lib folder back to Windows

You can just replace the libraries in the .app folder if you like to test different versions

The app(s) only link libwhisper.dylib (which can ask for others of course)