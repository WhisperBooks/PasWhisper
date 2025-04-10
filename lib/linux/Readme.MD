This directory should hold the Linux libraries

Names can vary a but of course but e.g.

- libwhisper.so
- libggml-cpu.so
- libggml-vulkan.so
- libggml-base.so
- libggml.so
- libggml-cuda.so

You can build the libraries on Linux. I do it something like this...

`
cmake -B build -DWHISPER_COREML=1 -DBUILD_SHARED_LIBS=ON -DWHISPER_SDL2=ON
cmake --build build -j --config Release


mkdir lib
mkdir lib/linux


cp build/src/libwhisper.so lib/linux
cp build/ggml/src/libggml-cpu.so lib/linux
cp build/ggml/src/ggml-vulkan/libggml-vulkan.so lib/linux
cp build/ggml/src/libggml-base.so lib/linux
cp build/ggml/src/libggml.so lib/linux
cp build/ggml/src/ggml-cuda/libggml-cuda.so lib/linux
`


Then SFTP the lib folder back to Windows

You can just replace the libraries in the PAServer folder if you like to test different versions

The app(s) only link libwhisper.so (which can ask for others of course)