unit ggmltypes;

interface

{$IFDEF FPC}
  {$packrecords C}
{$ELSE}
  {$ALIGN 4}
{$ENDIF}
{$MinEnumSize 4}

type

  TCallBack = Pointer;

  {$ALIGN 8}
  TGgmlBackendRegInterface = record
    GetName: TCallBack;
    GetDeviceCount: TCallBack;
    GetDevice: TCallBack;
    GetProcAddress: TCallBack;
  end;

  TGgmlBackendReg = record
    ApiVersion: Int32;
    IFace: TGgmlBackendRegInterface;
    Context: Pointer;
  end;
  PGgmlBackendReg = ^TGgmlBackendReg;

  TGgmlBackendDeviceInterface = record
{
        // device name: short identifier for this device, such as "CPU" or "CUDA0"
        const char * (*get_name)(ggml_backend_dev_t dev);

        // device description: short informative description of the device, could be the model name
        const char * (*get_description)(ggml_backend_dev_t dev);

        // device memory in bytes
        void         (*get_memory)(ggml_backend_dev_t dev, size_t * free, size_t * total);

        // device type
        enum ggml_backend_dev_type (*get_type)(ggml_backend_dev_t dev);

        // device properties
        void (*get_props)(ggml_backend_dev_t dev, struct ggml_backend_dev_props * props);

        // backend (stream) initialization
        ggml_backend_t (*init_backend)(ggml_backend_dev_t dev, const char * params);

        // preferred buffer type
        ggml_backend_buffer_type_t (*get_buffer_type)(ggml_backend_dev_t dev);

        // (optional) host buffer type (in system memory, typically this is a pinned memory buffer for faster transfers between host and device)
        ggml_backend_buffer_type_t (*get_host_buffer_type)(ggml_backend_dev_t dev);

        // (optional) buffer from pointer: create a buffer from a host pointer (useful for memory mapped models and importing data from other libraries)
        ggml_backend_buffer_t (*buffer_from_host_ptr)(ggml_backend_dev_t dev, void * ptr, size_t size, size_t max_tensor_size);

        // check if the backend can compute an operation
        bool (*supports_op)(ggml_backend_dev_t dev, const struct ggml_tensor * op);

        // check if the backend can use tensors allocated in a buffer type
        bool (*supports_buft)(ggml_backend_dev_t dev, ggml_backend_buffer_type_t buft);

        // (optional) check if the backend wants to run an operation, even if the weights are allocated in an incompatible buffer
        // these should be expensive operations that may benefit from running on this backend instead of the CPU backend
        bool (*offload_op)(ggml_backend_dev_t dev, const struct ggml_tensor * op);

        // (optional) event synchronization
        ggml_backend_event_t (*event_new)         (ggml_backend_dev_t dev);
        void                 (*event_free)        (ggml_backend_dev_t dev, ggml_backend_event_t event);
        void                 (*event_synchronize) (ggml_backend_dev_t dev, ggml_backend_event_t event);
}
  end;
  PGgmlBackendDeviceInterface = ^TGgmlBackendDeviceInterface;

  TGgmlBackendDev = record
    IFace: TGgmlBackendDeviceInterface;
    Reg: PGgmlBackendReg;
    Context: Pointer;
  end;
  PGgmlBackendDev = ^TGgmlBackendDev;

  {$ALIGN 4}

  TFloatArray = array of Single;
  ggmlType = (
    GGML_TYPE_F32     = 0,
    GGML_TYPE_F16     = 1,
    GGML_TYPE_Q4_0    = 2,
    GGML_TYPE_Q4_1    = 3,
    // GGML_TYPE_Q4_2 = 4, support has been removed
    // GGML_TYPE_Q4_3 = 5, support has been removed
    GGML_TYPE_Q5_0    = 6,
    GGML_TYPE_Q5_1    = 7,
    GGML_TYPE_Q8_0    = 8,
    GGML_TYPE_Q8_1    = 9,
    GGML_TYPE_Q2_K    = 10,
    GGML_TYPE_Q3_K    = 11,
    GGML_TYPE_Q4_K    = 12,
    GGML_TYPE_Q5_K    = 13,
    GGML_TYPE_Q6_K    = 14,
    GGML_TYPE_Q8_K    = 15,
    GGML_TYPE_IQ2_XXS = 16,
    GGML_TYPE_IQ2_XS  = 17,
    GGML_TYPE_IQ3_XXS = 18,
    GGML_TYPE_IQ1_S   = 19,
    GGML_TYPE_IQ4_NL  = 20,
    GGML_TYPE_IQ3_S   = 21,
    GGML_TYPE_IQ2_S   = 22,
    GGML_TYPE_IQ4_XS  = 23,
    GGML_TYPE_I8      = 24,
    GGML_TYPE_I16     = 25,
    GGML_TYPE_I32     = 26,
    GGML_TYPE_I64     = 27,
    GGML_TYPE_F64     = 28,
    GGML_TYPE_IQ1_M   = 29,
    GGML_TYPE_BF16    = 30,
    GGML_TYPE_Q4_0_4_4 = 31,
    GGML_TYPE_Q4_0_4_8 = 32,
    GGML_TYPE_Q4_0_8_8 = 33,
    GGML_TYPE_TQ1_0   = 34,
    GGML_TYPE_TQ2_0   = 35,
    GGML_TYPE_COUNT
  );

const
  GGML_MAX_DIMS = 4;
  GGML_MAX_OP_PARAMS = 32;
  GGML_MAX_SRC = 8;
  GGML_MAX_NAME = 64;


type
  TGgmlType = (ggmlTypeUnknown, ggmlTypeFloat, ggmlTypeInt); // Example enum values
  TGgmlOp = (ggmlOpNone, ggmlOpAdd, ggmlOpMul); // Example enum values

  TGgmlBackendBuffer = record
    // Define the structure of ggml_backend_buffer here
  end;

  PGgmlTensor = ^TGgmlTensor;
  TGgmlTensor = record
    &Type: TggmlType;
    // Deprecated: Backend: TggmlBackendType; // Use Buffer to find storage location
    Buffer: ^TggmlBackendBuffer;
    Ne: array[0..GGML_MAX_DIMS-1] of Int64; // Number of elements
    Nb: array[0..GGML_MAX_DIMS-1] of Int32; // Stride in bytes
    Op: TggmlOp;
    OpParams: array[0..(GGML_MAX_OP_PARAMS div SizeOf(Int32))-1] of Int32; // Op params for alignment
    Flags: Int32;
    Grad: PggmlTensor;
    Src: array[0..GGML_MAX_SRC-1] of PGgmlTensor;
    ViewSrc: PGgmlTensor; // Source tensor for views
    ViewOffs: Int32; // Offset for views
    Data: Pointer;
    Name: array[0..GGML_MAX_NAME-1] of AnsiChar;
    Extra: Pointer; // Extra things e.g. for ggml-cuda.cu
    // Padding: array[0..3] of AnsiChar; // Optional padding
  end;
implementation

{ TLanguageInfo }

end.
