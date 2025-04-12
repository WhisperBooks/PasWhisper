unit GgmlExternal;

interface

{$IFDEF FPC}
  {$packrecords C}
{$ELSE}
  {$ALIGN 4}
{$ENDIF}
{$MinEnumSize 4}

type

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
  TggmlType = (ggmlTypeUnknown, ggmlTypeFloat, ggmlTypeInt); // Example enum values
  TggmlOp = (ggmlOpNone, ggmlOpAdd, ggmlOpMul); // Example enum values

  TggmlBackendBuffer = record
    // Define the structure of ggml_backend_buffer here
  end;

  PggmlTensor = ^TggmlTensor;
  TggmlTensor = record
    &Type: TggmlType;
    // Deprecated: Backend: TggmlBackendType; // Use Buffer to find storage location
    Buffer: ^TggmlBackendBuffer;
    Ne: array[0..GGML_MAX_DIMS-1] of Int64; // Number of elements
    Nb: array[0..GGML_MAX_DIMS-1] of Int32; // Stride in bytes
    Op: TggmlOp;
    OpParams: array[0..(GGML_MAX_OP_PARAMS div SizeOf(Int32))-1] of Int32; // Op params for alignment
    Flags: Int32;
    Grad: PggmlTensor;
    Src: array[0..GGML_MAX_SRC-1] of PggmlTensor;
    ViewSrc: PggmlTensor; // Source tensor for views
    ViewOffs: Int32; // Offset for views
    Data: Pointer;
    Name: array[0..GGML_MAX_NAME-1] of AnsiChar;
    Extra: Pointer; // Extra things e.g. for ggml-cuda.cu
    // Padding: array[0..3] of AnsiChar; // Optional padding
  end;
implementation

{ TLanguageInfo }

end.
