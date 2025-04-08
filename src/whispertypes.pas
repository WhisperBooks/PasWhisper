unit whispertypes;

interface

{$ALIGN 4}
{$A+}
{$MinEnumSize 4}

uses GgmlExternal;

type
  Int64_t = Int64;
  Int32_t = Int32;
  WhisperBool = ByteBool;
  Size_t = UInt64;
  Float = Single;
  TFloatArray = array of Single;

  e_model = (
    MODEL_UNKNOWN,
    MODEL_TINY,
    MODEL_BASE,
    MODEL_SMALL,
    MODEL_MEDIUM,
    MODEL_LARGE
  );

  TWhisperMel = record
    n_len: Integer;
    n_len_org: Integer;
    n_mel: Integer;
    data: TFloatArray;
  end;

  TWhisperFilters = record
    n_mel: Int32;
    n_fft: Int32;
    data: TFloatArray;
  end;

  TWhisperHparams = record
    const
    n_vocab       : Int32_t = 51864;
    n_audio_ctx   : Int32_t = 1500;
    n_audio_state : Int32_t = 384;
    n_audio_head  : Int32_t = 6;
    n_audio_layer : Int32_t = 4;
    n_text_ctx    : Int32_t = 448;
    n_text_state  : Int32_t = 384;
    n_text_head   : Int32_t = 6;
    n_text_layer  : Int32_t = 4;
    n_mels        : Int32_t = 80;
    ftype         : Int32_t = 1;
    eps           : Float   = 1e-5;
  end;

  // audio encoding layer
  PWhisperLayerEncoder = ^TWhisperLayerEncoder;
  TWhisperLayerEncoder = record
    // encoder.blocks.*.attn_ln
    attn_ln_0_w: PggmlTensor;
    attn_ln_0_b: PggmlTensor;

    // encoder.blocks.*.attn.out
    attn_ln_1_w: PggmlTensor;
    attn_ln_1_b: PggmlTensor;

    // encoder.blocks.*.attn.query
    attn_q_w: PggmlTensor;
    attn_q_b: PggmlTensor;

    // encoder.blocks.*.attn.key
    attn_k_w: PggmlTensor;

    // encoder.blocks.*.attn.value
    attn_v_w: PggmlTensor;
    attn_v_b: PggmlTensor;

    // encoder.blocks.*.mlp_ln
    mlp_ln_w: PggmlTensor;
    mlp_ln_b: PggmlTensor;

    // encoder.blocks.*.mlp.0
    mlp_0_w: PggmlTensor;
    mlp_0_b: PggmlTensor;

    // encoder.blocks.*.mlp.2
    mlp_1_w: PggmlTensor;
    mlp_1_b: PggmlTensor;
  end;

  // token decoding layer
  PWhisperLayerDecoder = ^TWhisperLayerDecoder;
  TWhisperLayerDecoder = record
    // decoder.blocks.*.attn_ln
    attn_ln_0_w: PggmlTensor;
    attn_ln_0_b: PggmlTensor;

    // decoder.blocks.*.attn.out
    attn_ln_1_w: PggmlTensor;
    attn_ln_1_b: PggmlTensor;

    // decoder.blocks.*.attn.query
    attn_q_w: PggmlTensor;
    attn_q_b: PggmlTensor;

    // decoder.blocks.*.attn.key
    attn_k_w: PggmlTensor;

    // decoder.blocks.*.attn.value
    attn_v_w: PggmlTensor;
    attn_v_b: PggmlTensor;

    // decoder.blocks.*.cross_attn_ln
    cross_attn_ln_0_w: PggmlTensor;
    cross_attn_ln_0_b: PggmlTensor;

    // decoder.blocks.*.cross_attn.out
    cross_attn_ln_1_w: PggmlTensor;
    cross_attn_ln_1_b: PggmlTensor;

    // decoder.blocks.*.cross_attn.query
    cross_attn_q_w: PggmlTensor;
    cross_attn_q_b: PggmlTensor;

    // decoder.blocks.*.cross_attn.key
    cross_attn_k_w: PggmlTensor;

    // decoder.blocks.*.cross_attn.value
    cross_attn_v_w: PggmlTensor;
    cross_attn_v_b: PggmlTensor;

    // decoder.blocks.*.mlp_ln
    mlp_ln_w: PggmlTensor;
    mlp_ln_b: PggmlTensor;

    // decoder.blocks.*.mlp.0
    mlp_0_w: PggmlTensor;
    mlp_0_b: PggmlTensor;

    // decoder.blocks.*.mlp.2
    mlp_1_w: PggmlTensor;
    mlp_1_b: PggmlTensor;
  end;

  PggmlContext = Pointer;
  PggmlBackendBuffer = Pointer;

  TStdMap16 = Array [0..15] of byte;
  TStdMap24 = Array [0..23] of byte;

  TWhisperModel = record
  var
    mtype:          Int32; // = e_model.MODEL_UNKNOWN;
    hparams:        TWhisperHparams;
    filters:        TWhisperFilters;
    e_pe:           PggmlTensor; // encoder.positional_embedding
    e_conv_1_w:     PggmlTensor; // encoder.conv1
    e_conv_1_b:     PggmlTensor;
    e_conv_2_w:     PggmlTensor; // encoder.conv2
    e_conv_2_b:     PggmlTensor;
    e_ln_w:         PggmlTensor; // encoder.ln_post
    e_ln_b:         PggmlTensor;
    d_pe:           PggmlTensor; // decoder.positional_embedding
    d_te:           PggmlTensor; // decoder.token_embedding
    d_ln_w:         PggmlTensor; // decoder.ln
    d_ln_b:         PggmlTensor;
    layers_encoder: TStdMap24; // Map of PWhisperLayerEncoder;
    layers_decoder: TStdMap24; // Map of PWhisperLayerDecoder;
    // ggml context that contains all the meta information about the model tensors
    ctxs:           TStdMap24; // Map of PggmlContext;
    // the model backend data is read-only and can be shared between processors
    buffer:         TStdMap24; // PggmlBackendBuffer;
    // tensors
    n_loaded:       Int32;
    tensors:        TStdMap16; // Map of PggmlTensor
  end;
  PWhisperModel = ^TWhisperModel;

  TWhisperAlignmentHeadsPreset = (
    WHISPER_AHEADS_UNDEFIN£ED = -1,
    WHISPER_AHEADS_NONE,
    WHISPER_AHEADS_N_TOP_MOST,  // All heads from the N-top-most text-layers
    WHISPER_AHEADS_CUSTOM,
    WHISPER_AHEADS_TINY_EN,
    WHISPER_AHEADS_TINY,
    WHISPER_AHEADS_BASE_EN,
    WHISPER_AHEADS_BASE,
    WHISPER_AHEADS_SMALL_EN,
    WHISPER_AHEADS_SMALL,
    WHISPER_AHEADS_MEDIUM_EN,
    WHISPER_AHEADS_MEDIUM,
    WHISPER_AHEADS_LARGE_V1,
    WHISPER_AHEADS_LARGE_V2,
    WHISPER_AHEADS_LARGE_V3
    );

  TWhisperAhead = record
    nTextLayer: Int32;
    nHead: Int32;
  end;
  PWhisperAhead = ^TWhisperAhead;

  TWhisperAheads = record
    nHeads: UInt64;
    Heads: PWhisperAhead;
  end;

  TWhisperContextParams = record
    use_gpu: WhisperBool;
    flash_attn: WhisperBool;
    gpu_device: Int32;  // CUDA device
    dtw_token_timestamps: WhisperBool; // [EXPERIMENTAL] Token-level timestamps with DTW
    dtw_aheads_preset: TWhisperAlignmentHeadsPreset;
    dtw_n_top: Int32;
    dtw_aheads: TWhisperAheads;
    dtw_mem_size: UInt64; // TODO: remove
  end;
  PWhisperContextParams = ^TWhisperContextParams;


{
  TwhisperState = record
    t_sample_us: int64_t;
    t_encode_us: int64_t;
    t_decode_us: int64_t;
    t_batchd_us: int64_t;
    t_prompt_us: int64_t;
    t_mel_us: int64_t;

    n_sample: int32_t; // number of tokens sampled
    n_encode: int32_t; // number of encoder calls
    n_decode: int32_t; // number of decoder calls with n_tokens == 1  (text-generation)
    n_batchd: int32_t; // number of decoder calls with n_tokens <  16 (batch decoding)
    n_prompt: int32_t; // number of decoder calls with n_tokens >  1  (prompt encoding)
    n_fail_p: int32_t; // number of logprob threshold failures
    n_fail_h: int32_t; // number of entropy threshold failures

    // number of decoders for which we have constructed the KV cache
    kv_self_n_dec: int32_t;

    // unified self-attention KV cache for all decoders
    whisper_kv_cache kv_self;

    // cross-attention KV cache for the decoders
    // shared between all decoders
    whisper_kv_cache kv_cross;

    // padded buffer for flash-attention
    whisper_kv_cache kv_pad;

    whisper_mel mel;

    whisper_batch batch;

    whisper_decoder decoders[WHISPER_MAX_DECODERS];

    std::vector<ggml_backend_t> backends;

    // - stores meta info about the intermediate tensors into the `meta` buffers
    whisper_sched sched_conv;
    whisper_sched sched_encode;
    whisper_sched sched_cross;
    whisper_sched sched_decode;

    // result of the encoder
    struct ggml_tensor * embd_conv = nullptr;
    struct ggml_tensor * embd_enc  = nullptr;

    // helpers for GPU offloading
    std::vector<float> inp_mel;
    std::vector<float> inp_mask;

    // decode output (2-dimensional array: [n_tokens][n_vocab])
    std::vector<float> logits;

    std::vector<whisper_segment> result_all;
    std::vector<whisper_token>   prompt_past;

    int lang_id = 0; // english by default

    std::string path_model; // populated by whisper_init_from_file_with_params()

#ifdef WHISPER_USE_COREML
    whisper_coreml_context * ctx_coreml = nullptr;
#endif

#ifdef WHISPER_USE_OPENVINO
    whisper_openvino_context * ctx_openvino = nullptr;
#endif

    // [EXPERIMENTAL] token-level timestamps data
    int64_t t_beg  = 0;
    int64_t t_last = 0;

    whisper_token tid_last;

    std::vector<float> energy; // PCM signal energy
    float no_speech_prob = 0.0f;

    // [EXPERIMENTAL] Token-level timestamps with DTW
    whisper_aheads_masks aheads_masks;
    ggml_tensor * aheads_cross_QKs = nullptr;
    std::vector<float> aheads_cross_QKs_data;

    // [EXPERIMENTAL] speed-up techniques
    int32_t exp_n_audio_ctx = 0; // 0 - use default



}


  TVocabID = Int32;

  TWhisperVocab = record
  var
    n_vocab: Int64; // 51864;
    token_to_id: TStdMap16; // std::map<token, id>
    id_to_token: TStdMap16; // std::map<id, token>
    // reference: https://github.com/openai/whisper/blob/248b6cb124225dd263bb9bd32d060b6517e067f8/whisper/tokenizer.py#L334-L349
    token_eot: TVocabID; // 50256;
    token_sot: TVocabID; // 50257;
    // task tokens (used only for multilingual models)
    token_translate: TVocabID; // 50357;
    token_transcribe: TVocabID; // 50358;
    // other special tokens
    token_solm: TVocabID; // 50359; // [TDRZ] used by tinydiarize models to indicate speaker turn
    token_prev: TVocabID; // 50360;
    token_nosp: TVocabID; // 50361;
    token_not: TVocabID; // 50362; // no timestamps
    token_beg: TVocabID; // 50363; // begin timestamps
  end;


  TWhisperContext = record
  var
    t_load_us: int64_t;
    t_start_us: int64_t ;
  const
    wtype = ggmlType.GGML_TYPE_F16; // weight type (FP32 / FP16 / QX)
    itype =  ggmlType.GGML_TYPE_F16; // intermediate type (FP32 or FP16)
  var
    params: TWhisperContextParams;
    model: TWhisperModel;
    vocab: TWhisperVocab; // TWhisperVocab;
    state: Pointer; // TwhisperState;
    path_model: PAnsiChar; // populated by whisper_init_from_file_with_params()
  end;
  PWhisperContext = ^TWhisperContext;

implementation

end.
