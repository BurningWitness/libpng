{-# LANGUAGE OverloadedStrings
           , PatternSynonyms #-}

module Codec.Image.PNG.Internal.Raw where

import           Data.Int
import           Data.String (IsString)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           System.Posix.Internals

#include <png.h>

-- | @const *@
type ConstPtr = Ptr

-- | @* restrict@
type RPtr = Ptr

-- | @const * restrict@
type ConstRPtr = Ptr


pattern PNG_LIBPNG_VER_STRING
      , PNG_HEADER_VERSION_STRING
     :: (IsString a, Ord a) => a
pattern PNG_LIBPNG_VER_STRING     = #const_str PNG_LIBPNG_VER_STRING
pattern PNG_HEADER_VERSION_STRING = #const_str PNG_HEADER_VERSION_STRING


pattern PNG_LIBPNG_VER_SONUM
      , PNG_LIBPNG_VER_DLLNUM
      , PNG_LIBPNG_VER_MAJOR
      , PNG_LIBPNG_VER_MINOR
      , PNG_LIBPNG_VER_RELEASE
      , PNG_LIBPNG_VER_BUILD
      , PNG_LIBPNG_BUILD_ALPHA
      , PNG_LIBPNG_BUILD_BETA
      , PNG_LIBPNG_BUILD_RC
      , PNG_LIBPNG_BUILD_STABLE
      , PNG_LIBPNG_BUILD_RELEASE_STATUS_MASK
      , PNG_LIBPNG_BUILD_PATCH
      , PNG_LIBPNG_BUILD_PRIVATE
      , PNG_LIBPNG_BUILD_SPECIAL
      , PNG_LIBPNG_BUILD_BASE_TYPE
      , PNG_LIBPNG_VER
      , PNG_TEXT_COMPRESSION_NONE_WR
      , PNG_TEXT_COMPRESSION_zTXt_WR
      , PNG_TEXT_COMPRESSION_NONE
      , PNG_TEXT_COMPRESSION_zTXt
      , PNG_ITXT_COMPRESSION_NONE
      , PNG_ITXT_COMPRESSION_zTXt
      , PNG_TEXT_COMPRESSION_LAST
      , PNG_HAVE_IHDR
      , PNG_HAVE_PLTE
      , PNG_AFTER_IDAT
      , PNG_FP_1
      , PNG_FP_HALF
      , PNG_COLOR_MASK_PALETTE
      , PNG_COLOR_MASK_COLOR
      , PNG_COLOR_MASK_ALPHA
      , PNG_COLOR_TYPE_GRAY
      , PNG_COLOR_TYPE_RGBA
      , PNG_COLOR_TYPE_GA
      , PNG_COMPRESSION_TYPE_BASE
      , PNG_COMPRESSION_TYPE_DEFAULT
      , PNG_FILTER_TYPE_BASE
      , PNG_INTRAPIXEL_DIFFERENCING
      , PNG_FILTER_TYPE_DEFAULT
      , PNG_INTERLACE_NONE
      , PNG_INTERLACE_ADAM7
      , PNG_INTERLACE_LAST
      , PNG_OFFSET_PIXEL
      , PNG_OFFSET_MICROMETER
      , PNG_OFFSET_LAST
      , PNG_EQUATION_LINEAR
      , PNG_EQUATION_BASE_E
      , PNG_EQUATION_ARBITRARY
      , PNG_EQUATION_HYPERBOLIC
      , PNG_EQUATION_LAST
      , PNG_SCALE_UNKNOWN
      , PNG_SCALE_METER
      , PNG_SCALE_RADIAN
      , PNG_SCALE_LAST
      , PNG_RESOLUTION_UNKNOWN
      , PNG_RESOLUTION_METER
      , PNG_RESOLUTION_LAST
      , PNG_sRGB_INTENT_PERCEPTUAL
      , PNG_sRGB_INTENT_RELATIVE
      , PNG_sRGB_INTENT_SATURATION
      , PNG_sRGB_INTENT_ABSOLUTE
      , PNG_sRGB_INTENT_LAST
      , PNG_KEYWORD_MAX_LENGTH
      , PNG_MAX_PALETTE_LENGTH
      , PNG_INFO_gAMA
      , PNG_INFO_sBIT
      , PNG_INFO_cHRM
      , PNG_INFO_PLTE
      , PNG_INFO_tRNS
      , PNG_INFO_bKGD
      , PNG_INFO_hIST
      , PNG_INFO_pHYs
      , PNG_INFO_oFFs
      , PNG_INFO_tIME
      , PNG_INFO_pCAL
      , PNG_INFO_sRGB
      , PNG_INFO_iCCP
      , PNG_INFO_sPLT
      , PNG_INFO_sCAL
      , PNG_INFO_IDAT
      , PNG_INFO_eXIf
      , PNG_TRANSFORM_IDENTITY
      , PNG_TRANSFORM_STRIP_16
      , PNG_TRANSFORM_STRIP_ALPHA
      , PNG_TRANSFORM_PACKING
      , PNG_TRANSFORM_PACKSWAP
      , PNG_TRANSFORM_EXPAND
      , PNG_TRANSFORM_INVERT_MONO
      , PNG_TRANSFORM_SHIFT
      , PNG_TRANSFORM_BGR
      , PNG_TRANSFORM_SWAP_ALPHA
      , PNG_TRANSFORM_SWAP_ENDIAN
      , PNG_TRANSFORM_INVERT_ALPHA
      , PNG_TRANSFORM_STRIP_FILLER
      , PNG_TRANSFORM_STRIP_FILLER_BEFORE
      , PNG_TRANSFORM_STRIP_FILLER_AFTER
      , PNG_TRANSFORM_GRAY_TO_RGB
      , PNG_TRANSFORM_EXPAND_16
      , PNG_TRANSFORM_SCALE_16
      , PNG_FLAG_MNG_EMPTY_PLTE
      , PNG_FLAG_MNG_FILTER_64
      , PNG_ALL_MNG_FEATURES
      , PNG_ERROR_ACTION_NONE
      , PNG_ERROR_ACTION_WARN
      , PNG_ERROR_ACTION_ERROR
      , PNG_ALPHA_PNG
      , PNG_ALPHA_STANDARD
      , PNG_ALPHA_ASSOCIATED
      , PNG_ALPHA_PREMULTIPLIED
      , PNG_ALPHA_OPTIMIZED
      , PNG_ALPHA_BROKEN
      , PNG_DEFAULT_sRGB
      , PNG_GAMMA_MAC_18
      , PNG_GAMMA_sRGB
      , PNG_GAMMA_LINEAR
      , PNG_FILLER_BEFORE
      , PNG_FILLER_AFTER
      , PNG_BACKGROUND_GAMMA_UNKNOWN
      , PNG_BACKGROUND_GAMMA_SCREEN
      , PNG_BACKGROUND_GAMMA_FILE
      , PNG_BACKGROUND_GAMMA_UNIQUE
      , PNG_CRC_DEFAULT
      , PNG_CRC_ERROR_QUIT
      , PNG_CRC_WARN_DISCARD
      , PNG_CRC_WARN_USE
      , PNG_CRC_QUIET_USE
      , PNG_CRC_NO_CHANGE
      , PNG_NO_FILTERS
      , PNG_FILTER_NONE
      , PNG_FILTER_SUB
      , PNG_FILTER_UP
      , PNG_FILTER_AVG
      , PNG_FILTER_PAETH
      , PNG_FILTER_VALUE_NONE
      , PNG_FILTER_VALUE_SUB
      , PNG_FILTER_VALUE_UP
      , PNG_FILTER_VALUE_AVG
      , PNG_FILTER_VALUE_PAETH
      , PNG_FILTER_VALUE_LAST
      , PNG_FILTER_HEURISTIC_DEFAULT
      , PNG_FILTER_HEURISTIC_UNWEIGHTED
      , PNG_FILTER_HEURISTIC_WEIGHTED
      , PNG_FILTER_HEURISTIC_LAST
      , PNG_DESTROY_WILL_FREE_DATA
      , PNG_SET_WILL_FREE_DATA
      , PNG_USER_WILL_FREE_DATA
      , PNG_FREE_HIST
      , PNG_FREE_ICCP
      , PNG_FREE_SPLT
      , PNG_FREE_ROWS
      , PNG_FREE_PCAL
      , PNG_FREE_SCAL
      , PNG_FREE_UNKN
      , PNG_FREE_PLTE
      , PNG_FREE_TRNS
      , PNG_FREE_TEXT
      , PNG_FREE_EXIF
      , PNG_FREE_ALL
      , PNG_FREE_MUL
      , PNG_HANDLE_CHUNK_AS_DEFAULT
      , PNG_HANDLE_CHUNK_NEVER
      , PNG_HANDLE_CHUNK_IF_SAFE
      , PNG_HANDLE_CHUNK_ALWAYS
      , PNG_HANDLE_CHUNK_LAST
      , PNG_IO_NONE
      , PNG_IO_READING
      , PNG_IO_WRITING
      , PNG_IO_SIGNATURE
      , PNG_IO_CHUNK_HDR
      , PNG_IO_CHUNK_DATA
      , PNG_IO_CHUNK_CRC
      , PNG_IO_MASK_OP
      , PNG_IO_MASK_LOC
      , PNG_INTERLACE_ADAM7_PASSES
      , PNG_IMAGE_VERSION
      , PNG_IMAGE_WARNING
      , PNG_IMAGE_ERROR
      , PNG_FORMAT_FLAG_ALPHA
      , PNG_FORMAT_FLAG_COLOR
      , PNG_FORMAT_FLAG_LINEAR
      , PNG_FORMAT_FLAG_COLORMAP
      , PNG_FORMAT_FLAG_BGR
      , PNG_FORMAT_FLAG_AFIRST
      , PNG_FORMAT_FLAG_ASSOCIATED_ALPHA
      , PNG_FORMAT_GRAY
      , PNG_FORMAT_GA
      , PNG_FORMAT_RGB
      , PNG_FORMAT_LINEAR_Y
      , PNG_IMAGE_FLAG_COLORSPACE_NOT_sRGB
      , PNG_IMAGE_FLAG_FAST
      , PNG_IMAGE_FLAG_16BIT_sRGB
      , PNG_MAXIMUM_INFLATE_WINDOW
      , PNG_SKIP_sRGB_CHECK_PROFILE
      , PNG_IGNORE_ADLER32
      , PNG_OPTION_NEXT
      , PNG_OPTION_UNSET
      , PNG_OPTION_INVALID
      , PNG_OPTION_OFF
      , PNG_OPTION_ON
     :: (Num a, Ord a) => a
pattern PNG_LIBPNG_VER_SONUM                 = #const PNG_LIBPNG_VER_SONUM
pattern PNG_LIBPNG_VER_DLLNUM                = #const PNG_LIBPNG_VER_DLLNUM
pattern PNG_LIBPNG_VER_MAJOR                 = #const PNG_LIBPNG_VER_MAJOR
pattern PNG_LIBPNG_VER_MINOR                 = #const PNG_LIBPNG_VER_MINOR
pattern PNG_LIBPNG_VER_RELEASE               = #const PNG_LIBPNG_VER_RELEASE
pattern PNG_LIBPNG_VER_BUILD                 = #const PNG_LIBPNG_VER_BUILD
pattern PNG_LIBPNG_BUILD_ALPHA               = #const PNG_LIBPNG_BUILD_ALPHA
pattern PNG_LIBPNG_BUILD_BETA                = #const PNG_LIBPNG_BUILD_BETA
pattern PNG_LIBPNG_BUILD_RC                  = #const PNG_LIBPNG_BUILD_RC
pattern PNG_LIBPNG_BUILD_STABLE              = #const PNG_LIBPNG_BUILD_STABLE
pattern PNG_LIBPNG_BUILD_RELEASE_STATUS_MASK = #const PNG_LIBPNG_BUILD_RELEASE_STATUS_MASK
pattern PNG_LIBPNG_BUILD_PATCH               = #const PNG_LIBPNG_BUILD_PATCH
pattern PNG_LIBPNG_BUILD_PRIVATE             = #const PNG_LIBPNG_BUILD_PRIVATE
pattern PNG_LIBPNG_BUILD_SPECIAL             = #const PNG_LIBPNG_BUILD_SPECIAL
pattern PNG_LIBPNG_BUILD_BASE_TYPE           = #const PNG_LIBPNG_BUILD_BASE_TYPE
pattern PNG_LIBPNG_VER                       = #const PNG_LIBPNG_VER
pattern PNG_TEXT_COMPRESSION_NONE_WR         = #const PNG_TEXT_COMPRESSION_NONE_WR
pattern PNG_TEXT_COMPRESSION_zTXt_WR         = #const PNG_TEXT_COMPRESSION_zTXt_WR
pattern PNG_TEXT_COMPRESSION_NONE            = #const PNG_TEXT_COMPRESSION_NONE
pattern PNG_TEXT_COMPRESSION_zTXt            = #const PNG_TEXT_COMPRESSION_zTXt
pattern PNG_ITXT_COMPRESSION_NONE            = #const PNG_ITXT_COMPRESSION_NONE
pattern PNG_ITXT_COMPRESSION_zTXt            = #const PNG_ITXT_COMPRESSION_zTXt
pattern PNG_TEXT_COMPRESSION_LAST            = #const PNG_TEXT_COMPRESSION_LAST
pattern PNG_HAVE_IHDR                        = #const PNG_HAVE_IHDR
pattern PNG_HAVE_PLTE                        = #const PNG_HAVE_PLTE
pattern PNG_AFTER_IDAT                       = #const PNG_AFTER_IDAT
pattern PNG_FP_1                             = #const PNG_FP_1
pattern PNG_FP_HALF                          = #const PNG_FP_HALF
pattern PNG_COLOR_MASK_PALETTE               = #const PNG_COLOR_MASK_PALETTE
pattern PNG_COLOR_MASK_COLOR                 = #const PNG_COLOR_MASK_COLOR
pattern PNG_COLOR_MASK_ALPHA                 = #const PNG_COLOR_MASK_ALPHA
pattern PNG_COLOR_TYPE_GRAY                  = #const PNG_COLOR_TYPE_GRAY
pattern PNG_COLOR_TYPE_RGBA                  = #const PNG_COLOR_TYPE_RGBA
pattern PNG_COLOR_TYPE_GA                    = #const PNG_COLOR_TYPE_GA
pattern PNG_COMPRESSION_TYPE_BASE            = #const PNG_COMPRESSION_TYPE_BASE
pattern PNG_COMPRESSION_TYPE_DEFAULT         = #const PNG_COMPRESSION_TYPE_DEFAULT
pattern PNG_FILTER_TYPE_BASE                 = #const PNG_FILTER_TYPE_BASE
pattern PNG_INTRAPIXEL_DIFFERENCING          = #const PNG_INTRAPIXEL_DIFFERENCING
pattern PNG_FILTER_TYPE_DEFAULT              = #const PNG_FILTER_TYPE_DEFAULT
pattern PNG_INTERLACE_NONE                   = #const PNG_INTERLACE_NONE
pattern PNG_INTERLACE_ADAM7                  = #const PNG_INTERLACE_ADAM7
pattern PNG_INTERLACE_LAST                   = #const PNG_INTERLACE_LAST
pattern PNG_OFFSET_PIXEL                     = #const PNG_OFFSET_PIXEL
pattern PNG_OFFSET_MICROMETER                = #const PNG_OFFSET_MICROMETER
pattern PNG_OFFSET_LAST                      = #const PNG_OFFSET_LAST
pattern PNG_EQUATION_LINEAR                  = #const PNG_EQUATION_LINEAR
pattern PNG_EQUATION_BASE_E                  = #const PNG_EQUATION_BASE_E
pattern PNG_EQUATION_ARBITRARY               = #const PNG_EQUATION_ARBITRARY
pattern PNG_EQUATION_HYPERBOLIC              = #const PNG_EQUATION_HYPERBOLIC
pattern PNG_EQUATION_LAST                    = #const PNG_EQUATION_LAST
pattern PNG_SCALE_UNKNOWN                    = #const PNG_SCALE_UNKNOWN
pattern PNG_SCALE_METER                      = #const PNG_SCALE_METER
pattern PNG_SCALE_RADIAN                     = #const PNG_SCALE_RADIAN
pattern PNG_SCALE_LAST                       = #const PNG_SCALE_LAST
pattern PNG_RESOLUTION_UNKNOWN               = #const PNG_RESOLUTION_UNKNOWN
pattern PNG_RESOLUTION_METER                 = #const PNG_RESOLUTION_METER
pattern PNG_RESOLUTION_LAST                  = #const PNG_RESOLUTION_LAST
pattern PNG_sRGB_INTENT_PERCEPTUAL           = #const PNG_sRGB_INTENT_PERCEPTUAL
pattern PNG_sRGB_INTENT_RELATIVE             = #const PNG_sRGB_INTENT_RELATIVE
pattern PNG_sRGB_INTENT_SATURATION           = #const PNG_sRGB_INTENT_SATURATION
pattern PNG_sRGB_INTENT_ABSOLUTE             = #const PNG_sRGB_INTENT_ABSOLUTE
pattern PNG_sRGB_INTENT_LAST                 = #const PNG_sRGB_INTENT_LAST
pattern PNG_KEYWORD_MAX_LENGTH               = #const PNG_KEYWORD_MAX_LENGTH
pattern PNG_MAX_PALETTE_LENGTH               = #const PNG_MAX_PALETTE_LENGTH
pattern PNG_INFO_gAMA                        = #const PNG_INFO_gAMA
pattern PNG_INFO_sBIT                        = #const PNG_INFO_sBIT
pattern PNG_INFO_cHRM                        = #const PNG_INFO_cHRM
pattern PNG_INFO_PLTE                        = #const PNG_INFO_PLTE
pattern PNG_INFO_tRNS                        = #const PNG_INFO_tRNS
pattern PNG_INFO_bKGD                        = #const PNG_INFO_bKGD
pattern PNG_INFO_hIST                        = #const PNG_INFO_hIST
pattern PNG_INFO_pHYs                        = #const PNG_INFO_pHYs
pattern PNG_INFO_oFFs                        = #const PNG_INFO_oFFs
pattern PNG_INFO_tIME                        = #const PNG_INFO_tIME
pattern PNG_INFO_pCAL                        = #const PNG_INFO_pCAL
pattern PNG_INFO_sRGB                        = #const PNG_INFO_sRGB
pattern PNG_INFO_iCCP                        = #const PNG_INFO_iCCP
pattern PNG_INFO_sPLT                        = #const PNG_INFO_sPLT
pattern PNG_INFO_sCAL                        = #const PNG_INFO_sCAL
pattern PNG_INFO_IDAT                        = #const PNG_INFO_IDAT
pattern PNG_INFO_eXIf                        = #const PNG_INFO_eXIf
pattern PNG_TRANSFORM_IDENTITY               = #const PNG_TRANSFORM_IDENTITY
pattern PNG_TRANSFORM_STRIP_16               = #const PNG_TRANSFORM_STRIP_16
pattern PNG_TRANSFORM_STRIP_ALPHA            = #const PNG_TRANSFORM_STRIP_ALPHA
pattern PNG_TRANSFORM_PACKING                = #const PNG_TRANSFORM_PACKING
pattern PNG_TRANSFORM_PACKSWAP               = #const PNG_TRANSFORM_PACKSWAP
pattern PNG_TRANSFORM_EXPAND                 = #const PNG_TRANSFORM_EXPAND
pattern PNG_TRANSFORM_INVERT_MONO            = #const PNG_TRANSFORM_INVERT_MONO
pattern PNG_TRANSFORM_SHIFT                  = #const PNG_TRANSFORM_SHIFT
pattern PNG_TRANSFORM_BGR                    = #const PNG_TRANSFORM_BGR
pattern PNG_TRANSFORM_SWAP_ALPHA             = #const PNG_TRANSFORM_SWAP_ALPHA
pattern PNG_TRANSFORM_SWAP_ENDIAN            = #const PNG_TRANSFORM_SWAP_ENDIAN
pattern PNG_TRANSFORM_INVERT_ALPHA           = #const PNG_TRANSFORM_INVERT_ALPHA
pattern PNG_TRANSFORM_STRIP_FILLER           = #const PNG_TRANSFORM_STRIP_FILLER
pattern PNG_TRANSFORM_STRIP_FILLER_BEFORE    = #const PNG_TRANSFORM_STRIP_FILLER_BEFORE
pattern PNG_TRANSFORM_STRIP_FILLER_AFTER     = #const PNG_TRANSFORM_STRIP_FILLER_AFTER
pattern PNG_TRANSFORM_GRAY_TO_RGB            = #const PNG_TRANSFORM_GRAY_TO_RGB
pattern PNG_TRANSFORM_EXPAND_16              = #const PNG_TRANSFORM_EXPAND_16
pattern PNG_TRANSFORM_SCALE_16               = #const PNG_TRANSFORM_SCALE_16
pattern PNG_FLAG_MNG_EMPTY_PLTE              = #const PNG_FLAG_MNG_EMPTY_PLTE
pattern PNG_FLAG_MNG_FILTER_64               = #const PNG_FLAG_MNG_FILTER_64
pattern PNG_ALL_MNG_FEATURES                 = #const PNG_ALL_MNG_FEATURES
pattern PNG_ERROR_ACTION_NONE                = #const PNG_ERROR_ACTION_NONE
pattern PNG_ERROR_ACTION_WARN                = #const PNG_ERROR_ACTION_WARN
pattern PNG_ERROR_ACTION_ERROR               = #const PNG_ERROR_ACTION_ERROR
pattern PNG_ALPHA_PNG                        = #const PNG_ALPHA_PNG
pattern PNG_ALPHA_STANDARD                   = #const PNG_ALPHA_STANDARD
pattern PNG_ALPHA_ASSOCIATED                 = #const PNG_ALPHA_ASSOCIATED
pattern PNG_ALPHA_PREMULTIPLIED              = #const PNG_ALPHA_PREMULTIPLIED
pattern PNG_ALPHA_OPTIMIZED                  = #const PNG_ALPHA_OPTIMIZED
pattern PNG_ALPHA_BROKEN                     = #const PNG_ALPHA_BROKEN
pattern PNG_DEFAULT_sRGB                     = #const PNG_DEFAULT_sRGB
pattern PNG_GAMMA_MAC_18                     = #const PNG_GAMMA_MAC_18
pattern PNG_GAMMA_sRGB                       = #const PNG_GAMMA_sRGB
pattern PNG_GAMMA_LINEAR                     = #const PNG_GAMMA_LINEAR
pattern PNG_FILLER_BEFORE                    = #const PNG_FILLER_BEFORE
pattern PNG_FILLER_AFTER                     = #const PNG_FILLER_AFTER
pattern PNG_BACKGROUND_GAMMA_UNKNOWN         = #const PNG_BACKGROUND_GAMMA_UNKNOWN
pattern PNG_BACKGROUND_GAMMA_SCREEN          = #const PNG_BACKGROUND_GAMMA_SCREEN
pattern PNG_BACKGROUND_GAMMA_FILE            = #const PNG_BACKGROUND_GAMMA_FILE
pattern PNG_BACKGROUND_GAMMA_UNIQUE          = #const PNG_BACKGROUND_GAMMA_UNIQUE
pattern PNG_CRC_DEFAULT                      = #const PNG_CRC_DEFAULT
pattern PNG_CRC_ERROR_QUIT                   = #const PNG_CRC_ERROR_QUIT
pattern PNG_CRC_WARN_DISCARD                 = #const PNG_CRC_WARN_DISCARD
pattern PNG_CRC_WARN_USE                     = #const PNG_CRC_WARN_USE
pattern PNG_CRC_QUIET_USE                    = #const PNG_CRC_QUIET_USE
pattern PNG_CRC_NO_CHANGE                    = #const PNG_CRC_NO_CHANGE
pattern PNG_NO_FILTERS                       = #const PNG_NO_FILTERS
pattern PNG_FILTER_NONE                      = #const PNG_FILTER_NONE
pattern PNG_FILTER_SUB                       = #const PNG_FILTER_SUB
pattern PNG_FILTER_UP                        = #const PNG_FILTER_UP
pattern PNG_FILTER_AVG                       = #const PNG_FILTER_AVG
pattern PNG_FILTER_PAETH                     = #const PNG_FILTER_PAETH
pattern PNG_FILTER_VALUE_NONE                = #const PNG_FILTER_VALUE_NONE
pattern PNG_FILTER_VALUE_SUB                 = #const PNG_FILTER_VALUE_SUB
pattern PNG_FILTER_VALUE_UP                  = #const PNG_FILTER_VALUE_UP
pattern PNG_FILTER_VALUE_AVG                 = #const PNG_FILTER_VALUE_AVG
pattern PNG_FILTER_VALUE_PAETH               = #const PNG_FILTER_VALUE_PAETH
pattern PNG_FILTER_VALUE_LAST                = #const PNG_FILTER_VALUE_LAST
pattern PNG_FILTER_HEURISTIC_DEFAULT         = #const PNG_FILTER_HEURISTIC_DEFAULT
pattern PNG_FILTER_HEURISTIC_UNWEIGHTED      = #const PNG_FILTER_HEURISTIC_UNWEIGHTED
pattern PNG_FILTER_HEURISTIC_WEIGHTED        = #const PNG_FILTER_HEURISTIC_WEIGHTED
pattern PNG_FILTER_HEURISTIC_LAST            = #const PNG_FILTER_HEURISTIC_LAST
pattern PNG_DESTROY_WILL_FREE_DATA           = #const PNG_DESTROY_WILL_FREE_DATA
pattern PNG_SET_WILL_FREE_DATA               = #const PNG_SET_WILL_FREE_DATA
pattern PNG_USER_WILL_FREE_DATA              = #const PNG_USER_WILL_FREE_DATA
pattern PNG_FREE_HIST                        = #const PNG_FREE_HIST
pattern PNG_FREE_ICCP                        = #const PNG_FREE_ICCP
pattern PNG_FREE_SPLT                        = #const PNG_FREE_SPLT
pattern PNG_FREE_ROWS                        = #const PNG_FREE_ROWS
pattern PNG_FREE_PCAL                        = #const PNG_FREE_PCAL
pattern PNG_FREE_SCAL                        = #const PNG_FREE_SCAL
pattern PNG_FREE_UNKN                        = #const PNG_FREE_UNKN
pattern PNG_FREE_PLTE                        = #const PNG_FREE_PLTE
pattern PNG_FREE_TRNS                        = #const PNG_FREE_TRNS
pattern PNG_FREE_TEXT                        = #const PNG_FREE_TEXT
pattern PNG_FREE_EXIF                        = #const PNG_FREE_EXIF
pattern PNG_FREE_ALL                         = #const PNG_FREE_ALL
pattern PNG_FREE_MUL                         = #const PNG_FREE_MUL
pattern PNG_HANDLE_CHUNK_AS_DEFAULT          = #const PNG_HANDLE_CHUNK_AS_DEFAULT
pattern PNG_HANDLE_CHUNK_NEVER               = #const PNG_HANDLE_CHUNK_NEVER
pattern PNG_HANDLE_CHUNK_IF_SAFE             = #const PNG_HANDLE_CHUNK_IF_SAFE
pattern PNG_HANDLE_CHUNK_ALWAYS              = #const PNG_HANDLE_CHUNK_ALWAYS
pattern PNG_HANDLE_CHUNK_LAST                = #const PNG_HANDLE_CHUNK_LAST
pattern PNG_IO_NONE                          = #const PNG_IO_NONE
pattern PNG_IO_READING                       = #const PNG_IO_READING
pattern PNG_IO_WRITING                       = #const PNG_IO_WRITING
pattern PNG_IO_SIGNATURE                     = #const PNG_IO_SIGNATURE
pattern PNG_IO_CHUNK_HDR                     = #const PNG_IO_CHUNK_HDR
pattern PNG_IO_CHUNK_DATA                    = #const PNG_IO_CHUNK_DATA
pattern PNG_IO_CHUNK_CRC                     = #const PNG_IO_CHUNK_CRC
pattern PNG_IO_MASK_OP                       = #const PNG_IO_MASK_OP
pattern PNG_IO_MASK_LOC                      = #const PNG_IO_MASK_LOC
pattern PNG_INTERLACE_ADAM7_PASSES           = #const PNG_INTERLACE_ADAM7_PASSES
pattern PNG_IMAGE_VERSION                    = #const PNG_IMAGE_VERSION
pattern PNG_IMAGE_WARNING                    = #const PNG_IMAGE_WARNING
pattern PNG_IMAGE_ERROR                      = #const PNG_IMAGE_ERROR
pattern PNG_FORMAT_FLAG_ALPHA                = #const PNG_FORMAT_FLAG_ALPHA
pattern PNG_FORMAT_FLAG_COLOR                = #const PNG_FORMAT_FLAG_COLOR
pattern PNG_FORMAT_FLAG_LINEAR               = #const PNG_FORMAT_FLAG_LINEAR
pattern PNG_FORMAT_FLAG_COLORMAP             = #const PNG_FORMAT_FLAG_COLORMAP
pattern PNG_FORMAT_FLAG_BGR                  = #const PNG_FORMAT_FLAG_BGR
pattern PNG_FORMAT_FLAG_AFIRST               = #const PNG_FORMAT_FLAG_AFIRST
pattern PNG_FORMAT_FLAG_ASSOCIATED_ALPHA     = #const PNG_FORMAT_FLAG_ASSOCIATED_ALPHA
pattern PNG_FORMAT_GRAY                      = #const PNG_FORMAT_GRAY
pattern PNG_FORMAT_GA                        = #const PNG_FORMAT_GA
pattern PNG_FORMAT_RGB                       = #const PNG_FORMAT_RGB
pattern PNG_FORMAT_LINEAR_Y                  = #const PNG_FORMAT_LINEAR_Y
pattern PNG_IMAGE_FLAG_COLORSPACE_NOT_sRGB   = #const PNG_IMAGE_FLAG_COLORSPACE_NOT_sRGB
pattern PNG_IMAGE_FLAG_FAST                  = #const PNG_IMAGE_FLAG_FAST
pattern PNG_IMAGE_FLAG_16BIT_sRGB            = #const PNG_IMAGE_FLAG_16BIT_sRGB
pattern PNG_MAXIMUM_INFLATE_WINDOW           = #const PNG_MAXIMUM_INFLATE_WINDOW
pattern PNG_SKIP_sRGB_CHECK_PROFILE          = #const PNG_SKIP_sRGB_CHECK_PROFILE
pattern PNG_IGNORE_ADLER32                   = #const PNG_IGNORE_ADLER32
pattern PNG_OPTION_NEXT                      = #const PNG_OPTION_NEXT
pattern PNG_OPTION_UNSET                     = #const PNG_OPTION_UNSET
pattern PNG_OPTION_INVALID                   = #const PNG_OPTION_INVALID
pattern PNG_OPTION_OFF                       = #const PNG_OPTION_OFF
pattern PNG_OPTION_ON                        = #const PNG_OPTION_ON


type PngError = Ptr PngStructDef -> ConstPtr #{type char} -> IO ()

foreign import ccall "wrapper"
  mkPngError :: PngError -> IO (FunPtr PngError)

type PngRw = Ptr PngStructDef -> Ptr #{type png_byte} -> #{type size_t} -> IO ()

foreign import ccall "wrapper"
  mkPngRw :: PngRw -> IO (FunPtr PngRw)

type PngFlush = Ptr PngStructDef -> IO ()

foreign import ccall "wrapper"
  mkPngFlush :: PngFlush -> IO (FunPtr PngFlush)

type PngReadStatus = Ptr PngStructDef -> #{type png_uint_32} -> #{type int} -> IO ()

foreign import ccall "wrapper"
  mkPngReadStatus :: PngReadStatus -> IO (FunPtr PngReadStatus)

type PngWriteStatus = Ptr PngStructDef -> #{type png_uint_32} -> #{type int} -> IO ()

foreign import ccall "wrapper"
  mkPngWriteStatus :: PngWriteStatus -> IO (FunPtr PngWriteStatus)

type PngProgressiveInfo = Ptr PngStructDef -> Ptr PngInfoDef -> IO ()

foreign import ccall "wrapper"
  mkPngProgressiveInfo :: PngProgressiveInfo -> IO (FunPtr PngProgressiveInfo)

type PngProgressiveEnd = Ptr PngStructDef -> Ptr PngInfoDef -> IO ()

foreign import ccall "wrapper"
  mkPngProgressiveEnd :: PngProgressiveEnd -> IO (FunPtr PngProgressiveEnd)

type PngProgressiveRow = Ptr PngStructDef -> Ptr #{type png_byte} -> #{type png_uint_32} -> #{type int} -> IO ()

foreign import ccall "wrapper"
  mkPngProgressiveRow :: PngProgressiveRow -> IO (FunPtr PngProgressiveRow)

type PngUserTransform = Ptr PngStructDef -> Ptr PngRowInfoStruct -> Ptr #{type png_byte} -> IO ()

foreign import ccall "wrapper"
  mkPngUserTransform :: PngUserTransform -> IO (FunPtr PngUserTransform)

type PngUserChunk = Ptr PngStructDef -> Ptr PngUnknownChunkT -> IO #{type int}

foreign import ccall "wrapper"
  mkPngUserChunk :: PngUserChunk -> IO (FunPtr PngUserChunk)

type PngMalloc = Ptr PngStructDef -> #{type png_alloc_size_t} -> IO (Ptr ())

foreign import ccall "wrapper"
  mkPngMalloc :: PngMalloc -> IO (FunPtr PngMalloc)

type PngFree = Ptr PngStructDef -> Ptr () -> IO ()

foreign import ccall "wrapper"
  mkPngFree :: PngFree -> IO (FunPtr PngFree)


data PngStructDef

data PngInfoDef

data PngColorStruct =
       PngColorStruct
         { pcsRed :: #{type png_byte}
         , pcsGreen :: #{type png_byte}
         , pcsBlue :: #{type png_byte}
         } 

instance Storable PngColorStruct where
  sizeOf _    = #size png_color
  alignment _ = #alignment png_color

  peek ptr = PngColorStruct
               <$> #{peek png_color, red} ptr
               <*> #{peek png_color, green} ptr
               <*> #{peek png_color, blue} ptr

  poke ptr val = do
    #{poke png_color, red} ptr $ pcsRed val
    #{poke png_color, green} ptr $ pcsGreen val
    #{poke png_color, blue} ptr $ pcsBlue val

data PngColor16Struct =
       PngColor16Struct
         { pc16sIndex :: #{type png_byte}
         , pc16sRed :: #{type png_uint_16}
         , pc16sGreen :: #{type png_uint_16}
         , pc16sBlue :: #{type png_uint_16}
         , pc16sGray :: #{type png_uint_16}
         } 

instance Storable PngColor16Struct where
  sizeOf _    = #size png_color_16
  alignment _ = #alignment png_color_16

  peek ptr = PngColor16Struct
               <$> #{peek png_color_16, index} ptr
               <*> #{peek png_color_16, red} ptr
               <*> #{peek png_color_16, green} ptr
               <*> #{peek png_color_16, blue} ptr
               <*> #{peek png_color_16, gray} ptr

  poke ptr val = do
    #{poke png_color_16, index} ptr $ pc16sIndex val
    #{poke png_color_16, red} ptr $ pc16sRed val
    #{poke png_color_16, green} ptr $ pc16sGreen val
    #{poke png_color_16, blue} ptr $ pc16sBlue val
    #{poke png_color_16, gray} ptr $ pc16sGray val

data PngColor8Struct =
       PngColor8Struct
         { pc8sRed :: #{type png_byte}
         , pc8sGreen :: #{type png_byte}
         , pc8sBlue :: #{type png_byte}
         , pc8sGray :: #{type png_byte}
         , pc8sAlpha :: #{type png_byte}
         } 

instance Storable PngColor8Struct where
  sizeOf _    = #size png_color_8
  alignment _ = #alignment png_color_8

  peek ptr = PngColor8Struct
               <$> #{peek png_color_8, red} ptr
               <*> #{peek png_color_8, green} ptr
               <*> #{peek png_color_8, blue} ptr
               <*> #{peek png_color_8, gray} ptr
               <*> #{peek png_color_8, alpha} ptr

  poke ptr val = do
    #{poke png_color_8, red} ptr $ pc8sRed val
    #{poke png_color_8, green} ptr $ pc8sGreen val
    #{poke png_color_8, blue} ptr $ pc8sBlue val
    #{poke png_color_8, gray} ptr $ pc8sGray val
    #{poke png_color_8, alpha} ptr $ pc8sAlpha val

data PngSPLTEntryStruct =
       PngSPLTEntryStruct
         { psesRed :: #{type png_uint_16}
         , psesGreen :: #{type png_uint_16}
         , psesBlue :: #{type png_uint_16}
         , psesAlpha :: #{type png_uint_16}
         , psesFrequency :: #{type png_uint_16}
         } 

instance Storable PngSPLTEntryStruct where
  sizeOf _    = #size png_sPLT_entry
  alignment _ = #alignment png_sPLT_entry

  peek ptr = PngSPLTEntryStruct
               <$> #{peek png_sPLT_entry, red} ptr
               <*> #{peek png_sPLT_entry, green} ptr
               <*> #{peek png_sPLT_entry, blue} ptr
               <*> #{peek png_sPLT_entry, alpha} ptr
               <*> #{peek png_sPLT_entry, frequency} ptr

  poke ptr val = do
    #{poke png_sPLT_entry, red} ptr $ psesRed val
    #{poke png_sPLT_entry, green} ptr $ psesGreen val
    #{poke png_sPLT_entry, blue} ptr $ psesBlue val
    #{poke png_sPLT_entry, alpha} ptr $ psesAlpha val
    #{poke png_sPLT_entry, frequency} ptr $ psesFrequency val

data PngSPLTStruct =
       PngSPLTStruct
         { pssName :: Ptr #{type char}
         , pssDepth :: #{type png_byte}
         , pssEntries :: Ptr PngSPLTEntryStruct
         , pssNentries :: #{type png_int_32}
         } 

instance Storable PngSPLTStruct where
  sizeOf _    = #size png_sPLT_t
  alignment _ = #alignment png_sPLT_t

  peek ptr = PngSPLTStruct
               <$> #{peek png_sPLT_t, name} ptr
               <*> #{peek png_sPLT_t, depth} ptr
               <*> #{peek png_sPLT_t, entries} ptr
               <*> #{peek png_sPLT_t, nentries} ptr

  poke ptr val = do
    #{poke png_sPLT_t, name} ptr $ pssName val
    #{poke png_sPLT_t, depth} ptr $ pssDepth val
    #{poke png_sPLT_t, entries} ptr $ pssEntries val
    #{poke png_sPLT_t, nentries} ptr $ pssNentries val

data PngTextStruct =
       PngTextStruct
         { ptsCompression :: #{type int}
         , ptsKey :: Ptr #{type char}
         , ptsText :: Ptr #{type char}
         , ptsTextLength :: #{type size_t}
         , ptsItxtLength :: #{type size_t}
         , ptsLang :: Ptr #{type char}
         , ptsLangKey :: Ptr #{type char}
         } 

instance Storable PngTextStruct where
  sizeOf _    = #size png_text
  alignment _ = #alignment png_text

  peek ptr = PngTextStruct
               <$> #{peek png_text, compression} ptr
               <*> #{peek png_text, key} ptr
               <*> #{peek png_text, text} ptr
               <*> #{peek png_text, text_length} ptr
               <*> #{peek png_text, itxt_length} ptr
               <*> #{peek png_text, lang} ptr
               <*> #{peek png_text, lang_key} ptr

  poke ptr val = do
    #{poke png_text, compression} ptr $ ptsCompression val
    #{poke png_text, key} ptr $ ptsKey val
    #{poke png_text, text} ptr $ ptsText val
    #{poke png_text, text_length} ptr $ ptsTextLength val
    #{poke png_text, itxt_length} ptr $ ptsItxtLength val
    #{poke png_text, lang} ptr $ ptsLang val
    #{poke png_text, lang_key} ptr $ ptsLangKey val

data PngTimeStruct =
       PngTimeStruct
         { ptsYear :: #{type png_uint_16}
         , ptsMonth :: #{type png_byte}
         , ptsDay :: #{type png_byte}
         , ptsHour :: #{type png_byte}
         , ptsMinute :: #{type png_byte}
         , ptsSecond :: #{type png_byte}
         } 

instance Storable PngTimeStruct where
  sizeOf _    = #size png_time
  alignment _ = #alignment png_time

  peek ptr = PngTimeStruct
               <$> #{peek png_time, year} ptr
               <*> #{peek png_time, month} ptr
               <*> #{peek png_time, day} ptr
               <*> #{peek png_time, hour} ptr
               <*> #{peek png_time, minute} ptr
               <*> #{peek png_time, second} ptr

  poke ptr val = do
    #{poke png_time, year} ptr $ ptsYear val
    #{poke png_time, month} ptr $ ptsMonth val
    #{poke png_time, day} ptr $ ptsDay val
    #{poke png_time, hour} ptr $ ptsHour val
    #{poke png_time, minute} ptr $ ptsMinute val
    #{poke png_time, second} ptr $ ptsSecond val

data PngUnknownChunkT =
       PngUnknownChunkT
         { puctName :: Ptr #{type png_byte}
         , puctData :: Ptr #{type png_byte}
         , puctSize :: #{type size_t}
         , puctLocation :: #{type png_byte}
         } 

instance Storable PngUnknownChunkT where
  sizeOf _    = #size png_unknown_chunk
  alignment _ = #alignment png_unknown_chunk

  peek ptr = PngUnknownChunkT
               <$> #{peek png_unknown_chunk, name} ptr
               <*> #{peek png_unknown_chunk, data} ptr
               <*> #{peek png_unknown_chunk, size} ptr
               <*> #{peek png_unknown_chunk, location} ptr

  poke ptr val = do
    #{poke png_unknown_chunk, name} ptr $ puctName val
    #{poke png_unknown_chunk, data} ptr $ puctData val
    #{poke png_unknown_chunk, size} ptr $ puctSize val
    #{poke png_unknown_chunk, location} ptr $ puctLocation val

data PngRowInfoStruct =
       PngRowInfoStruct
         { prisWidth :: #{type png_uint_32}
         , prisRowbytes :: #{type size_t}
         , prisColorType :: #{type png_byte}
         , prisBitDepth :: #{type png_byte}
         , prisChannels :: #{type png_byte}
         , prisPixelDepth :: #{type png_byte}
         } 

instance Storable PngRowInfoStruct where
  sizeOf _    = #size png_row_info
  alignment _ = #alignment png_row_info

  peek ptr = PngRowInfoStruct
               <$> #{peek png_row_info, width} ptr
               <*> #{peek png_row_info, rowbytes} ptr
               <*> #{peek png_row_info, color_type} ptr
               <*> #{peek png_row_info, bit_depth} ptr
               <*> #{peek png_row_info, channels} ptr
               <*> #{peek png_row_info, pixel_depth} ptr

  poke ptr val = do
    #{poke png_row_info, width} ptr $ prisWidth val
    #{poke png_row_info, rowbytes} ptr $ prisRowbytes val
    #{poke png_row_info, color_type} ptr $ prisColorType val
    #{poke png_row_info, bit_depth} ptr $ prisBitDepth val
    #{poke png_row_info, channels} ptr $ prisChannels val
    #{poke png_row_info, pixel_depth} ptr $ prisPixelDepth val

data PngControl

data PngImageStruct =
       PngImageStruct
         { pisOpaque :: Ptr PngControl
         , pisVersion :: #{type png_uint_32}
         , pisWidth :: #{type png_uint_32}
         , pisHeight :: #{type png_uint_32}
         , pisFormat :: #{type png_uint_32}
         , pisFlags :: #{type png_uint_32}
         , pisColormapEntries :: #{type png_uint_32}
         , pisWarningOrError :: #{type png_uint_32}
         , pisMessage :: Ptr #{type char}
         } 

instance Storable PngImageStruct where
  sizeOf _    = #size png_image
  alignment _ = #alignment png_image

  peek ptr = PngImageStruct
               <$> #{peek png_image, opaque} ptr
               <*> #{peek png_image, version} ptr
               <*> #{peek png_image, width} ptr
               <*> #{peek png_image, height} ptr
               <*> #{peek png_image, format} ptr
               <*> #{peek png_image, flags} ptr
               <*> #{peek png_image, colormap_entries} ptr
               <*> #{peek png_image, warning_or_error} ptr
               <*> #{peek png_image, message} ptr

  poke ptr val = do
    #{poke png_image, opaque} ptr $ pisOpaque val
    #{poke png_image, version} ptr $ pisVersion val
    #{poke png_image, width} ptr $ pisWidth val
    #{poke png_image, height} ptr $ pisHeight val
    #{poke png_image, format} ptr $ pisFormat val
    #{poke png_image, flags} ptr $ pisFlags val
    #{poke png_image, colormap_entries} ptr $ pisColormapEntries val
    #{poke png_image, warning_or_error} ptr $ pisWarningOrError val
    #{poke png_image, message} ptr $ pisMessage val


foreign import ccall "png_access_version_number"
  png_access_version_number :: IO #{type png_uint_32}

foreign import ccall "png_set_sig_bytes"
  png_set_sig_bytes
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ num_bytes
    -> IO ()

foreign import ccall "png_sig_cmp"
  png_sig_cmp
    :: ConstPtr #{type png_byte} -- ^ sig
    -> #{type size_t} -- ^ start
    -> #{type size_t} -- ^ num_to_check
    -> IO #{type int}

foreign import ccall "png_create_read_struct"
  png_create_read_struct
    :: ConstPtr #{type char} -- ^ user_png_ver
    -> Ptr () -- ^ error_ptr
    -> FunPtr PngError -- ^ error_fn
    -> FunPtr PngError -- ^ warn_fn
    -> IO (Ptr PngStructDef)

foreign import ccall "png_create_write_struct"
  png_create_write_struct
    :: ConstPtr #{type char} -- ^ user_png_ver
    -> Ptr () -- ^ error_ptr
    -> FunPtr PngError -- ^ error_fn
    -> FunPtr PngError -- ^ warn_fn
    -> IO (Ptr PngStructDef)

foreign import ccall "png_get_compression_buffer_size"
  png_get_compression_buffer_size
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type size_t}

foreign import ccall "png_set_compression_buffer_size"
  png_set_compression_buffer_size
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type size_t} -- ^ size
    -> IO ()

foreign import ccall "png_set_longjmp_fn"
  png_set_longjmp_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr CJmpBuf -- ^ longjmp_fn
    -> #{type size_t} -- ^ jmp_buf_size
    -> IO (Ptr CJmpBuf)

foreign import ccall "png_longjmp"
  png_longjmp
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ val
    -> IO ()

foreign import ccall "png_reset_zstream"
  png_reset_zstream
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO #{type int}

foreign import ccall "png_create_read_struct_2"
  png_create_read_struct_2
    :: ConstPtr #{type char} -- ^ user_png_ver
    -> Ptr () -- ^ error_ptr
    -> FunPtr PngError -- ^ error_fn
    -> FunPtr PngError -- ^ warn_fn
    -> Ptr () -- ^ mem_ptr
    -> FunPtr PngMalloc -- ^ malloc_fn
    -> FunPtr PngFree -- ^ free_fn
    -> IO (Ptr PngStructDef)

foreign import ccall "png_create_write_struct_2"
  png_create_write_struct_2
    :: ConstPtr #{type char} -- ^ user_png_ver
    -> Ptr () -- ^ error_ptr
    -> FunPtr PngError -- ^ error_fn
    -> FunPtr PngError -- ^ warn_fn
    -> Ptr () -- ^ mem_ptr
    -> FunPtr PngMalloc -- ^ malloc_fn
    -> FunPtr PngFree -- ^ free_fn
    -> IO (Ptr PngStructDef)

foreign import ccall "png_write_sig"
  png_write_sig
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_write_chunk"
  png_write_chunk
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type png_byte} -- ^ chunk_name
    -> ConstPtr #{type png_byte} -- ^ data
    -> #{type size_t} -- ^ length
    -> IO ()

foreign import ccall "png_write_chunk_start"
  png_write_chunk_start
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type png_byte} -- ^ chunk_name
    -> #{type png_uint_32} -- ^ length
    -> IO ()

foreign import ccall "png_write_chunk_data"
  png_write_chunk_data
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type png_byte} -- ^ data
    -> #{type size_t} -- ^ length
    -> IO ()

foreign import ccall "png_write_chunk_end"
  png_write_chunk_end
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_create_info_struct"
  png_create_info_struct
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr PngInfoDef)

foreign import ccall "png_info_init_3"
  png_info_init_3
    :: Ptr (Ptr PngInfoDef) -- ^ info_ptr
    -> #{type size_t} -- ^ png_info_struct_size
    -> IO ()

foreign import ccall "png_write_info_before_PLTE"
  png_write_info_before_PLTE
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO ()

foreign import ccall "png_write_info"
  png_write_info
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO ()

foreign import ccall "png_read_info"
  png_read_info
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> IO ()

foreign import ccall "png_convert_to_rfc1123"
  png_convert_to_rfc1123
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr PngTimeStruct -- ^ ptime
    -> IO (ConstPtr #{type char})

foreign import ccall "png_convert_to_rfc1123_buffer"
  png_convert_to_rfc1123_buffer
    :: Ptr #{type char} -- ^ out
    -> ConstPtr PngTimeStruct -- ^ ptime
    -> IO #{type int}

foreign import ccall "png_convert_from_struct_tm"
  png_convert_from_struct_tm
    :: Ptr PngTimeStruct -- ^ ptime
    -> ConstPtr CTm -- ^ ttime
    -> IO ()

foreign import ccall "png_convert_from_time_t"
  png_convert_from_time_t
    :: Ptr PngTimeStruct -- ^ ptime
    -> #{type time_t} -- ^ ttime
    -> IO ()

foreign import ccall "png_set_expand"
  png_set_expand
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_expand_gray_1_2_4_to_8"
  png_set_expand_gray_1_2_4_to_8
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_palette_to_rgb"
  png_set_palette_to_rgb
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_tRNS_to_alpha"
  png_set_tRNS_to_alpha
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_expand_16"
  png_set_expand_16
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_bgr"
  png_set_bgr
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_gray_to_rgb"
  png_set_gray_to_rgb
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_rgb_to_gray"
  png_set_rgb_to_gray
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ error_action
    -> #{type double} -- ^ red
    -> #{type double} -- ^ green
    -> IO ()

foreign import ccall "png_set_rgb_to_gray_fixed"
  png_set_rgb_to_gray_fixed
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ error_action
    -> #{type png_fixed_point} -- ^ red
    -> #{type png_fixed_point} -- ^ green
    -> IO ()

foreign import ccall "png_get_rgb_to_gray_status"
  png_get_rgb_to_gray_status
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_byte}

foreign import ccall "png_build_grayscale_palette"
  png_build_grayscale_palette
    :: #{type int} -- ^ bit_depth
    -> Ptr PngColorStruct -- ^ palette
    -> IO ()

foreign import ccall "png_set_alpha_mode"
  png_set_alpha_mode
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ mode
    -> #{type double} -- ^ output_gamma
    -> IO ()

foreign import ccall "png_set_alpha_mode_fixed"
  png_set_alpha_mode_fixed
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ mode
    -> #{type png_fixed_point} -- ^ output_gamma
    -> IO ()

foreign import ccall "png_set_strip_alpha"
  png_set_strip_alpha
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_swap_alpha"
  png_set_swap_alpha
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_invert_alpha"
  png_set_invert_alpha
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_filler"
  png_set_filler
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_uint_32} -- ^ filler
    -> #{type int} -- ^ flags
    -> IO ()

foreign import ccall "png_set_add_alpha"
  png_set_add_alpha
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_uint_32} -- ^ filler
    -> #{type int} -- ^ flags
    -> IO ()

foreign import ccall "png_set_swap"
  png_set_swap
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_packing"
  png_set_packing
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_packswap"
  png_set_packswap
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_shift"
  png_set_shift
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr PngColor8Struct -- ^ true_bits
    -> IO ()

foreign import ccall "png_set_interlace_handling"
  png_set_interlace_handling
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO #{type int}

foreign import ccall "png_set_invert_mono"
  png_set_invert_mono
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_background"
  png_set_background
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr PngColor16Struct -- ^ background_color
    -> #{type int} -- ^ background_gamma_code
    -> #{type int} -- ^ need_expand
    -> #{type double} -- ^ background_gamma
    -> IO ()

foreign import ccall "png_set_background_fixed"
  png_set_background_fixed
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr PngColor16Struct -- ^ background_color
    -> #{type int} -- ^ background_gamma_code
    -> #{type int} -- ^ need_expand
    -> #{type png_fixed_point} -- ^ background_gamma
    -> IO ()

foreign import ccall "png_set_scale_16"
  png_set_scale_16
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_set_quantize"
  png_set_quantize
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr PngColorStruct -- ^ palette
    -> #{type int} -- ^ num_palette
    -> #{type int} -- ^ maximum_colors
    -> ConstPtr #{type png_uint_16} -- ^ histogram
    -> #{type int} -- ^ full_quantize
    -> IO ()

foreign import ccall "png_set_gamma"
  png_set_gamma
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type double} -- ^ screen_gamma
    -> #{type double} -- ^ override_file_gamma
    -> IO ()

foreign import ccall "png_set_gamma_fixed"
  png_set_gamma_fixed
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_fixed_point} -- ^ screen_gamma
    -> #{type png_fixed_point} -- ^ override_file_gamma
    -> IO ()

foreign import ccall "png_set_flush"
  png_set_flush
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ nrows
    -> IO ()

foreign import ccall "png_write_flush"
  png_write_flush
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_start_read_image"
  png_start_read_image
    :: RPtr PngStructDef -- ^ png_ptr
    -> IO ()

foreign import ccall "png_read_update_info"
  png_read_update_info
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> IO ()

foreign import ccall "png_read_rows"
  png_read_rows
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ row
    -> Ptr (Ptr #{type png_byte}) -- ^ display_row
    -> #{type png_uint_32} -- ^ num_rows
    -> IO ()

foreign import ccall "png_read_row"
  png_read_row
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr #{type png_byte} -- ^ row
    -> Ptr #{type png_byte} -- ^ display_row
    -> IO ()

foreign import ccall "png_read_image"
  png_read_image
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ image
    -> IO ()

foreign import ccall "png_write_row"
  png_write_row
    :: RPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type png_byte} -- ^ row
    -> IO ()

foreign import ccall "png_write_rows"
  png_write_rows
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ row
    -> #{type png_uint_32} -- ^ num_rows
    -> IO ()

foreign import ccall "png_write_image"
  png_write_image
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ image
    -> IO ()

foreign import ccall "png_write_end"
  png_write_end
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> IO ()

foreign import ccall "png_read_end"
  png_read_end
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> IO ()

foreign import ccall "png_destroy_info_struct"
  png_destroy_info_struct
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> Ptr (Ptr PngInfoDef) -- ^ info_ptr_ptr
    -> IO ()

foreign import ccall "png_destroy_read_struct"
  png_destroy_read_struct
    :: Ptr (Ptr PngStructDef) -- ^ png_ptr_ptr
    -> Ptr (Ptr PngInfoDef) -- ^ info_ptr_ptr
    -> Ptr (Ptr PngInfoDef) -- ^ end_info_ptr_ptr
    -> IO ()

foreign import ccall "png_destroy_write_struct"
  png_destroy_write_struct
    :: Ptr (Ptr PngStructDef) -- ^ png_ptr_ptr
    -> Ptr (Ptr PngInfoDef) -- ^ info_ptr_ptr
    -> IO ()

foreign import ccall "png_set_crc_action"
  png_set_crc_action
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ crit_action
    -> #{type int} -- ^ ancil_action
    -> IO ()

foreign import ccall "png_set_filter"
  png_set_filter
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ method
    -> #{type int} -- ^ filters
    -> IO ()

foreign import ccall "png_set_filter_heuristics"
  png_set_filter_heuristics
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ heuristic_method
    -> #{type int} -- ^ num_weights
    -> ConstPtr #{type double} -- ^ filter_weights
    -> ConstPtr #{type double} -- ^ filter_costs
    -> IO ()

foreign import ccall "png_set_filter_heuristics_fixed"
  png_set_filter_heuristics_fixed
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ heuristic_method
    -> #{type int} -- ^ num_weights
    -> ConstPtr #{type png_fixed_point} -- ^ filter_weights
    -> ConstPtr #{type png_fixed_point} -- ^ filter_costs
    -> IO ()

foreign import ccall "png_set_compression_level"
  png_set_compression_level
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ level
    -> IO ()

foreign import ccall "png_set_compression_mem_level"
  png_set_compression_mem_level
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ mem_level
    -> IO ()

foreign import ccall "png_set_compression_strategy"
  png_set_compression_strategy
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ strategy
    -> IO ()

foreign import ccall "png_set_compression_window_bits"
  png_set_compression_window_bits
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ window_bits
    -> IO ()

foreign import ccall "png_set_compression_method"
  png_set_compression_method
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ method
    -> IO ()

foreign import ccall "png_set_text_compression_level"
  png_set_text_compression_level
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ level
    -> IO ()

foreign import ccall "png_set_text_compression_mem_level"
  png_set_text_compression_mem_level
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ mem_level
    -> IO ()

foreign import ccall "png_set_text_compression_strategy"
  png_set_text_compression_strategy
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ strategy
    -> IO ()

foreign import ccall "png_set_text_compression_window_bits"
  png_set_text_compression_window_bits
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ window_bits
    -> IO ()

foreign import ccall "png_set_text_compression_method"
  png_set_text_compression_method
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ method
    -> IO ()

foreign import ccall "png_init_io"
  png_init_io
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr CFile -- ^ fp
    -> IO ()

foreign import ccall "png_set_error_fn"
  png_set_error_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ error_ptr
    -> FunPtr PngError -- ^ error_fn
    -> FunPtr PngError -- ^ warning_fn
    -> IO ()

foreign import ccall "png_get_error_ptr"
  png_get_error_ptr
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr ())

foreign import ccall "png_set_write_fn"
  png_set_write_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ io_ptr
    -> FunPtr PngRw -- ^ write_data_fn
    -> FunPtr PngFlush -- ^ output_flush_fn
    -> IO ()

foreign import ccall "png_set_read_fn"
  png_set_read_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ io_ptr
    -> FunPtr PngRw -- ^ read_data_fn
    -> IO ()

foreign import ccall "png_get_io_ptr"
  png_get_io_ptr
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr ())

foreign import ccall "png_set_read_status_fn"
  png_set_read_status_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> FunPtr PngReadStatus -- ^ read_row_fn
    -> IO ()

foreign import ccall "png_set_write_status_fn"
  png_set_write_status_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> FunPtr PngWriteStatus -- ^ write_row_fn
    -> IO ()

foreign import ccall "png_set_mem_fn"
  png_set_mem_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ mem_ptr
    -> FunPtr PngMalloc -- ^ malloc_fn
    -> FunPtr PngFree -- ^ free_fn
    -> IO ()

foreign import ccall "png_get_mem_ptr"
  png_get_mem_ptr
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr ())

foreign import ccall "png_set_read_user_transform_fn"
  png_set_read_user_transform_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> FunPtr PngUserTransform -- ^ read_user_transform_fn
    -> IO ()

foreign import ccall "png_set_write_user_transform_fn"
  png_set_write_user_transform_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> FunPtr PngUserTransform -- ^ write_user_transform_fn
    -> IO ()

foreign import ccall "png_set_user_transform_info"
  png_set_user_transform_info
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ user_transform_ptr
    -> #{type int} -- ^ user_transform_depth
    -> #{type int} -- ^ user_transform_channels
    -> IO ()

foreign import ccall "png_get_user_transform_ptr"
  png_get_user_transform_ptr
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr ())

foreign import ccall "png_get_current_row_number"
  png_get_current_row_number
    :: ConstRPtr PngStructDef
    -> IO #{type png_uint_32}

foreign import ccall "png_get_current_pass_number"
  png_get_current_pass_number
    :: ConstRPtr PngStructDef
    -> IO #{type png_byte}

foreign import ccall "png_set_read_user_chunk_fn"
  png_set_read_user_chunk_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ user_chunk_ptr
    -> FunPtr PngUserChunk -- ^ read_user_chunk_fn
    -> IO ()

foreign import ccall "png_get_user_chunk_ptr"
  png_get_user_chunk_ptr
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr ())

foreign import ccall "png_set_progressive_read_fn"
  png_set_progressive_read_fn
    :: RPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ progressive_ptr
    -> FunPtr PngProgressiveInfo -- ^ info_fn
    -> FunPtr PngProgressiveRow -- ^ row_fn
    -> FunPtr PngProgressiveEnd -- ^ end_fn
    -> IO ()

foreign import ccall "png_get_progressive_ptr"
  png_get_progressive_ptr
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (Ptr ())

foreign import ccall "png_process_data"
  png_process_data
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_byte} -- ^ buffer
    -> #{type size_t} -- ^ buffer_size
    -> IO ()

foreign import ccall "png_process_data_pause"
  png_process_data_pause
    :: RPtr PngStructDef
    -> #{type int} -- ^ save
    -> IO #{type size_t}

foreign import ccall "png_process_data_skip"
  png_process_data_skip
    :: RPtr PngStructDef
    -> IO #{type png_uint_32}

foreign import ccall "png_progressive_combine_row"
  png_progressive_combine_row
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> Ptr #{type png_byte} -- ^ old_row
    -> ConstPtr #{type png_byte} -- ^ new_row
    -> IO ()

foreign import ccall "png_malloc"
  png_malloc
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> #{type png_alloc_size_t} -- ^ size
    -> IO (Ptr ())

foreign import ccall "png_calloc"
  png_calloc
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> #{type png_alloc_size_t} -- ^ size
    -> IO (Ptr ())

foreign import ccall "png_malloc_warn"
  png_malloc_warn
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> #{type png_alloc_size_t} -- ^ size
    -> IO (Ptr ())

foreign import ccall "png_free"
  png_free
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ ptr
    -> IO ()

foreign import ccall "png_free_data"
  png_free_data
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_uint_32} -- ^ free_me
    -> #{type int} -- ^ num
    -> IO ()

foreign import ccall "png_data_freer"
  png_data_freer
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ freer
    -> #{type png_uint_32} -- ^ mask
    -> IO ()

foreign import ccall "png_malloc_default"
  png_malloc_default
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> #{type png_alloc_size_t} -- ^ size
    -> IO (Ptr ())

foreign import ccall "png_free_default"
  png_free_default
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> Ptr () -- ^ ptr
    -> IO ()

foreign import ccall "png_error"
  png_error
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type char} -- ^ error_message
    -> IO ()

foreign import ccall "png_chunk_error"
  png_chunk_error
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type char} -- ^ error_message
    -> IO ()

foreign import ccall "png_warning"
  png_warning
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type char} -- ^ warning_message
    -> IO ()

foreign import ccall "png_chunk_warning"
  png_chunk_warning
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type char} -- ^ warning_message
    -> IO ()

foreign import ccall "png_benign_error"
  png_benign_error
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type char} -- ^ warning_message
    -> IO ()

foreign import ccall "png_chunk_benign_error"
  png_chunk_benign_error
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type char} -- ^ warning_message
    -> IO ()

foreign import ccall "png_set_benign_errors"
  png_set_benign_errors
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ allowed
    -> IO ()

foreign import ccall "png_get_valid"
  png_get_valid
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> #{type png_uint_32} -- ^ flag
    -> IO #{type png_uint_32}

foreign import ccall "png_get_rowbytes"
  png_get_rowbytes
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type size_t}

foreign import ccall "png_get_rows"
  png_get_rows
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO (Ptr (Ptr #{type png_byte}))

foreign import ccall "png_set_rows"
  png_set_rows
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ row_pointers
    -> IO ()

foreign import ccall "png_get_channels"
  png_get_channels
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_byte}

foreign import ccall "png_get_image_width"
  png_get_image_width
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_image_height"
  png_get_image_height
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_bit_depth"
  png_get_bit_depth
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_byte}

foreign import ccall "png_get_color_type"
  png_get_color_type
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_byte}

foreign import ccall "png_get_filter_type"
  png_get_filter_type
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_byte}

foreign import ccall "png_get_interlace_type"
  png_get_interlace_type
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_byte}

foreign import ccall "png_get_compression_type"
  png_get_compression_type
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_byte}

foreign import ccall "png_get_pixels_per_meter"
  png_get_pixels_per_meter
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_x_pixels_per_meter"
  png_get_x_pixels_per_meter
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_y_pixels_per_meter"
  png_get_y_pixels_per_meter
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_pixel_aspect_ratio"
  png_get_pixel_aspect_ratio
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type float}

foreign import ccall "png_get_pixel_aspect_ratio_fixed"
  png_get_pixel_aspect_ratio_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_fixed_point}

foreign import ccall "png_get_x_offset_pixels"
  png_get_x_offset_pixels
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_int_32}

foreign import ccall "png_get_y_offset_pixels"
  png_get_y_offset_pixels
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_int_32}

foreign import ccall "png_get_x_offset_microns"
  png_get_x_offset_microns
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_int_32}

foreign import ccall "png_get_y_offset_microns"
  png_get_y_offset_microns
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_int_32}

foreign import ccall "png_get_signature"
  png_get_signature
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO (ConstPtr #{type png_byte})

foreign import ccall "png_get_bKGD"
  png_get_bKGD
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngColor16Struct) -- ^ background
    -> IO #{type png_uint_32}

foreign import ccall "png_set_bKGD"
  png_set_bKGD
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngColor16Struct -- ^ background
    -> IO ()

foreign import ccall "png_get_cHRM"
  png_get_cHRM
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type double} -- ^ white_x
    -> Ptr #{type double} -- ^ white_y
    -> Ptr #{type double} -- ^ red_x
    -> Ptr #{type double} -- ^ red_y
    -> Ptr #{type double} -- ^ green_x
    -> Ptr #{type double} -- ^ green_y
    -> Ptr #{type double} -- ^ blue_x
    -> Ptr #{type double} -- ^ blue_y
    -> IO #{type png_uint_32}

foreign import ccall "png_get_cHRM_XYZ"
  png_get_cHRM_XYZ
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type double} -- ^ red_X
    -> Ptr #{type double} -- ^ red_Y
    -> Ptr #{type double} -- ^ red_Z
    -> Ptr #{type double} -- ^ green_X
    -> Ptr #{type double} -- ^ green_Y
    -> Ptr #{type double} -- ^ green_Z
    -> Ptr #{type double} -- ^ blue_X
    -> Ptr #{type double} -- ^ blue_Y
    -> Ptr #{type double} -- ^ blue_Z
    -> IO #{type png_uint_32}

foreign import ccall "png_get_cHRM_fixed"
  png_get_cHRM_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_fixed_point} -- ^ int_white_x
    -> Ptr #{type png_fixed_point} -- ^ int_white_y
    -> Ptr #{type png_fixed_point} -- ^ int_red_x
    -> Ptr #{type png_fixed_point} -- ^ int_red_y
    -> Ptr #{type png_fixed_point} -- ^ int_green_x
    -> Ptr #{type png_fixed_point} -- ^ int_green_y
    -> Ptr #{type png_fixed_point} -- ^ int_blue_x
    -> Ptr #{type png_fixed_point} -- ^ int_blue_y
    -> IO #{type png_uint_32}

foreign import ccall "png_get_cHRM_XYZ_fixed"
  png_get_cHRM_XYZ_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_fixed_point} -- ^ int_red_X
    -> Ptr #{type png_fixed_point} -- ^ int_red_Y
    -> Ptr #{type png_fixed_point} -- ^ int_red_Z
    -> Ptr #{type png_fixed_point} -- ^ int_green_X
    -> Ptr #{type png_fixed_point} -- ^ int_green_Y
    -> Ptr #{type png_fixed_point} -- ^ int_green_Z
    -> Ptr #{type png_fixed_point} -- ^ int_blue_X
    -> Ptr #{type png_fixed_point} -- ^ int_blue_Y
    -> Ptr #{type png_fixed_point} -- ^ int_blue_Z
    -> IO #{type png_uint_32}

foreign import ccall "png_set_cHRM"
  png_set_cHRM
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type double} -- ^ white_x
    -> #{type double} -- ^ white_y
    -> #{type double} -- ^ red_x
    -> #{type double} -- ^ red_y
    -> #{type double} -- ^ green_x
    -> #{type double} -- ^ green_y
    -> #{type double} -- ^ blue_x
    -> #{type double} -- ^ blue_y
    -> IO ()

foreign import ccall "png_set_cHRM_XYZ"
  png_set_cHRM_XYZ
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type double} -- ^ red_X
    -> #{type double} -- ^ red_Y
    -> #{type double} -- ^ red_Z
    -> #{type double} -- ^ green_X
    -> #{type double} -- ^ green_Y
    -> #{type double} -- ^ green_Z
    -> #{type double} -- ^ blue_X
    -> #{type double} -- ^ blue_Y
    -> #{type double} -- ^ blue_Z
    -> IO ()

foreign import ccall "png_set_cHRM_fixed"
  png_set_cHRM_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_fixed_point} -- ^ int_white_x
    -> #{type png_fixed_point} -- ^ int_white_y
    -> #{type png_fixed_point} -- ^ int_red_x
    -> #{type png_fixed_point} -- ^ int_red_y
    -> #{type png_fixed_point} -- ^ int_green_x
    -> #{type png_fixed_point} -- ^ int_green_y
    -> #{type png_fixed_point} -- ^ int_blue_x
    -> #{type png_fixed_point} -- ^ int_blue_y
    -> IO ()

foreign import ccall "png_set_cHRM_XYZ_fixed"
  png_set_cHRM_XYZ_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_fixed_point} -- ^ int_red_X
    -> #{type png_fixed_point} -- ^ int_red_Y
    -> #{type png_fixed_point} -- ^ int_red_Z
    -> #{type png_fixed_point} -- ^ int_green_X
    -> #{type png_fixed_point} -- ^ int_green_Y
    -> #{type png_fixed_point} -- ^ int_green_Z
    -> #{type png_fixed_point} -- ^ int_blue_X
    -> #{type png_fixed_point} -- ^ int_blue_Y
    -> #{type png_fixed_point} -- ^ int_blue_Z
    -> IO ()

foreign import ccall "png_get_eXIf"
  png_get_eXIf
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ exif
    -> IO #{type png_uint_32}

foreign import ccall "png_set_eXIf"
  png_set_eXIf
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_byte} -- ^ exif
    -> IO ()

foreign import ccall "png_get_eXIf_1"
  png_get_eXIf_1
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_uint_32} -- ^ num_exif
    -> Ptr (Ptr #{type png_byte}) -- ^ exif
    -> IO #{type png_uint_32}

foreign import ccall "png_set_eXIf_1"
  png_set_eXIf_1
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_uint_32} -- ^ num_exif
    -> Ptr #{type png_byte} -- ^ exif
    -> IO ()

foreign import ccall "png_get_gAMA"
  png_get_gAMA
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type double} -- ^ file_gamma
    -> IO #{type png_uint_32}

foreign import ccall "png_get_gAMA_fixed"
  png_get_gAMA_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_fixed_point} -- ^ int_file_gamma
    -> IO #{type png_uint_32}

foreign import ccall "png_set_gAMA"
  png_set_gAMA
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type double} -- ^ file_gamma
    -> IO ()

foreign import ccall "png_set_gAMA_fixed"
  png_set_gAMA_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_fixed_point} -- ^ int_file_gamma
    -> IO ()

foreign import ccall "png_get_hIST"
  png_get_hIST
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr #{type png_uint_16}) -- ^ hist
    -> IO #{type png_uint_32}

foreign import ccall "png_set_hIST"
  png_set_hIST
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr #{type png_uint_16} -- ^ hist
    -> IO ()

foreign import ccall "png_get_IHDR"
  png_get_IHDR
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_uint_32} -- ^ width
    -> Ptr #{type png_uint_32} -- ^ height
    -> Ptr #{type int} -- ^ bit_depth
    -> Ptr #{type int} -- ^ color_type
    -> Ptr #{type int} -- ^ interlace_method
    -> Ptr #{type int} -- ^ compression_method
    -> Ptr #{type int} -- ^ filter_method
    -> IO #{type png_uint_32}

foreign import ccall "png_set_IHDR"
  png_set_IHDR
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_uint_32} -- ^ width
    -> #{type png_uint_32} -- ^ height
    -> #{type int} -- ^ bit_depth
    -> #{type int} -- ^ color_type
    -> #{type int} -- ^ interlace_method
    -> #{type int} -- ^ compression_method
    -> #{type int} -- ^ filter_method
    -> IO ()

foreign import ccall "png_get_oFFs"
  png_get_oFFs
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_int_32} -- ^ offset_x
    -> Ptr #{type png_int_32} -- ^ offset_y
    -> Ptr #{type int} -- ^ unit_type
    -> IO #{type png_uint_32}

foreign import ccall "png_set_oFFs"
  png_set_oFFs
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_int_32} -- ^ offset_x
    -> #{type png_int_32} -- ^ offset_y
    -> #{type int} -- ^ unit_type
    -> IO ()

foreign import ccall "png_get_pCAL"
  png_get_pCAL
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr #{type char}) -- ^ purpose
    -> Ptr #{type png_int_32} -- ^ X0
    -> Ptr #{type png_int_32} -- ^ X1
    -> Ptr #{type int} -- ^ type
    -> Ptr #{type int} -- ^ nparams
    -> Ptr (Ptr #{type char}) -- ^ units
    -> Ptr (Ptr (Ptr #{type char})) -- ^ params
    -> IO #{type png_uint_32}

foreign import ccall "png_set_pCAL"
  png_set_pCAL
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr #{type char} -- ^ purpose
    -> #{type png_int_32} -- ^ X0
    -> #{type png_int_32} -- ^ X1
    -> #{type int} -- ^ type
    -> #{type int} -- ^ nparams
    -> ConstPtr #{type char} -- ^ units
    -> Ptr (Ptr #{type char}) -- ^ params
    -> IO ()

foreign import ccall "png_get_pHYs"
  png_get_pHYs
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_uint_32} -- ^ res_x
    -> Ptr #{type png_uint_32} -- ^ res_y
    -> Ptr #{type int} -- ^ unit_type
    -> IO #{type png_uint_32}

foreign import ccall "png_set_pHYs"
  png_set_pHYs
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type png_uint_32} -- ^ res_x
    -> #{type png_uint_32} -- ^ res_y
    -> #{type int} -- ^ unit_type
    -> IO ()

foreign import ccall "png_get_PLTE"
  png_get_PLTE
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngColorStruct) -- ^ palette
    -> Ptr #{type int} -- ^ num_palette
    -> IO #{type png_uint_32}

foreign import ccall "png_set_PLTE"
  png_set_PLTE
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngColorStruct -- ^ palette
    -> #{type int} -- ^ num_palette
    -> IO ()

foreign import ccall "png_get_sBIT"
  png_get_sBIT
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngColor8Struct) -- ^ sig_bit
    -> IO #{type png_uint_32}

foreign import ccall "png_set_sBIT"
  png_set_sBIT
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngColor8Struct -- ^ sig_bit
    -> IO ()

foreign import ccall "png_get_sRGB"
  png_get_sRGB
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type int} -- ^ file_srgb_intent
    -> IO #{type png_uint_32}

foreign import ccall "png_set_sRGB"
  png_set_sRGB
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ srgb_intent
    -> IO ()

foreign import ccall "png_set_sRGB_gAMA_and_cHRM"
  png_set_sRGB_gAMA_and_cHRM
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ srgb_intent
    -> IO ()

foreign import ccall "png_get_iCCP"
  png_get_iCCP
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr #{type char}) -- ^ name
    -> Ptr #{type int} -- ^ compression_type
    -> Ptr (Ptr #{type png_byte}) -- ^ profile
    -> Ptr #{type png_uint_32} -- ^ proflen
    -> IO #{type png_uint_32}

foreign import ccall "png_set_iCCP"
  png_set_iCCP
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr #{type char} -- ^ name
    -> #{type int} -- ^ compression_type
    -> ConstPtr #{type png_byte} -- ^ profile
    -> #{type png_uint_32} -- ^ proflen
    -> IO ()

foreign import ccall "png_get_sPLT"
  png_get_sPLT
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngSPLTStruct) -- ^ entries
    -> IO #{type int}

foreign import ccall "png_set_sPLT"
  png_set_sPLT
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngSPLTStruct -- ^ entries
    -> #{type int} -- ^ nentries
    -> IO ()

foreign import ccall "png_get_text"
  png_get_text
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngTextStruct) -- ^ text_ptr
    -> Ptr #{type int} -- ^ num_text
    -> IO #{type int}

foreign import ccall "png_set_text"
  png_set_text
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngTextStruct -- ^ text_ptr
    -> #{type int} -- ^ num_text
    -> IO ()

foreign import ccall "png_get_tIME"
  png_get_tIME
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngTimeStruct) -- ^ mod_time
    -> IO #{type png_uint_32}

foreign import ccall "png_set_tIME"
  png_set_tIME
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngTimeStruct -- ^ mod_time
    -> IO ()

foreign import ccall "png_get_tRNS"
  png_get_tRNS
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr #{type png_byte}) -- ^ trans_alpha
    -> Ptr #{type int} -- ^ num_trans
    -> Ptr (Ptr PngColor16Struct) -- ^ trans_color
    -> IO #{type png_uint_32}

foreign import ccall "png_set_tRNS"
  png_set_tRNS
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr #{type png_byte} -- ^ trans_alpha
    -> #{type int} -- ^ num_trans
    -> ConstPtr PngColor16Struct -- ^ trans_color
    -> IO ()

foreign import ccall "png_get_sCAL"
  png_get_sCAL
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type int} -- ^ unit
    -> Ptr #{type double} -- ^ width
    -> Ptr #{type double} -- ^ height
    -> IO #{type png_uint_32}

foreign import ccall "png_get_sCAL_fixed"
  png_get_sCAL_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type int} -- ^ unit
    -> Ptr #{type png_fixed_point} -- ^ width
    -> Ptr #{type png_fixed_point} -- ^ height
    -> IO #{type png_uint_32}

foreign import ccall "png_get_sCAL_s"
  png_get_sCAL_s
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type int} -- ^ unit
    -> Ptr (Ptr #{type char}) -- ^ swidth
    -> Ptr (Ptr #{type char}) -- ^ sheight
    -> IO #{type png_uint_32}

foreign import ccall "png_set_sCAL"
  png_set_sCAL
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ unit
    -> #{type double} -- ^ width
    -> #{type double} -- ^ height
    -> IO ()

foreign import ccall "png_set_sCAL_fixed"
  png_set_sCAL_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ unit
    -> #{type png_fixed_point} -- ^ width
    -> #{type png_fixed_point} -- ^ height
    -> IO ()

foreign import ccall "png_set_sCAL_s"
  png_set_sCAL_s
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ unit
    -> ConstPtr #{type char} -- ^ swidth
    -> ConstPtr #{type char} -- ^ sheight
    -> IO ()

foreign import ccall "png_set_keep_unknown_chunks"
  png_set_keep_unknown_chunks
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ keep
    -> ConstPtr #{type png_byte} -- ^ chunk_list
    -> #{type int} -- ^ num_chunks
    -> IO ()

foreign import ccall "png_handle_as_unknown"
  png_handle_as_unknown
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type png_byte} -- ^ chunk_name
    -> IO #{type int}

foreign import ccall "png_set_unknown_chunks"
  png_set_unknown_chunks
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> ConstPtr PngUnknownChunkT -- ^ unknowns
    -> #{type int} -- ^ num_unknowns
    -> IO ()

foreign import ccall "png_set_unknown_chunk_location"
  png_set_unknown_chunk_location
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ chunk
    -> #{type int} -- ^ location
    -> IO ()

foreign import ccall "png_get_unknown_chunks"
  png_get_unknown_chunks
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> Ptr (Ptr PngUnknownChunkT) -- ^ entries
    -> IO #{type int}

foreign import ccall "png_set_invalid"
  png_set_invalid
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ mask
    -> IO ()

foreign import ccall "png_read_png"
  png_read_png
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ transforms
    -> Ptr () -- ^ params
    -> IO ()

foreign import ccall "png_write_png"
  png_write_png
    :: RPtr PngStructDef -- ^ png_ptr
    -> RPtr PngInfoDef -- ^ info_ptr
    -> #{type int} -- ^ transforms
    -> Ptr () -- ^ params
    -> IO ()

foreign import ccall "png_get_copyright"
  png_get_copyright
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (ConstPtr #{type char})

foreign import ccall "png_get_header_ver"
  png_get_header_ver
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (ConstPtr #{type char})

foreign import ccall "png_get_header_version"
  png_get_header_version
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (ConstPtr #{type char})

foreign import ccall "png_get_libpng_ver"
  png_get_libpng_ver
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO (ConstPtr #{type char})

foreign import ccall "png_permit_mng_features"
  png_permit_mng_features
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_uint_32} -- ^ mng_features_permitted
    -> IO #{type png_uint_32}

foreign import ccall "png_set_user_limits"
  png_set_user_limits
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_uint_32} -- ^ user_width_max
    -> #{type png_uint_32} -- ^ user_height_max
    -> IO ()

foreign import ccall "png_get_user_width_max"
  png_get_user_width_max
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_user_height_max"
  png_get_user_height_max
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_set_chunk_cache_max"
  png_set_chunk_cache_max
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_uint_32} -- ^ user_chunk_cache_max
    -> IO ()

foreign import ccall "png_get_chunk_cache_max"
  png_get_chunk_cache_max
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_set_chunk_malloc_max"
  png_set_chunk_malloc_max
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type png_alloc_size_t} -- ^ user_chunk_cache_max
    -> IO ()

foreign import ccall "png_get_chunk_malloc_max"
  png_get_chunk_malloc_max
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_alloc_size_t}

foreign import ccall "png_get_pixels_per_inch"
  png_get_pixels_per_inch
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_x_pixels_per_inch"
  png_get_x_pixels_per_inch
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_y_pixels_per_inch"
  png_get_y_pixels_per_inch
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_x_offset_inches"
  png_get_x_offset_inches
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type float}

foreign import ccall "png_get_x_offset_inches_fixed"
  png_get_x_offset_inches_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_fixed_point}

foreign import ccall "png_get_y_offset_inches"
  png_get_y_offset_inches
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type float}

foreign import ccall "png_get_y_offset_inches_fixed"
  png_get_y_offset_inches_fixed
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> IO #{type png_fixed_point}

foreign import ccall "png_get_pHYs_dpi"
  png_get_pHYs_dpi
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstRPtr PngInfoDef -- ^ info_ptr
    -> Ptr #{type png_uint_32} -- ^ res_x
    -> Ptr #{type png_uint_32} -- ^ res_y
    -> Ptr #{type int} -- ^ unit_type
    -> IO #{type png_uint_32}

foreign import ccall "png_get_io_state"
  png_get_io_state
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_io_chunk_type"
  png_get_io_chunk_type
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> IO #{type png_uint_32}

foreign import ccall "png_get_uint_32"
  png_get_uint_32
    :: ConstPtr #{type png_byte} -- ^ buf
    -> IO #{type png_uint_32}

foreign import ccall "png_get_uint_16"
  png_get_uint_16
    :: ConstPtr #{type png_byte} -- ^ buf
    -> IO #{type png_uint_16}

foreign import ccall "png_get_int_32"
  png_get_int_32
    :: ConstPtr #{type png_byte} -- ^ buf
    -> IO #{type png_int_32}

foreign import ccall "png_get_uint_31"
  png_get_uint_31
    :: ConstRPtr PngStructDef -- ^ png_ptr
    -> ConstPtr #{type png_byte} -- ^ buf
    -> IO #{type png_uint_32}

foreign import ccall "png_save_uint_32"
  png_save_uint_32
    :: Ptr #{type png_byte} -- ^ buf
    -> #{type png_uint_32} -- ^ i
    -> IO ()

foreign import ccall "png_save_int_32"
  png_save_int_32
    :: Ptr #{type png_byte} -- ^ buf
    -> #{type png_int_32} -- ^ i
    -> IO ()

foreign import ccall "png_save_uint_16"
  png_save_uint_16
    :: Ptr #{type png_byte} -- ^ buf
    -> #{type unsigned int} -- ^ i
    -> IO ()

foreign import ccall "png_set_check_for_invalid_index"
  png_set_check_for_invalid_index
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ allowed
    -> IO ()

foreign import ccall "png_get_palette_max"
  png_get_palette_max
    :: ConstPtr PngStructDef -- ^ png_ptr
    -> ConstPtr PngInfoDef -- ^ info_ptr
    -> IO #{type int}

foreign import ccall "png_image_begin_read_from_file"
  png_image_begin_read_from_file
    :: Ptr PngImageStruct -- ^ image
    -> ConstPtr #{type char} -- ^ file_name
    -> IO #{type int}

foreign import ccall "png_image_begin_read_from_stdio"
  png_image_begin_read_from_stdio
    :: Ptr PngImageStruct -- ^ image
    -> Ptr CFile -- ^ file
    -> IO #{type int}

foreign import ccall "png_image_begin_read_from_memory"
  png_image_begin_read_from_memory
    :: Ptr PngImageStruct -- ^ image
    -> ConstPtr () -- ^ memory
    -> #{type size_t} -- ^ size
    -> IO #{type int}

foreign import ccall "png_image_finish_read"
  png_image_finish_read
    :: Ptr PngImageStruct -- ^ image
    -> ConstPtr PngColorStruct -- ^ background
    -> Ptr () -- ^ buffer
    -> #{type png_int_32} -- ^ row_stride
    -> Ptr () -- ^ colormap
    -> IO #{type int}

foreign import ccall "png_image_free"
  png_image_free
    :: Ptr PngImageStruct -- ^ image
    -> IO ()

foreign import ccall "png_image_write_to_file"
  png_image_write_to_file
    :: Ptr PngImageStruct -- ^ image
    -> ConstPtr #{type char} -- ^ file
    -> #{type int} -- ^ convert_to_8bit
    -> ConstPtr () -- ^ buffer
    -> #{type png_int_32} -- ^ row_stride
    -> ConstPtr () -- ^ colormap
    -> IO #{type int}

foreign import ccall "png_image_write_to_stdio"
  png_image_write_to_stdio
    :: Ptr PngImageStruct -- ^ image
    -> Ptr CFile -- ^ file
    -> #{type int} -- ^ convert_to_8_bit
    -> ConstPtr () -- ^ buffer
    -> #{type png_int_32} -- ^ row_stride
    -> ConstPtr () -- ^ colormap
    -> IO #{type int}

foreign import ccall "png_image_write_to_memory"
  png_image_write_to_memory
    :: Ptr PngImageStruct -- ^ image
    -> Ptr () -- ^ memory
    -> RPtr #{type png_alloc_size_t} -- ^ memory_bytes
    -> #{type int} -- ^ convert_to_8_bit
    -> ConstPtr () -- ^ buffer
    -> #{type png_int_32} -- ^ row_stride
    -> ConstPtr () -- ^ colormap
    -> IO #{type int}

foreign import ccall "png_set_option"
  png_set_option
    :: RPtr PngStructDef -- ^ png_ptr
    -> #{type int} -- ^ option
    -> #{type int} -- ^ onoff
    -> IO #{type int}

