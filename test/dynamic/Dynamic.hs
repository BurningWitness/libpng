module Main where

import           Dynamic.Report

import           Codec.Image.PNG.Internal.Raw



-- | Tests whether all of the @foreign import ccall@s refer to correct function names.
--   You do not need to launch the test to know that the names are correctly
--   bound -- that is figured out at compilation time.
main :: IO ()
main = do
  _ <- report "png_access_version_number" png_access_version_number
  _ <- report "png_set_sig_bytes" png_set_sig_bytes
  _ <- report "png_sig_cmp" png_sig_cmp
  _ <- report "png_create_read_struct" png_create_read_struct
  _ <- report "png_create_write_struct" png_create_write_struct
  _ <- report "png_get_compression_buffer_size" png_get_compression_buffer_size
  _ <- report "png_set_compression_buffer_size" png_set_compression_buffer_size
  _ <- report "png_set_longjmp_fn" png_set_longjmp_fn
  _ <- report "png_longjmp" png_longjmp
  _ <- report "png_reset_zstream" png_reset_zstream
  _ <- report "png_create_read_struct_2" png_create_read_struct_2
  _ <- report "png_create_write_struct_2" png_create_write_struct_2
  _ <- report "png_write_sig" png_write_sig
  _ <- report "png_write_chunk" png_write_chunk
  _ <- report "png_write_chunk_start" png_write_chunk_start
  _ <- report "png_write_chunk_data" png_write_chunk_data
  _ <- report "png_write_chunk_end" png_write_chunk_end
  _ <- report "png_create_info_struct" png_create_info_struct
  _ <- report "png_info_init_3" png_info_init_3
  _ <- report "png_write_info_before_PLTE" png_write_info_before_PLTE
  _ <- report "png_write_info" png_write_info
  _ <- report "png_read_info" png_read_info
  _ <- report "png_convert_to_rfc1123" png_convert_to_rfc1123
  _ <- report "png_convert_to_rfc1123_buffer" png_convert_to_rfc1123_buffer
  _ <- report "png_convert_from_struct_tm" png_convert_from_struct_tm
  _ <- report "png_convert_from_time_t" png_convert_from_time_t
  _ <- report "png_set_expand" png_set_expand
  _ <- report "png_set_expand_gray_1_2_4_to_8" png_set_expand_gray_1_2_4_to_8
  _ <- report "png_set_palette_to_rgb" png_set_palette_to_rgb
  _ <- report "png_set_tRNS_to_alpha" png_set_tRNS_to_alpha
  _ <- report "png_set_expand_16" png_set_expand_16
  _ <- report "png_set_bgr" png_set_bgr
  _ <- report "png_set_gray_to_rgb" png_set_gray_to_rgb
  _ <- report "png_set_rgb_to_gray" png_set_rgb_to_gray
  _ <- report "png_set_rgb_to_gray_fixed" png_set_rgb_to_gray_fixed
  _ <- report "png_get_rgb_to_gray_status" png_get_rgb_to_gray_status
  _ <- report "png_build_grayscale_palette" png_build_grayscale_palette
  _ <- report "png_set_alpha_mode" png_set_alpha_mode
  _ <- report "png_set_alpha_mode_fixed" png_set_alpha_mode_fixed
  _ <- report "png_set_strip_alpha" png_set_strip_alpha
  _ <- report "png_set_swap_alpha" png_set_swap_alpha
  _ <- report "png_set_invert_alpha" png_set_invert_alpha
  _ <- report "png_set_filler" png_set_filler
  _ <- report "png_set_add_alpha" png_set_add_alpha
  _ <- report "png_set_swap" png_set_swap
  _ <- report "png_set_packing" png_set_packing
  _ <- report "png_set_packswap" png_set_packswap
  _ <- report "png_set_shift" png_set_shift
  _ <- report "png_set_interlace_handling" png_set_interlace_handling
  _ <- report "png_set_invert_mono" png_set_invert_mono
  _ <- report "png_set_background" png_set_background
  _ <- report "png_set_background_fixed" png_set_background_fixed
  _ <- report "png_set_scale_16" png_set_scale_16
  _ <- report "png_set_quantize" png_set_quantize
  _ <- report "png_set_gamma" png_set_gamma
  _ <- report "png_set_gamma_fixed" png_set_gamma_fixed
  _ <- report "png_set_flush" png_set_flush
  _ <- report "png_write_flush" png_write_flush
  _ <- report "png_start_read_image" png_start_read_image
  _ <- report "png_read_update_info" png_read_update_info
  _ <- report "png_read_rows" png_read_rows
  _ <- report "png_read_row" png_read_row
  _ <- report "png_read_image" png_read_image
  _ <- report "png_write_row" png_write_row
  _ <- report "png_write_rows" png_write_rows
  _ <- report "png_write_image" png_write_image
  _ <- report "png_write_end" png_write_end
  _ <- report "png_read_end" png_read_end
  _ <- report "png_destroy_info_struct" png_destroy_info_struct
  _ <- report "png_destroy_read_struct" png_destroy_read_struct
  _ <- report "png_destroy_write_struct" png_destroy_write_struct
  _ <- report "png_set_crc_action" png_set_crc_action
  _ <- report "png_set_filter" png_set_filter
  _ <- report "png_set_filter_heuristics" png_set_filter_heuristics
  _ <- report "png_set_filter_heuristics_fixed" png_set_filter_heuristics_fixed
  _ <- report "png_set_compression_level" png_set_compression_level
  _ <- report "png_set_compression_mem_level" png_set_compression_mem_level
  _ <- report "png_set_compression_strategy" png_set_compression_strategy
  _ <- report "png_set_compression_window_bits" png_set_compression_window_bits
  _ <- report "png_set_compression_method" png_set_compression_method
  _ <- report "png_set_text_compression_level" png_set_text_compression_level
  _ <- report "png_set_text_compression_mem_level" png_set_text_compression_mem_level
  _ <- report "png_set_text_compression_strategy" png_set_text_compression_strategy
  _ <- report "png_set_text_compression_window_bits" png_set_text_compression_window_bits
  _ <- report "png_set_text_compression_method" png_set_text_compression_method
  _ <- report "png_init_io" png_init_io
  _ <- report "png_set_error_fn" png_set_error_fn
  _ <- report "png_get_error_ptr" png_get_error_ptr
  _ <- report "png_set_write_fn" png_set_write_fn
  _ <- report "png_set_read_fn" png_set_read_fn
  _ <- report "png_get_io_ptr" png_get_io_ptr
  _ <- report "png_set_read_status_fn" png_set_read_status_fn
  _ <- report "png_set_write_status_fn" png_set_write_status_fn
  _ <- report "png_set_mem_fn" png_set_mem_fn
  _ <- report "png_get_mem_ptr" png_get_mem_ptr
  _ <- report "png_set_read_user_transform_fn" png_set_read_user_transform_fn
  _ <- report "png_set_write_user_transform_fn" png_set_write_user_transform_fn
  _ <- report "png_set_user_transform_info" png_set_user_transform_info
  _ <- report "png_get_user_transform_ptr" png_get_user_transform_ptr
  _ <- report "png_get_current_row_number" png_get_current_row_number
  _ <- report "png_get_current_pass_number" png_get_current_pass_number
  _ <- report "png_set_read_user_chunk_fn" png_set_read_user_chunk_fn
  _ <- report "png_get_user_chunk_ptr" png_get_user_chunk_ptr
  _ <- report "png_set_progressive_read_fn" png_set_progressive_read_fn
  _ <- report "png_get_progressive_ptr" png_get_progressive_ptr
  _ <- report "png_process_data" png_process_data
  _ <- report "png_process_data_pause" png_process_data_pause
  _ <- report "png_process_data_skip" png_process_data_skip
  _ <- report "png_progressive_combine_row" png_progressive_combine_row
  _ <- report "png_malloc" png_malloc
  _ <- report "png_calloc" png_calloc
  _ <- report "png_malloc_warn" png_malloc_warn
  _ <- report "png_free" png_free
  _ <- report "png_free_data" png_free_data
  _ <- report "png_data_freer" png_data_freer
  _ <- report "png_malloc_default" png_malloc_default
  _ <- report "png_free_default" png_free_default
  _ <- report "png_error" png_error
  _ <- report "png_chunk_error" png_chunk_error
  _ <- report "png_warning" png_warning
  _ <- report "png_chunk_warning" png_chunk_warning
  _ <- report "png_benign_error" png_benign_error
  _ <- report "png_chunk_benign_error" png_chunk_benign_error
  _ <- report "png_set_benign_errors" png_set_benign_errors
  _ <- report "png_get_valid" png_get_valid
  _ <- report "png_get_rowbytes" png_get_rowbytes
  _ <- report "png_get_rows" png_get_rows
  _ <- report "png_set_rows" png_set_rows
  _ <- report "png_get_channels" png_get_channels
  _ <- report "png_get_image_width" png_get_image_width
  _ <- report "png_get_image_height" png_get_image_height
  _ <- report "png_get_bit_depth" png_get_bit_depth
  _ <- report "png_get_color_type" png_get_color_type
  _ <- report "png_get_filter_type" png_get_filter_type
  _ <- report "png_get_interlace_type" png_get_interlace_type
  _ <- report "png_get_compression_type" png_get_compression_type
  _ <- report "png_get_pixels_per_meter" png_get_pixels_per_meter
  _ <- report "png_get_x_pixels_per_meter" png_get_x_pixels_per_meter
  _ <- report "png_get_y_pixels_per_meter" png_get_y_pixels_per_meter
  _ <- report "png_get_pixel_aspect_ratio" png_get_pixel_aspect_ratio
  _ <- report "png_get_pixel_aspect_ratio_fixed" png_get_pixel_aspect_ratio_fixed
  _ <- report "png_get_x_offset_pixels" png_get_x_offset_pixels
  _ <- report "png_get_y_offset_pixels" png_get_y_offset_pixels
  _ <- report "png_get_x_offset_microns" png_get_x_offset_microns
  _ <- report "png_get_y_offset_microns" png_get_y_offset_microns
  _ <- report "png_get_signature" png_get_signature
  _ <- report "png_get_bKGD" png_get_bKGD
  _ <- report "png_set_bKGD" png_set_bKGD
  _ <- report "png_get_cHRM" png_get_cHRM
  _ <- report "png_get_cHRM_XYZ" png_get_cHRM_XYZ
  _ <- report "png_get_cHRM_fixed" png_get_cHRM_fixed
  _ <- report "png_get_cHRM_XYZ_fixed" png_get_cHRM_XYZ_fixed
  _ <- report "png_set_cHRM" png_set_cHRM
  _ <- report "png_set_cHRM_XYZ" png_set_cHRM_XYZ
  _ <- report "png_set_cHRM_fixed" png_set_cHRM_fixed
  _ <- report "png_set_cHRM_XYZ_fixed" png_set_cHRM_XYZ_fixed
  _ <- report "png_get_eXIf" png_get_eXIf
  _ <- report "png_set_eXIf" png_set_eXIf
  _ <- report "png_get_eXIf_1" png_get_eXIf_1
  _ <- report "png_set_eXIf_1" png_set_eXIf_1
  _ <- report "png_get_gAMA" png_get_gAMA
  _ <- report "png_get_gAMA_fixed" png_get_gAMA_fixed
  _ <- report "png_set_gAMA" png_set_gAMA
  _ <- report "png_set_gAMA_fixed" png_set_gAMA_fixed
  _ <- report "png_get_hIST" png_get_hIST
  _ <- report "png_set_hIST" png_set_hIST
  _ <- report "png_get_IHDR" png_get_IHDR
  _ <- report "png_set_IHDR" png_set_IHDR
  _ <- report "png_get_oFFs" png_get_oFFs
  _ <- report "png_set_oFFs" png_set_oFFs
  _ <- report "png_get_pCAL" png_get_pCAL
  _ <- report "png_set_pCAL" png_set_pCAL
  _ <- report "png_get_pHYs" png_get_pHYs
  _ <- report "png_set_pHYs" png_set_pHYs
  _ <- report "png_get_PLTE" png_get_PLTE
  _ <- report "png_set_PLTE" png_set_PLTE
  _ <- report "png_get_sBIT" png_get_sBIT
  _ <- report "png_set_sBIT" png_set_sBIT
  _ <- report "png_get_sRGB" png_get_sRGB
  _ <- report "png_set_sRGB" png_set_sRGB
  _ <- report "png_set_sRGB_gAMA_and_cHRM" png_set_sRGB_gAMA_and_cHRM
  _ <- report "png_get_iCCP" png_get_iCCP
  _ <- report "png_set_iCCP" png_set_iCCP
  _ <- report "png_get_sPLT" png_get_sPLT
  _ <- report "png_set_sPLT" png_set_sPLT
  _ <- report "png_get_text" png_get_text
  _ <- report "png_set_text" png_set_text
  _ <- report "png_get_tIME" png_get_tIME
  _ <- report "png_set_tIME" png_set_tIME
  _ <- report "png_get_tRNS" png_get_tRNS
  _ <- report "png_set_tRNS" png_set_tRNS
  _ <- report "png_get_sCAL" png_get_sCAL
  _ <- report "png_get_sCAL_fixed" png_get_sCAL_fixed
  _ <- report "png_get_sCAL_s" png_get_sCAL_s
  _ <- report "png_set_sCAL" png_set_sCAL
  _ <- report "png_set_sCAL_fixed" png_set_sCAL_fixed
  _ <- report "png_set_sCAL_s" png_set_sCAL_s
  _ <- report "png_set_keep_unknown_chunks" png_set_keep_unknown_chunks
  _ <- report "png_handle_as_unknown" png_handle_as_unknown
  _ <- report "png_set_unknown_chunks" png_set_unknown_chunks
  _ <- report "png_set_unknown_chunk_location" png_set_unknown_chunk_location
  _ <- report "png_get_unknown_chunks" png_get_unknown_chunks
  _ <- report "png_set_invalid" png_set_invalid
  _ <- report "png_read_png" png_read_png
  _ <- report "png_write_png" png_write_png
  _ <- report "png_get_copyright" png_get_copyright
  _ <- report "png_get_header_ver" png_get_header_ver
  _ <- report "png_get_header_version" png_get_header_version
  _ <- report "png_get_libpng_ver" png_get_libpng_ver
  _ <- report "png_permit_mng_features" png_permit_mng_features
  _ <- report "png_set_user_limits" png_set_user_limits
  _ <- report "png_get_user_width_max" png_get_user_width_max
  _ <- report "png_get_user_height_max" png_get_user_height_max
  _ <- report "png_set_chunk_cache_max" png_set_chunk_cache_max
  _ <- report "png_get_chunk_cache_max" png_get_chunk_cache_max
  _ <- report "png_set_chunk_malloc_max" png_set_chunk_malloc_max
  _ <- report "png_get_chunk_malloc_max" png_get_chunk_malloc_max
  _ <- report "png_get_pixels_per_inch" png_get_pixels_per_inch
  _ <- report "png_get_x_pixels_per_inch" png_get_x_pixels_per_inch
  _ <- report "png_get_y_pixels_per_inch" png_get_y_pixels_per_inch
  _ <- report "png_get_x_offset_inches" png_get_x_offset_inches
  _ <- report "png_get_x_offset_inches_fixed" png_get_x_offset_inches_fixed
  _ <- report "png_get_y_offset_inches" png_get_y_offset_inches
  _ <- report "png_get_y_offset_inches_fixed" png_get_y_offset_inches_fixed
  _ <- report "png_get_pHYs_dpi" png_get_pHYs_dpi
  _ <- report "png_get_io_state" png_get_io_state
  _ <- report "png_get_io_chunk_type" png_get_io_chunk_type
  _ <- report "png_get_uint_32" png_get_uint_32
  _ <- report "png_get_uint_16" png_get_uint_16
  _ <- report "png_get_int_32" png_get_int_32
  _ <- report "png_get_uint_31" png_get_uint_31
  _ <- report "png_save_uint_32" png_save_uint_32
  _ <- report "png_save_int_32" png_save_int_32
  _ <- report "png_save_uint_16" png_save_uint_16
  _ <- report "png_set_check_for_invalid_index" png_set_check_for_invalid_index
  _ <- report "png_get_palette_max" png_get_palette_max
  _ <- report "png_image_begin_read_from_file" png_image_begin_read_from_file
  _ <- report "png_image_begin_read_from_stdio" png_image_begin_read_from_stdio
  _ <- report "png_image_begin_read_from_memory" png_image_begin_read_from_memory
  _ <- report "png_image_finish_read" png_image_finish_read
  _ <- report "png_image_free" png_image_free
  _ <- report "png_image_write_to_file" png_image_write_to_file
  _ <- report "png_image_write_to_stdio" png_image_write_to_stdio
  _ <- report "png_image_write_to_memory" png_image_write_to_memory
  _ <- report "png_set_option" png_set_option

  putStrLn "✓ All clear ✓"