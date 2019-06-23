context("CreateFLX testing")

#functionality tests
reg_obj <- CreateFLX(t_df_struct)

test_that("regulartable is created successfully", {
  expect_s3_class(reg_obj, "regulartable")
})

test_that("header is assigned successfully", {
  origin_header_part <- setNames(as.data.frame(matrix(unlist(t_df_struct$header), nrow = length(t_df_struct$header), byrow = T), stringsAsFactors = FALSE), names(t_df_struct$data))

  expect_equal(reg_obj$header$dataset, origin_header_part)
})

test_that("footer is assigned successfully", {
  origin_footer_part <- setNames(as.data.frame(matrix(unlist(t_df_struct$footer), nrow = length(t_df_struct$footer), byrow = T), stringsAsFactors = FALSE), names(t_df_struct$data))

  expect_equal(reg_obj$footer$dataset, origin_footer_part)
})

#TODO: cells_to_merge, rows_to_merge, columns_to_merge, table_width

test_that("font is assigned successfully", {
  expect_equal(reg_obj$body$styles$text$get_fp()[[2]]$font.size, t_df_struct$font)
  expect_equal(reg_obj$header$styles$text$get_fp()[[2]]$font.size, t_df_struct$font)
  expect_equal(reg_obj$footer$styles$text$get_fp()[[2]]$font.size, t_df_struct$font)
})

test_that("1 and 3 footer part rows are of italic font",{
  expect_equal(reg_obj$footer$styles$text$get_fp()[[3]]$italic, TRUE)
})

#TODO: rows_as_inner_headers, rows_section_init, columns_left_align, rows_bold_upper_bordered, cells_color, rows_p_value_color

#output tests
#test_that("regulartable is failed to create", {
  #expect_error()
  #expect_message()
#})

