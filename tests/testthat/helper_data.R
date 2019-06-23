#data definition
t_data <- head(mtcars[1:6])
t_df_struct <- list(data = t_data,
                    header = list(row1 = rep("all columns", ncol(t_data)),
                                   row2 = c(rep("1st column groups", floor(ncol(t_data)/2)), rep("2st column groups", ceiling(ncol(t_data)/2))),
                                   row3 = names(t_data)),
                    footer = list(row1 = rep("Note:", ncol(t_data)),
                                  row2 = rep("Nothing special for this table", ncol(t_data)),
                                  row3 = rep("Designation:", ncol(t_data)),
                                  row4 = rep("NA - non available", ncol(t_data))),
                    font = 12,
                    rows_as_footnote_title = c(1, 3)
                    # ,
                    #   rows_as_inner_headers =
                    #   rows_section_init =
                    #   columns_left_align =
                    #   rows_bold_upper_bordered =
                    #   cells_color =
                    #   rows_p_value_color =
                     )
