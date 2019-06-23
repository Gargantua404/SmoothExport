#data definition
t_data <- dplyr::mutate_all(mtcars[1:3, 1:5], as.character) %>%
  dplyr::add_row(mpg = "population A", cyl = "population A", disp = "population A", hp = "population A", drat = "population A")  %>%
  dplyr::bind_rows(dplyr::mutate_all(mtcars[4:6, 1:5], as.character)) %>%
  dplyr::add_row(mpg = "norm test:", cyl = "norm test:", disp = "norm test:", hp = "norm test:", drat = "norm test:") %>%
  dplyr::add_row(mpg = "p = 0.43", cyl = "p < 0,001", disp = "p = 0,44", hp = "p = 0.004", drat = "p = 0.002") %>%
  dplyr::bind_rows(dplyr::mutate_all(mtcars[7:9, 1:5], as.character)) %>%
  dplyr::add_row(mpg = "population B", cyl = "population B", disp = "population B", hp = "population B", drat = "population B") %>%
  dplyr::bind_rows(dplyr::mutate_all(mtcars[10:12, 1:5], as.character))


t_df_struct <- list(data = t_data,
                    header = list(row1 = rep("all columns", ncol(t_data)),
                                   row2 = c(rep("1st column group", floor(ncol(t_data)/2)), rep("2st column group", ceiling(ncol(t_data)/2))),
                                   row3 = names(t_data)),
                    footer = list(row1 = rep("Note:", ncol(t_data)),
                                  row2 = rep("Nothing special for this table", ncol(t_data)),
                                  row3 = rep("Designation:", ncol(t_data)),
                                  row4 = rep("NA - not available", ncol(t_data))),
                    rows_to_merge = c(4, 8, 13),
                    cells_to_merge = list(rect1 = list(rows = 1:2, columns = 1),
                                         rect2 = list(rows = 14:15, columns = 4:5)),
                    columns_to_merge = 5,
                    table_width = NULL,
                    font = 12,
                    rows_as_footnote_title = c(1, 3),
                    rows_as_inner_headers = c(4, 13),
                    rows_section_init = 8,
                    columns_left_align = c(2, 4),
                    rows_bold_upper_bordered = 15,
                    cells_color = list(cell1 = list(row = 15, column = 1),
                                       cell2 = list(row = 16, column = 2)),
                    rows_p_value_color = 9)
