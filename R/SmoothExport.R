#' SmoothExport: package to convert R dataframes into flextable objects with capability of further export into Word files
#'
#'
#' @name SmoothExport
#' @docType package
#' @import magrittr

NULL

#' @title Set cell borders of a flextable object
#' @description Set cell borders for header, footer and body parts of the flextable object
#'
#' @param df_flex a flextable object
#'
#' @return The flextable object with borders
SetBorders <- function(df_flex){
  #border settings
  std_border = officer::fp_border(color = "black", width = 1L)
  #TODO: make more sophisticated type of horizontal rows

  df_flex <- df_flex %>%
    flextable::border_inner_h(border = std_border, part = "header") %>%
    flextable::border_inner_h(border = std_border, part = "body") %>%
    flextable::border_inner_v(border = std_border, part = "header") %>%
    flextable::border_inner_v(border = std_border, part = "body") %>%
    flextable::border_outer(border = std_border, part = "all")

  return(df_flex)
}

#' @title Merges cells, rows and columns of a flextable object
#' @description Merges cells, rows and columns of a flextable object
#'
#' @param df_flex a flextable object
#' @param is_footer whether there is a footer in a flextable object
#' @param columns indexes of the columns to merge
#' @param rows indexes of the rows to merge
#' @param cells list of rectangulars as subdataframe to merge
#'
#' @return The flextable object with merged cells
MergeCells <- function(df_flex, is_footer, columns, rows, cells){
  if(!is.null(cells)){
    for(cell in cells){
      df_flex <- flextable::merge_at(df_flex, i = cell$rows, j = cell$columns, part = "body")
    }
  }
  if(!is.null(columns)){
    df_flex <- flextable::merge_v(df_flex, j = columns, part = "body")
  }
  if(!is.null(rows)){
    df_flex <- flextable::merge_h(df_flex, i = rows, part = "body")
  }

  #merge header fully
  df_flex <- df_flex %>%
    flextable::merge_v(part = "header") %>%
    flextable::merge_h(part = "header")

  if(is_footer){
    #merge footer fully
    df_flex <- df_flex %>%
      flextable::merge_v(part = "footer") %>%
      flextable::merge_h(part = "footer")
  }

  return(df_flex)
}

#' @title Set cell widths of a flextable object
#' @description Set cell widths of a flextable object
#'
#' @param df_flex a flextable object
#' @param widths a vector of widths in cm
#'
#' @return The flextable object with the fitted columns
SetWidthCells <- function(df_flex, widths){
  for (col in seq_along(widths)){
    df_flex <- flextable::width(df_flex, j = col, width = widths[col]/2.54)
  }
  return(df_flex)
}

#' @title Set styles of flextable cells
#' @description Set styles of flextable cells
#'
#' @param df_flex a flextable object
#' @param font font size
#' @param rows_as_footnote_title footer rows corresponding titles (Note, Designation, etc.)
#' @param rows_as_inner_headers body rows with inner headers (names of features, characteristics, etc)
#' @param rows_section_init body rows representing new section beginning (Normality test, etc)
#' @param columns_left_align columns to align left
#' @param rows_bold_upper_bordered rows with bold upper border
#' @param cells_color rows coloured orange (i.e. for a dropouts table)
#' @param rows_p_value_color body rows consisting of p_value for colouring
#'
#' @details For more detailed parameters description see \code{CreateFLX()}.
#' @return The styled flextable object
SetStyle <- function(df_flex, font, rows_as_footnote_title, rows_as_inner_headers, rows_section_init, columns_left_align, rows_bold_upper_bordered, cells_color, rows_p_value_color){

  #types of styles
  styles = tibble::lst(body = tibble::lst(cell = officer::fp_cell(border = officer::fp_border(color = "#000000")),
                                          cell_inner = stats::update(cell, background.color = "#F0F0F0"),
                                          cell_dr = stats::update(cell, background.color = "#FFE599"),
                                          text = officer::fp_text(font.size = font, bold = FALSE, font.family = "Times New Roman"),
                                          par = officer::fp_par(text.align = "center", padding.left = 2L, padding.right = 2L, padding.bottom = 0L, padding.top = 0L),
                                          par_left = stats::update(par, text.align = "left")),
                       header = tibble::lst(cell = officer::fp_cell(border = officer::fp_border(color = "#000000")),
                                            text = stats::update(body$text, bold = TRUE)),
                       footer = tibble::lst(cell = officer::fp_cell(border.left = officer::fp_border(color = "#000000"), border.right = officer::fp_border(color = "#000000")),
                                            text = stats::update(body$text, italic = TRUE),
                                            par = stats::update(body$par, text.align = "justify")))


  # body styles -------------------------------------------------------------
  df_flex <- flextable::style(df_flex,
                              pr_c = styles$body$cell,
                              pr_p = styles$body$par,
                              pr_t = styles$body$text,
                              part = "body")

  if(!is.null(columns_left_align)){
    df_flex <- flextable::style(df_flex,
                                j = columns_left_align,
                                pr_c = styles$body$cell,
                                pr_p = styles$body$par_left,
                                pr_t = styles$body$text,
                                part = "body")
  }

  #for inner body cells
  if(!is.null(rows_as_inner_headers)){
    df_flex <- flextable::style(df_flex,
                                i = rows_as_inner_headers,
                                pr_c = styles$body$cell_inner,
                                pr_p = styles$body$par,
                                pr_t = styles$header$text,
                                part = "body")
  }

  if(!is.null(rows_section_init)){
    df_flex <- flextable::style(df_flex,
                                i = rows_section_init,
                                pr_c = styles$body$cell,
                                pr_p = styles$body$par_left,
                                pr_t = styles$header$text,
                                part = "body")
  }

  #for dropouts
  if(!is.null(cells_color)){
    for (cell in cells_color){
      df_flex <- flextable::style(df_flex,
                                  i = cell$row,
                                  j = cell$column,
                                  pr_c = styles$body$cell_dr,
                                  pr_p = styles$body$par,
                                  pr_t = styles$body$text,
                                  part = "body")
    }
  }

  #bolding seelected borders
  if(!is.null(rows_bold_upper_bordered)){
    df_flex <- flextable::border(df_flex,
                                 i = rows_bold_upper_bordered,
                                 border.top = officer::fp_border(color = "black", width = 2L),
                                 part = "body")
  }

  #p-value colouring
  if(!is.null(rows_p_value_color)){
    for (row in rows_p_value_color){
      for (col in seq_len(ncol(df_flex$body$dataset))){
        if(grepl("(p \\= 0(,|\\.)0([0-4][0-9]?|05)|p < 0(,|\\.)001)", df_flex$body$dataset[row, col])){
          df_flex <- flextable::style(df_flex,
                                      i = row,
                                      j = col,
                                      pr_p = officer::fp_par(text.align = "center", padding.left = 2L, padding.right = 2L, padding.bottom = 0L, padding.top = 0L),
                                      pr_t =  officer::fp_text(font.size =  font, font.family = "Times New Roman", shading.color =  "#FF0000"), part = "body")
        }
      }
    }
  }

  # header styles ------------------------------------------------------------
  df_flex <- flextable::style(df_flex,
                              pr_c = styles$header$cell,
                              pr_p = styles$body$par,
                              pr_t = styles$header$text,
                              part = "header")


  # footer style -----------------------------------------------------------
  if(nrow(df_flex$footer$dataset) > 0){
    df_flex <- flextable::style(df_flex,
                                pr_c = styles$footer$cell,
                                pr_p = styles$footer$par,
                                pr_t = styles$body$text,
                                part = "footer")

    df_flex <- flextable::style(df_flex,
                                i = rows_as_footnote_title,
                                pr_c = styles$footer$cell,
                                pr_p = styles$footer$par,
                                pr_t = styles$footer$text,
                                part = "footer")

    df_flex <- flextable::border_outer(df_flex,
                                       border = officer::fp_border(color = "black", width = 1),
                                       part = "all")

  }

  return(df_flex)
}

#' @title Create a flextable object from data provided
#' @description Create a flextable object generated from data and settings provided
#'
#' @param df_struct a list including the data structure and all properties to represent these data with in a table. This data structure includes the next obligatory elements:
#' \describe{
#' \item{\strong{data}}{a dataframe prepared for conversion into the flextable object. It's up to user to format the dataframe with raw data into this format, because each statistical table has it's own unique format.}
#' }
#' The next elements of the \emph{df_struct} are optional, thus can omitted, and should be used for making the unique design of the generated flextable object. Each flextable object consists of \emph{header}, \emph{body} and \emph{footer} parts the detailed description of which can be found in the documentation of \emph{flextable} package.
#' \describe{
#' \item{\strong{header}}{a named list of ordered arrays representing the names of the columns. Each element in the list corresponds to the new row in a flextable header part. Any flextable object always consists of a header part. If no \emph{header} element is provided, then the names of the original dataframe are used for constructing the flextable object header}
#' \item{\strong{footer}}{a named list of ordered arrays representing the footer part of the flextable object. Each element in the list corresponds to the new row in a flextable footer part. A flextable object may include no footer part}
#' \item{\strong{rows_to_merge}}{indexes of rows to merge vertically in a body part}
#' \item{\strong{columns_to_merge}}{indexes of columns to merge horizontally in a body part}
#' \item{\strong{cells_to_merge}}{a list of elements represanting the rectangular area of the dataframe body to merge. Each element is a list consisting of two subelements: \emph{rows} and \emph{columns}, each of which is a range of rows or columns respectively defining the rectangular. All cells in either footer or header parts with the same value are merged automatically}
#' \item{\strong{table_width}}{an array of column widths. If omitted, columns widths are uniformly distributed provided that the table total width equals to 6.54 inches corresponding to a Word vertical oriented table}
#' \item{\strong{font}}{font size of all data in a table. If omitted, 10 points is used}
#' \item{\strong{rows_as_footnote_title}}{indexes of rows corresponding to the local titles which should be highlighted with italic font (Note, Designation, etc.) in a footer part. The value of this element doesn't effect on the generated flextable in case no footer is provided. If omitted, the first row is highlighted}
#' \item{\strong{columns_left_align}}{indexes of columns to left align in a body part}
#' \item{\strong{rows_as_inner_headers}}{indexes of rows corresponding to the names of measured clinical quantities in a body part and should be colored}
#' \item{\strong{rows_section_init}}{indexes of rows corresponding to the names of statistical methods and sections in a body part and should be highlighted}
#' \item{\strong{rows_bold_upper_bordered}}{indexes of rows with an upper border to highlight in a body part}
#' \item{\strong{cells_color}}{a list of cells to color where a cell is a list with two elements: \emph{row} and \emph{column}}
#' \item{\strong{rows_p_value_color}}{indexes of rows with p-value data to check for colouring}
#' }
#' @return The flextable object. Can be overviewed in the inbuilt \emph{Rstudio Viewer}.
#' @export
#'
#' @examples
#' # a table with a complex header and footer
#' t_data <- head(mtcars)
#' t_df_struct <- list(data = t_data,
#'                     header = list(row1 = rep("all columns", ncol(t_data)),
#'                                   row2 = c(rep("1st column groups", floor(ncol(t_data)/2)),
#'                                            rep("2st column groups", ceiling(ncol(t_data)/2))),
#'                                   row3 = names(t_data)),
#'                     footer = list(row1 = rep("Note:", ncol(t_data)),
#'                                   row2 = rep("Nothing special for this table", ncol(t_data))))
#' CreateFLX(t_df_struct)
#'
#'# a table with all possible design modifications
#'t_data <- dplyr::mutate_all(mtcars[1:3, 1:5], as.character) %>%
#'          dplyr::add_row(mpg = "population A", cyl = "population A", disp = "population A",
#'                         hp = "population A", drat = "population A")  %>%
#'          dplyr::bind_rows(dplyr::mutate_all(mtcars[4:6, 1:5], as.character)) %>%
#'          dplyr::add_row(mpg = "norm test:", cyl = "norm test:", disp = "norm test:",
#'                         hp = "norm test:", drat = "norm test:") %>%
#'          dplyr::add_row(mpg = "p = 0.43", cyl = "p < 0,001", disp = "p = 0,44",
#'                         hp = "p = 0.004", drat = "p = 0.002") %>%
#'          dplyr::bind_rows(dplyr::mutate_all(mtcars[7:9, 1:5], as.character)) %>%
#'          dplyr::add_row(mpg = "population B", cyl = "population B", disp = "population B",
#'                         hp = "population B", drat = "population B") %>%
#'          dplyr::bind_rows(dplyr::mutate_all(mtcars[10:12, 1:5], as.character))
#'t_df_struct <- list(data = t_data,
#'                    header = list(row1 = rep("all columns", ncol(t_data)),
#'                                  row2 = c(rep("1st column group", floor(ncol(t_data)/2)),
#'                                           rep("2st column group", ceiling(ncol(t_data)/2))),
#'                                  row3 = names(t_data)),
#'                    footer = list(row1 = rep("Note:", ncol(t_data)),
#'                                  row2 = rep("Nothing special for this table", ncol(t_data)),
#'                                  row3 = rep("Designation:", ncol(t_data)),
#'                                  row4 = rep("NA - not available", ncol(t_data))),
#'                    rows_to_merge = c(4, 8, 13),
#'                    cells_to_merge = list(rect1 = list(rows = 1:2, columns = 1),
#'                                          rect2 = list(rows = 14:15, columns = 4:5)),
#'                    columns_to_merge = 5,
#'                    table_width = NULL,
#'                    font = 12,
#'                    rows_as_footnote_title = c(1, 3),
#'                    rows_as_inner_headers = c(4, 13),
#'                    rows_section_init = 8,
#'                    columns_left_align = c(2, 4),
#'                    rows_bold_upper_bordered = 15,
#'                    cells_color = list(cell1 = list(row = 15, column = 1),
#'                                       cell2 = list(row = 16, column = 2)),
#'                    rows_p_value_color = 9)
#'CreateFLX(t_df_struct)

CreateFLX <- function(df_struct){
  if(is.null(df_struct$data)){
    stop(paste0("The structure with no data is sent into", deparse(sys.call())))
  }

  #default settings (constant structure)
  settings_default <- list(table_width = rep(6.54/ncol(df_struct$data), ncol(df_struct$data)),
                           font = 10L,
                           rows_as_footnote_title = 1L,
                           columns_left_align = NULL,
                           rows_as_inner_headers = NULL,
                           rows_section_init = NULL,
                           rows_to_merge = NULL,
                           columns_to_merge = NULL,
                           cells_to_merge = NULL,
                           rows_bold_upper_bordered = NULL,
                           cells_color = NULL,
                           rows_p_value_color = NULL
                           )

  #modify some of not specified settings of the given structure in accordance with default settings
  #note: the names of settings_default should correspond to the settings names of the given structure
  for (sett_name in names(settings_default)){
    if(is.null(df_struct[[sett_name]])){
      df_struct[[sett_name]] <- settings_default[[sett_name]]
    }
  }

  #initiate a flextable object
  df_flex <- flextable::regulartable(df_struct$data)

  #add header
  if(length(df_struct$header) > 1L){
    #the header consists of more then 1 rows
    typology_header <- tibble::as_tibble(df_struct$header) %>%
      tibble::add_column(col_keys = colnames(df_struct$data))

    #apply unique style
    df_flex <- flextable::set_header_df(df_flex, mapping = typology_header, key = "col_keys")
  }

  #add footer
  if(!is.null(df_struct$footer)){
    typology_footer <- tibble::as_tibble(df_struct$footer) %>%
      tibble::add_column(col_keys = colnames(df_struct$data))

    df_flex <- flextable::set_footer_df(df_flex, mapping = typology_footer, key = "col_keys")
  }

  df_flex <- df_flex %>%
    SetBorders() %>%
    SetStyle(rows_as_inner_headers = df_struct$rows_as_inner_headers,
             rows_section_init = df_struct$rows_section_init,
             rows_as_footnote_title = df_struct$rows_as_footnote_title,
             font = df_struct$font,
             columns_left_align = df_struct$columns_left_align,
             rows_bold_upper_bordered = df_struct$rows_bold_upper_bordered,
             cells_color = df_struct$cells_color,
             rows_p_value_color = df_struct$rows_p_value_color) %>%
    MergeCells(is_footer = !is.null(df_struct$footer),
               columns = df_struct$columns_to_merge,
               rows =  df_struct$rows_to_merge,
               cells = df_struct$cells_to_merge) %>%
    SetWidthCells(df_struct$table_width)

  return(df_flex)
}


#' @title Default styles for objects exporting
#' @description Store default styles settings for flextable or figures exporting into Word document
#'
#' @return set of styles
DocExportStyles <- function(){
  rez <- list()

  rez$header_style <- list(text = officer::fp_text(color = "black", font.size = 12, bold = TRUE, italic = FALSE, underlined = FALSE, font.family = "Times New Roman", vertical.align = "baseline", shading.color = "transparent"),
                           par = officer::fp_par(text.align = "center"))

  rez$body_style <- list(text = stats::update(rez$header_style$text, bold = FALSE))

  rez$figure_style <- list(height = 4, width = 6, aligned = "center")

  return(rez)
}

#' @title Export objects into a Word document
#' @description  Export flextable objects or ggplot2 figures into a Word document
#'
#' @param filename the name of the target Word document to create
#' @param tables a named list of flextable objects
#' @param tables_title a named list of table titles. The names should correspond to those of \emph{tables} parameter
#' @param figures a named list of \code{ggplot()} objects
#' @param figures_title a named list of figure titles. The names should correspond to those of \emph{figures} parameter
#' @param base_docx_filename the name of another Word file to inherit data from
#'
#' @return The message in console in case of successful Word document creation
#' @export
#'
#' @examples
#' #export simple flextable objects
#' t_flx_obj <- CreateFLX(list(data = mtcars[1:8,]))
#' t_flname <- paste0(getwd(), "/test_table.docx")
#' DocExport(filename = t_flname,
#'           tables = list(table1 = t_flx_obj),
#'           tables_title = list(table1 = "Test table"))
#' t_flx_obj_sec <- CreateFLX(list(data = mtcars[9:16,]))
#' t_flname_sec <- paste0(getwd(), "/test_table_second.docx")
#' DocExport(filename = t_flname_sec,
#'           tables = list(table2 = t_flx_obj_sec),
#'           tables_title = list(table2 = "Test table second"),
#'           base_docx_filename = t_flname)
DocExport <- function(filename, tables = NULL, tables_title = NULL, figures = NULL, figures_title = NULL, base_docx_filename = NULL){

  st <- DocExportStyles()

  #open a doc file to modify
  doc <- officer::read_docx(base_docx_filename)

  #in case this file was written to previously
  if(!is.null(base_docx_filename)) doc <- officer::body_add_break(doc)

  #tables insert
  if(!is.null(tables)){
    if(is.null(tables_title)){
      stop("There are no table titles provided")
    }

    #common piece of header name for all tables
    tables_header_common <- officer::ftext("Table XX.X. ", prop = st$header_style$text)

    for (table_name in names(tables)){
      #gather both parts of the header together
      header <- officer::fpar(tables_header_common,
                              officer::ftext(tables_title[[table_name]], prop = st$body_style$text),
                              fp_p = st$header_style$par)

      doc <- doc %>%
        officer::body_add_fpar(value = header) %>%
        flextable::body_add_flextable(value = tables[[table_name]], split = TRUE)

      if(which(names(tables) == table_name) != length(tables)){
        #dont add page break after the last table inserted
        doc <- officer::body_add_break(doc)
      }
    }
  }

  #insert figures
  if(!is.null(figures)){
    if(is.null(figures_title)){
      stop("There are no figures titles provided")
    }

    #common piece of header name for all tables
    figure_header_common <- officer::ftext("Figure XX.X. ", prop = st$header_style$text)

    for (figure_name in names(figures)){
      caption <- officer::fpar(figure_header_common,
                               officer::ftext(figures_title[[figure_name]], prop = st$body_style$text),
                               fp_p = st$header_style$par)
      doc <- doc %>%
        officer::body_add_gg(value = figures[[figure_name]],
                             width = st$figure_style$width,
                             height = st$figure_style$height,
                             style = st$figure_style$aligned) %>%
        officer::body_add_fpar(value = caption)

      if(which(names(figures) == figure_name) != length(figures)){
        #dont add page break after the last table inserted
        doc <- officer::body_add_break(doc)
      }
    }
  }

  #make a doc file
  print(doc, target = filename)

  message(sprintf("Word file %s was successfully created", filename))
  #TODO:add macros execution here: body_replace_all_text
}
