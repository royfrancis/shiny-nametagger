library(shiny)
library(bslib)
library(bsicons)
library(quarto)
library(markdown)
library(colourpicker)
library(shinyWidgets)
library(readr)

Sys.setlocale("LC_ALL", "en_US.UTF-8")
source("functions.r")

## ui --------------------------------------------------------------------------

ui <- page_fluid(
  title = "NBIS Nametagger",
  theme = bs_theme(preset = "zephyr", primary = "#A7C947"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  lang = "en",
  card(
    full_screen = TRUE,
    card_header(
      class = "app-card-header",
      tags$div(
        class = "app-header",
        span(tags$img(src = "logos/nbis-lime.png", style = "height:18px;"), style = "vertical-align:top;display:inline-block;"),
        span(tags$h5("•", style = "margin:0px;margin-left:6px;margin-right:6px;"), style = "vertical-align:top;display:inline-block;"),
        span(tags$h5("Nametagger", style = "margin:0px;"), style = "vertical-align:middle;display:inline-block;")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 305,
        div(style = "margin-top:5px;",
          accordion(
            open = TRUE,
            accordion_panel("Standard input",
              icon = bsicons::bs_icon("sliders"),
              layout_columns(
                selectInput("in_input", "Input method",
                  choices = c("Upload file", "Paste text"),
                  selected = "Paste text", multiple = FALSE
                ),
                tooltip(
                  selectInput("in_data_format", "File format", choices = c("tsv", "csv", "csv2"), selected = "csv", multiple = FALSE),
                  "Set delimiter. tsv: tab separated values (copy-paste from spreadsheet), csv: comma separated values, csv2: semicolon separated values",
                  placement = "top"
                )
              ),
              uiOutput("ui_input"),
              popover(
                div(class = "help-note", HTML("<span><i class='fa fa-circle-info'></i></span><span style='margin-left:5px;'>Content guide</span>")),
                includeMarkdown("help.md"),
                title = "Content guide"
              ),
              layout_columns(
                shinyWidgets::pickerInput("in_logo_left", "Logo left", choices = get_logos(), choicesOpt = list(content = logos$img), selected = 1, multiple = FALSE),
                shinyWidgets::pickerInput("in_logo_right", "Logo right", choices = get_logos(), choicesOpt = list(content = logos$img), selected = 1, multiple = FALSE),
              ),
              layout_columns(
                tooltip(
                  numericInput("in_logo_left_height", label = "Logo left height", value = 5.5, min = 0., max = 50, step = 0.1),
                  "Set height of left logo. Value between 0 and 50 mm.",
                  placement = "right"
                ),
                tooltip(
                  numericInput("in_logo_right_height", label = "Logo right height", value = 5.5, min = 0, max = 50, step = 0.1),
                  "Set height of right logo. Value between 0.1 and 50mm.",
                  placement = "right"
                )
              )
            )
          )
        ),
        div(
          accordion(
            open = FALSE,
            accordion_panel("More settings",
              icon = bsicons::bs_icon("gear-fill"),
              uiOutput("ui_bg"),
              layout_columns(
                tooltip(
                  numericInput("in_fontsize", label = "Font size", value = 16, min = 5, max = 30, step = 1),
                  "Set base font size. Value between 5 and 30 pt."
                ),
                tooltip(
                  numericInput("in_leading", label = "Line spacing", value = 0.5, min = 0.1, max = 1, step = 0.10),
                  "Set spacing between lines. Value between 0.1 and 1 em."
                )
              ),
              layout_columns(
                tooltip(
                  numericInput("in_text_pos_x", label = "Text pos x", value = 0, min = -100, max = 100, step = 1),
                  "Horizontal text position. Value between -100 and 100mm."
                ),
                tooltip(
                  numericInput("in_text_pos_y", label = "Text pos y", value = 0, min = -100, max = 100, step = 1),
                  "Vertical text position. Value between -100 and 100mm."
                )
              ),
              layout_columns(
                tooltip(
                  colourInput("in_col_text", "Text color", "#2e4053"),
                  "Color for all text. A hexadecimal value."
                ),
                tooltip(
                  colourInput("in_col_trim", "trim color", "#aeb6bf"),
                  "Color for dashed trim line. A hexadecimal value."
                )
              )
            )
          )
        ),
        actionButton("btn_run", "RUN", class = "btn-large"),
        layout_columns(
          style = "margin-top:5px;",
          tooltip(actionButton("btn_reset", "Reset", class = "btn-warning"), "Reset all inputs", placement = "bottom"),
          downloadButton("btn_download", "Download"),
          col_widths = c(4, 8)
        )
      ),
      uiOutput("out_pdf", width = "100%", height = "100%")
    ),
    card_footer(
      class = "app-footer",
      div(
        class = "help-note",
        paste0(format(Sys.time(), "%Y"), " Roy Francis • Version: ", fn_version()),
        HTML("• <a href='https://github.com/royfrancis/shiny-nametagger' target='_blank'><i class='fab fa-github'></i></a> • <a href='mailto:zydoosu@gmail.com' target='_blank'><i class='fa fa-envelope'></i></a>")
      )
    )
  )
)

## -----------------------------------------------------------------------------
## server ----------------------------------------------------------------------

server <- function(session, input, output) {
  ## content block -------------------------------------------------------------
  ## create temporary directory

  temp_dir <- tempdir(check = TRUE)
  temp_id <- paste(sample(letters, 10), collapse = "")
  temp_dir_active <- file.path(temp_dir, temp_id)
  cat(paste0("Working directory: ", temp_dir_active, "\n"))
  store <- reactiveValues(wd = temp_dir_active, id = temp_id, bg_path = NULL)
  if (!dir.exists(temp_dir_active)) dir.create(temp_dir_active)
  copy_dirs(temp_dir_active)
  addResourcePath(temp_id, temp_dir_active)

  ## UI: ui_input --------------------------------------------------------------
  ## ui to select input type: upload or paste

  output$ui_input <- renderUI({
    validate(fn_validate(input$in_input))

    if (input$in_input == "Upload file") {
      fileInput("in_data", "Upload a text file. Use UTF-8 encoding.", multiple = FALSE)
    } else {
      div(
        tooltip(
          textAreaInput("in_data", "Data", value = "line1,line2\nJohn Döe,Uppsala University\nMary Jane,Stockholm University", width = "100%", resize = "vertical", height = "100px"),
          "Input must contain columns named line1, line2 etc. Up to line5 is allowed. Use UTF-8 encoding.",
          placement = "right"
        )
      )
    }
  })

  ## UI ----------- ------------------------------------------------------------
  ## render ui for bg upload and bg state

  output$ui_bg <- renderUI({
    input$btn_reset
    tooltip(
      fileInput("in_bg", "Background image", multiple = FALSE, accept = c("image/png", "image/jpeg", "image/tiff", "image/gif"), width = "100%", placeholder = "Upload image"),
      "An image with dimensions close to nametag dimensions.",
      placement = "right"
    )
  })

  ## FN: fn_bg ---------------------------------------------------------------
  ## function to get bg

  fn_bg <- reactive({
    validate(fn_validate_im(input$in_bg))

    if (is.null(input$in_bg)) {
      store$bg_path <- NULL
    } else {
      ext <- tools::file_ext(input$in_bg$datapath)
      new_name <- paste0("bg-upload.", ext)
      if (file.exists(file.path(store$wd, new_name))) file.remove(file.path(store$wd, new_name))
      file.copy(input$in_bg$datapath, file.path(store$wd, new_name))
      store$bg_path <- list(path = new_name)
    }
  })

  ## FN: fn_input --------------------------------------------------------
  ## function to get input data

  fn_input <- reactive({
    validate(fn_validate(input$in_input))
    validate(fn_validate(try(input$in_data), message1 = "Upload a file or paste text."))
    validate(fn_validate(input$in_data_format))
    fr <- ifelse(input$in_data_format == "tsv", "\t", ifelse(input$in_data_format == "csv", ",", ";"))

    list_info <- NULL
    if (input$in_input == "Upload file") {
      tryCatch({
          dfr <- readr::read_delim(input$in_data$datapath, delim = fr, locale = locale(encoding = "UTF-8"))
      },error = function(e) {
          print("Error in reading data. Check input. Headers must be present.")
      })
    } else if (input$in_input == "Paste text") {
      tryCatch({
        dfr <- readr::read_delim(input$in_data, delim = fr, locale = locale(encoding = "UTF-8"))
      },error = function(e) {
        print("Error in reading data. Check input. Headers must be present.")
      })
    }

    # add flag icon if line5 is a valid country code
    if ("line5" %in% names(dfr)) {
      matched_indices <- match(dfr$line5, cou$code) # Match once and use the indices
      valid_matches <- !is.na(matched_indices) # Logical vector for valid matches

      dfr$icon <- ifelse(valid_matches, cou$path[matched_indices], "")
      dfr$line5 <- ifelse(valid_matches, cou$name[matched_indices], dfr$line5)
    }

    # convert to list
    list_info <- as.list(apply(dfr, 1, function(row) {
      as.list(setNames(as.character(row), names(dfr)))
    }))

    return(list_info)
  })

  ## FN: fn_vars ---------------------------------------------------------------
  ## function to get meta variables

  fn_vars <- reactive({
    validate(fn_validate(try(fn_input()), message1 = "No input data. Check input."))
    v_info <- fn_input()

    if (is.null(input$in_fontsize)) {
      v_fontsize <- "16pt"
    } else {
      validate(fn_validate_numeric(input$in_fontsize))
      v_fontsize <- paste0(input$in_fontsize, "pt")
    }

    if (is.null(input$in_leading)) {
      v_leading <- "0.5em"
    } else {
      validate(fn_validate_numeric(input$in_leading))
      v_leading <- paste0(input$in_leading, "em")
    }

    if (is.null(input$in_col_trim)) {
      v_color_trim <- "#aeb6bf"
    } else {
      v_color_trim <- input$in_col_trim
    }

    if (is.null(input$in_col_text)) {
      v_color_text <- "#2e4053"
    } else {
      v_color_text <- input$in_col_text
    }

    if (is.null(input$in_text_pos_x)) {
      v_text_pos_x <- "0mm"
    } else {
      validate(fn_validate_numeric(input$in_text_pos_x))
      v_text_pos_x <- paste0(input$in_text_pos_x, "mm")
    }

    if (is.null(input$in_text_pos_y)) {
      v_text_pos_y <- "0mm"
    } else {
      validate(fn_validate_numeric(input$in_text_pos_y))
      v_text_pos_y <- paste0(input$in_text_pos_y, "mm")
    }

    if (!is.null(input$in_logo_right) && input$in_logo_right != "") {
      v_logo_right <- list(path = input$in_logo_right)
    } else {
      v_logo_right <- ""
    }

    if (!is.null(input$in_logo_left) && input$in_logo_left != "") {
      v_logo_left <- list(path = input$in_logo_left)
    } else {
      v_logo_left <- ""
    }

    if (is.null(input$in_logo_left_height)) {
      v_logo_left_height <- "5.5mm"
    } else {
      validate(fn_validate_numeric(input$in_logo_left_height))
      v_logo_left_height <- paste0(input$in_logo_left_height, "mm")
    }

    if (is.null(input$in_logo_right_height)) {
      v_logo_right_height <- "5.5mm"
    } else {
      validate(fn_validate_numeric(input$in_logo_right_height))
      v_logo_right_height <- paste0(input$in_logo_right_height, "mm")
    }

    fn_bg()
    if (is.null(store$bg_path)) {
      v_bg_image <- ""
    } else {
      v_bg_image <- list(path = store$bg_path)
    }


    return(list(
      info = v_info,
      "bg-image" = v_bg_image,
      "logo-left" = v_logo_left,
      "logo-right" = v_logo_right,
      "logo-left-height" = v_logo_left_height,
      "logo-right-height" = v_logo_right_height,
      "font-size" = v_fontsize,
      leading = v_leading,
      "text-color" = v_color_text,
      "trim-color" = v_color_trim,
      "text-pos-x" = v_text_pos_x,
      "text-pos-y" = v_text_pos_y,
      "bg-image" = store$bg_path,
      version = fn_version()
    ))
  })

  ## ER: run button binding -------------------------------------------------

  evr_run <- eventReactive(input$btn_run, {
    return(fn_vars())
  })

  ## FN: fn_build -------------------------------------------------------------
  ## function to create pdf

  fn_build <- reactive({
    vars <- evr_run()
    print(vars)
    validate(fn_validate(vars))

    progress_plot <- shiny::Progress$new()
    progress_plot$set(message = "Creating PDF ...", value = 0.1)

    output_file <- "nametag.pdf"
    ppath <- store$wd
    if (file.exists(file.path(ppath, output_file))) file.remove(file.path(ppath, output_file))
    quarto::quarto_render(input = file.path(ppath, "nametag.qmd"), metadata = vars)

    progress_plot$set(message = "Rendering PDF ...", value = 1)
    progress_plot$close()
  })

  ## OUT: out_pdf -------------------------------------------------------------
  ## plots figure

  output$out_pdf <- renderUI({
    if (input$btn_run == 0) {
      return(div(p("Click 'RUN' to generate PDF.")))
    } else {
      fn_build()
      return(tags$iframe(src = file.path(store$id, "nametag.pdf"), height = "100%", width = "100%"))
    }
  })

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  output$btn_download <- downloadHandler(
    filename = "nametag.pdf",
    content = function(file) {
      fn_build()
      cpath <- file.path(store$wd, "nametag.pdf")
      file.copy(cpath, file, overwrite = T)
      unlink(cpath)
    }
  )

  ## OBS: btn_reset ------------------------------------------------------------
  ## observer for reset

  observeEvent(input$btn_reset, {
    updateSelectInput(session, "in_input", "Input method", choices = c("Upload file", "Paste text"), selected = "Paste text")
    updateSelectInput(session, "in_data_format", "File format", choices = c("tsv", "csv", "csv2"), selected = "csv")
    updateTextAreaInput(session, "in_data", "Data", value = "line1,line2\nJohn Doe,Uppsala University\nMary Jane,Stockholm University")
    updatePickerInput(session, "in_logo_left", "Logo left", choices = get_logos(), choicesOpt = list(content = logos$img), selected = 1)
    updatePickerInput(session, "in_logo_right", "Logo right", choices = get_logos(), choicesOpt = list(content = logos$img), selected = 1)
    updateNumericInput(session, "in_logo_left_height", "Logo left height", value = 5.5)
    updateNumericInput(session, "in_logo_right_height", "Logo right height", value = 5.5)
    updateNumericInput(session, "in_fontsize", "Font size", value = 16)
    updateNumericInput(session, "in_leading", "Line spacing", value = 0.5)
    updateColourInput(session, "in_col_text", "Text color", "#2e4053")
    updateColourInput(session, "in_col_trim", "trim color", "#aeb6bf")
    updateNumericInput(session, "in_text_pos_x", "Text pos x", value = 0)
    updateNumericInput(session, "in_text_pos_y", "Text pos y", value = 0)
    store$bg_path <- NULL
  })

  ## OSE -----------------------------------------------------------------------
  ## delete user directory when session ends

  session$onSessionEnded(function() {
    cat(paste0("Removing working directory: ", isolate(store$wd), " ...\n"))
    if (dir.exists(isolate(store$wd))) {
      unlink(isolate(store$wd), recursive = TRUE)
    }
  })
}

## launch ----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
