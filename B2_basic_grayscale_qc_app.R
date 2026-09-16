# ============================================================
# Grayscale QC - Basic Shiny App
# Purpose: show how reusable functions are connected in Shiny
# ============================================================

library(shiny)
library(rlang)

source("~/Downloads/B1_grayscale_qc_functions.R")

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Abnormal Page Break Detector - Basic Version"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload PDF/RTF file"),

      radioButtons(
        "scan_mode",
        "Scan Mode",
        choices = c(
          "Quick (Pages 2-21)" = "quick",
          "Custom Range" = "range",
          "Full (Pages 2-End)" = "full"
        ),
        selected = "full"
      ),

      uiOutput("range_ui"),
      actionButton("run", "Start Scan"),
      tags$hr(),

      numericInput("dpi", "DPI", 120, min = 72),
      sliderInput("resize_pct", "Resize (%)", 10, 100, 25),
      numericInput("white_thr", "White Threshold", 245, 0, 255),
      numericInput(
        "sus_thr",
        "Primary Threshold: bottom/right white ratio >",
        0.98,
        0,
        1,
        0.01
      ),
      numericInput(
        "w_thr",
        "Secondary Threshold: overall white ratio >",
        0.95,
        0,
        1,
        0.01
      ),
      numericInput("pct_region", "Detection Region (%)", 50, 0, 100, 1),
      numericInput("top_crop_thr", "Top Margin Crop (%)", 6, 0, 100),
      numericInput("bot_crop_thr", "Bottom Margin Crop (%)", 6, 0, 100),
      numericInput("side_crop_thr", "Left/Right Margin Crop (%)", 8, 0, 100)
    ),

    mainPanel(
      uiOutput("note"),
      tags$hr(),
      uiOutput("sus_ui"),
      h4("QC Metrics Table"),
      tableOutput("qc_table")
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {

  # 1. Convert the uploaded file into a PDF path used by the app
  pdf_path <- reactive({
    req(input$file)

    ext <- tolower(tools::file_ext(input$file$name))

    if (ext == "pdf") {
      input$file$datapath
    } else if (ext == "rtf") {
      rtf_to_pdf(input$file$datapath)
    } else {
      stop("Only PDF or RTF file is supported")
    }
  })

  # 2. Get total number of pages
  n_pages <- reactive({
    pdf_n_pages(pdf_path())
  })

  output$note <- renderUI({
    req(pdf_path())

    tagList(
      tags$div(
        style = "color:#666;",
        paste0("Total pages: ", n_pages())
      ),
      tags$div(
        style = "color:#666;",
        "*Note: Mean and Standard Deviation (SD) are reference metrics only."
      )
    )
  })

  # 3. Show custom range inputs only when needed
  output$range_ui <- renderUI({
    req(pdf_path())

    if (input$scan_mode != "range") return(NULL)

    n <- n_pages()

    tagList(
      numericInput("scan_from", "From", 2, min = 1),
      numericInput("scan_to", "To", n, min = 1)
    )
  })

  # 4. Decide which pages to scan
  scan_pages <- reactive({
    n <- n_pages()

    if (input$scan_mode == "range") {
      from <- max(1, as.integer(input$scan_from %||% 2))
      to <- max(from, as.integer(input$scan_to %||% from))

      if (!is.na(n)) to <- min(to, n)
      return(seq(from, to))
    }

    if (input$scan_mode == "quick") {
      if (!is.na(n)) return(seq(2, min(n, 21)))
      return(2:21)
    }

    # Full mode
    req(!is.na(n))

    if (n < 2) {
      integer(0)
    } else {
      2:n
    }
  })

  # 5. Run page-level QC only when Start Scan is clicked
  qc_df <- eventReactive(input$run, {
    path <- pdf_path()
    pages <- scan_pages()

    if (!length(pages)) return(data.frame())

    results <- lapply(pages, function(p) {
      tryCatch(
        analyze_page(
          path = path,
          page = p,
          dpi = input$dpi,
          resize_pct = input$resize_pct,
          white_thr = input$white_thr,
          sus_thr = input$sus_thr,
          w_thr = input$w_thr,
          top_crop_thr = input$top_crop_thr,
          bot_crop_thr = input$bot_crop_thr,
          side_crop_thr = input$side_crop_thr,
          pct_region = input$pct_region
        ),
        error = function(err) {
          list(
            page = p,
            mean_c = NA,
            sd_c = NA,
            bottom_white_ratio = NA,
            right_white_ratio = NA,
            overall_white_ratio = NA,
            status = paste("ERROR:", err$message)
          )
        }
      )
    })

    do.call(
      rbind,
      lapply(results, function(x) {
        data.frame(
          Page = x$page,
          "Bottom White Ratio" = x$bottom_white_ratio,
          "Right White Ratio" = x$right_white_ratio,
          "Overall White Ratio" = x$overall_white_ratio,
          "Mean*" = x$mean_c,
          "SD*" = x$sd_c,
          Status = x$status,
          check.names = FALSE
        )
      })
    )
  })

  # 6. Display QC results
  output$qc_table <- renderTable({
    req(qc_df())
    qc_df()
  })

  # 7. Pull suspicious pages from the QC table
  sus_pages <- reactive({
    df <- qc_df()

    if (!nrow(df)) return(integer(0))
    df$Page[df$Status == "CHECK"]
  })

  output$sus_ui <- renderUI({
    req(qc_df())

    sp <- sus_pages()

    if (!length(sp)) {
      return(tags$div("Suspicious pages: none"))
    }

    tags$div(
      tags$b("Suspicious pages: "),
      paste(sp, collapse = ", ")
    )
  })
}

shinyApp(ui = ui, server = server)
