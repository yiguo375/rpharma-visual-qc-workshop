library(shiny)
library(magick)
library(base64enc)
library(rlang)   
library(pdftools)
library(shinyscreenshot)

#convert RFT to PDF (LibreOffice)
rtf_to_pdf <- function(rtf_path){
  soffice <- "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  if (!file.exists(soffice)) stop("LibreOffice soffice not found")
  
  tmp <- tempfile(fileext = ".rtf")
  file.copy(rtf_path, tmp, overwrite = TRUE)
  
  out_dir <- tempfile("rtf_to_pdf_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  res <- suppressWarnings(system2(
    soffice,
    args = c(
      "--headless","--nologo","--nolockcheck",
      "--convert-to","pdf:writer_pdf_Export",
      "--outdir", out_dir,
      normalizePath(tmp, winslash = "/", mustWork = TRUE)
    ),
    stdout = TRUE, stderr = TRUE
  ))
  
  pdfs <- list.files(out_dir, pattern="\\.pdf$", full.names = TRUE)
  if (!length(pdfs)) stop(paste0("RTF to PDF conversion failed:\n", paste(res, collapse = "\n")))
  pdfs[1]
}


#get total number of pages in a PDF file
pdf_n_pages <- function(path) {
  pdftools::pdf_info(path)$pages
}


#page rasterization and downsampling
raster_resize <- function(path, page, dpi, resize_pct){
  #page rasterization
  img <- image_read_pdf(path, pages = page, density = dpi)
  
  #image downsampling
  if (!is.null(resize_pct) && resize_pct > 0 && resize_pct < 100) {
    img <- image_resize(img, paste0(resize_pct, "%"))
  }
  img
}

#grayscale conversion 
to_gray <- function(img){
  image_convert(img, colorspace = "gray")
}

#margin cropping
crop_margins <- function(gray_img, top_crop_thr, bot_crop_thr, side_crop_thr) {
  
  info <- image_info(gray_img)
  w <- info$width
  h <- info$height
  
  top_h  <- max(1, floor(h * top_crop_thr / 100))
  bot_h  <- max(1, floor(h * bot_crop_thr / 100))
  side_w <- max(1, floor(w * side_crop_thr / 100))
  
  image_crop(
    gray_img,
    geometry_area(
      w - 2 * side_w,
      h - top_h - bot_h,
      side_w,
      top_h
    )
  )
}


#pixel values extraction
gray_vals <- function(gray_img){
  a <- image_data(gray_img, channels = "gray")
  strtoi(as.vector(a[1, , ]), 16L)
}

#vertical and horizontal ink profiles from grayscale image
ink_profiles <- function(gray_img){
  a <- image_data(gray_img, channels = "gray")
  
  #a[1,,] is width x height
  mat <- apply(a[1, , ], c(1, 2), function(x) strtoi(x, 16L))
  
  #ink = 255 - gray
  ink <- 255 - mat
  
  list(
    vertical = apply(ink, 2, mean),    #from top to bottom
    horizontal = apply(ink, 1, mean)   #from left to right
  )
}

#pixel-density metrics 
analyze_page <- function(path, page, dpi, resize_pct, white_thr, sus_thr, w_thr, top_crop_thr, bot_crop_thr, side_crop_thr, pct_region){
  img <- raster_resize(path, page, dpi, resize_pct)
  g <- to_gray(img)
  c_img <- crop_margins(
    g,
    top_crop_thr,
    bot_crop_thr,
    side_crop_thr
  )
  
  c_info <- image_info(c_img)
  cw <- c_info$width
  ch <- c_info$height
  
  #center region as numeric pixel values
  center <- gray_vals(c_img)
  
  #reference metrics from cropped center region
  mean_c <- mean(center)
  sd_c <- sd(center)
  
  #secondary indicator from cropped center region
  overall_white_ratio <- mean(center > white_thr)
  
  #primary indicators from cropped center region
  bot <- gray_vals(
    image_crop(c_img, geometry_area(cw, floor(ch * pct_region / 100), 0, ch - floor(ch * pct_region / 100)))
  )
  
  rgt <- gray_vals(
    image_crop(c_img, geometry_area(floor(cw * pct_region / 100), ch, cw - floor(cw * pct_region / 100), 0))
  )
  
  bottom_white_ratio <- mean(bot > white_thr)
  right_white_ratio  <- mean(rgt > white_thr)

  #page detection
  status <- if (
    bottom_white_ratio > sus_thr ||
    right_white_ratio  > sus_thr ||
    overall_white_ratio > w_thr
  ) "CHECK" else "OK"
  

  prof <- ink_profiles(c_img)
  
  list(
    page = page,
    mean_c = mean_c,
    sd_c = sd_c,
    overall_white_ratio = overall_white_ratio,
    bottom_white_ratio = bottom_white_ratio,
    right_white_ratio  = right_white_ratio,
    status = status,
    vertical_profile = prof$vertical,
    horizontal_profile = prof$horizontal
  )
}


# ---- UI ----
ui <- fluidPage(
  tags$style(HTML("
  .viewer-box{
    height:50vh;
    overflow:auto;
    display:block;
    width:100%;
    border:1px solid #ddd;
    padding:8px;
    background:#fafafa;
    text-align:center;
  }
  .viewer-inner{
    display:inline-block;
    min-width:100%;
  }
  .viewer-img{
    display:block;
    height:auto;
    margin:auto;
    max-width:none;
  }
  .viewer-toolbar{
    display:flex;
    justify-content:flex-end;
    gap:8px;
    flex-wrap:wrap;
    margin:8px 0;
  }
  .viewer-footer{margin-top:10px;}
  .viewer-footer .form-group{margin-bottom:8px;}
  .top-sus{margin-bottom:10px;}
  .section-gap{margin-top:14px;}
")),
    
  titlePanel("Abnormal Page Break Detector"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload PDF/RTF file"),
      radioButtons(
        "scan_mode", "Scan Mode",
        choices = c(
          "Quick (Pages 2-21)" = "quick",
          "Custom Range" = "range",
          "Full (Pages 2-End)" = "full"
        ),
        selected = "full"
      ),
      actionButton("run", "Start Scan"), br(), br(),
      uiOutput("range_ui"),
      tags$hr(),
      numericInput("dpi", "DPI", 120, min = 72),
      sliderInput("resize_pct", "Resize (%)", 10, 100, 25),

      numericInput("white_thr", "White Threshold", 245, 0, 255),
      numericInput("sus_thr", "Primary Threshold: bottom/right white ratio >", 0.98, 0, 1, 0.01),
      numericInput("w_thr", "Secondary Threshold: overall white ratio >", 0.95, 0, 1, 0.01),
      
      numericInput("pct_region", "Detection Region (%)", 50, 0, 100, 1),
      numericInput("top_crop_thr", "Top Margin Crop (%)", 6, 0, 100),
      numericInput("bot_crop_thr", "Bottom Margin Crop (%)", 6, 0, 100),
      numericInput("side_crop_thr", "Left/Right Margin Crop (%)", 8, 0, 100),
      
      actionButton("go", "Take a screenshot")
  
    ),
    
    mainPanel(
      div(class = "top-sus", uiOutput("sus_ui")),
      
      h4("Viewer"),
      checkboxInput("sus_only", "Show suspicious pages only", FALSE),
      sliderInput("viewer_zoom", "Viewer Zoom (%)", 30, 150, 50, 5),
      div(
        class = "viewer-toolbar",
        div(
          actionButton("prev_page", "◀ Previous Page"),
          actionButton("next_page", "Next Page ▶")
        ),
        div(
          actionButton("prev_sus", "◀ Previous Suspicious"),
          actionButton("next_sus", "Next Suspicious ▶")
        )
      ),
      uiOutput("viewer_ui"),
      div(
        class = "viewer-footer",
        numericInput("viewer_page", "Page Number", 1, min = 1)
      ),
      
      hr(class = "section-gap"),
      h4("QC Metrics Table"),
      tags$details(
        uiOutput("note"),
        tableOutput("qc_table")
      ),
      
      hr(),
      h4("Dark Pixel Density Projections"),
      tags$details(
        numericInput("pro_page", "Selected Page Distributions (available only if scanned)", 2, min = 1), 
        plotOutput("proj_plot", height = "500px")
      ),
      
      imageOutput("preview", width = "50%", height = "200")
      
    )
  )
)

# ---- Server ----
server <- function(input, output, session){
  
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
  
  n_pages <- reactive(pdf_n_pages(pdf_path()))
  
  output$note <- renderUI({
    req(pdf_path())
    n <- n_pages()
    
    tagList(
      if (!is.na(n)) {
        tags$div(style = "color:#666;", paste0("Total pages: ", n))
      },
      
      tags$div(style = "color:#666;", "*Note: Mean and Standard Deviation (SD) are reference metrics only.")
    )
  })
  
  output$range_ui <- renderUI({
    req(pdf_path())
    if (input$scan_mode != "range") return(NULL)
    n <- n_pages()
    tagList(
      numericInput("scan_from", "From", 2, min = 1), # 1 is the min value the user can enter
      numericInput("scan_to", "To", n , min = 1) # 1 is the min value the user can enter
    )
  })
  
  scan_pages <- reactive({
    n <- n_pages()
    
    if (input$scan_mode == "range"){
      from <- max(1, as.integer(input$scan_from %||% 2))
      to   <- max(from, as.integer(input$scan_to %||% from))
      if (!is.na(n)) to <- min(to, n)
      return(seq(from, to))
    }
    
    else if (input$scan_mode == "quick"){
      if (!is.na(n)) return(seq(2, min(n, 21)))
      return(2:21)
    }
    
    
    # full mode 
    else {
      req(!is.na(n))
      
      if (n < 2) {
        return(integer(0))
      } else {
        return(2:n)
      }
    }
  })
  
  qc_cache <- reactiveVal(NULL)
  
  qc_df <- eventReactive(input$run, {
    path  <- pdf_path()
    pages <- scan_pages()
    if (!length(pages)) return(data.frame())
    
    lst <- lapply(pages, function(p){
      tryCatch(
        analyze_page(
          path = path,
          page = p,
          dpi = input$dpi,
          resize_pct = input$resize_pct,
          pct_region = input$pct_region,
          white_thr = input$white_thr,
          sus_thr = input$sus_thr,
          w_thr = input$w_thr,
          top_crop_thr = input$top_crop_thr,
          bot_crop_thr = input$bot_crop_thr,
          side_crop_thr = input$side_crop_thr
        ),
        error = function(err) list(
          page = p,
          mean_c = NA,
          sd_c = NA,
          bottom_white_ratio = NA,
          right_white_ratio = NA,
          overall_white_ratio = NA,
          status = paste("ERROR:", e$message),
          vertical_profile = numeric(0),
          horizontal_profile = numeric(0)
        )
      )
    })
    
    qc_cache(lst)
    
    do.call(rbind, lapply(lst, function(x) data.frame(
       Page = x$page,
      'Bottom White Ratio' = x$bottom_white_ratio,
      'Right White Ratio' = x$right_white_ratio,
      'Overall White Ratio' = x$overall_white_ratio,
      'Mean*' = x$mean_c,
      'SD*' = x$sd_c,
       Status = x$status,
      check.names = FALSE # Keep column names as written
    )))
  })
  
  output$qc_table <- renderTable({
    req(qc_df())
    qc_df()
  })
  
  # All suspicious page numbers
  sus_pages <- reactive({
    df <- qc_df() # QC summary table
    if (!nrow(df)) return(integer(0))
    df$Page[df$Status == "CHECK"]
  })
  
  output$sus_ui <- renderUI({
    req(qc_df())
    sp <- sus_pages()
    if (!length(sp)) return(tags$div("Suspicious pages: none"))
    tagList(
      tags$div(paste0("Suspicious pages: ", paste(sp, collapse = ", "))),
      selectInput("sus_pick", "Select suspicious page", choices = sp, selected = sp[1]),
      actionButton("goto_sus", "Go to Viewer")
    )
  })
  
  #check 
  observe({
    req(input$sus_pick)
    print(class(sus_pages()))
    print(typeof(sus_pages()))
    
    print(class(input$sus_pick))
    print(typeof(input$sus_pick))
  })
  
  observeEvent(input$goto_sus, {
    p <- as.integer(input$sus_pick)  # Convert the selected page from character to integer
    if (is.na(p)) return(NULL)
    updateNumericInput(session, "viewer_page", value = p)
    updateNumericInput(session, "pro_page", value = p)
  })
  
  # ---- Viewer navigation ----
  viewer_list <- reactive({
    if (!isTRUE(input$sus_only)){ # Checkbox
      n <- n_pages()
      if (!is.na(n)) return(seq_len(n))
      return(NULL)
    }
    req(qc_df())
    sus_pages()
  })
  
  viewer_idx <- reactiveVal(1)
  
  observeEvent(input$viewer_page, {
    lst <- viewer_list()
    p <- as.integer(input$viewer_page %||% 1) #NULL
    if (is.na(p)) p <- 1 #NA
    
    # viewer_page changed
    # User selected a page -> preserve that page if list is not ready
    if (is.null(lst)) {
      viewer_idx(p) # NULL: page list is not available yet, so keep the current page
      return()
    }
    
    # empty list 
    if (!length(lst)) {
      viewer_idx(1) # page list is available but empty, so reset to index 1
      return()
    }
    
    # Get the index of page p in lst
    i <- match(p, lst)  
    if (is.na(i)) i <- 1
    
    viewer_idx(i)
    updateNumericInput(session, "viewer_page", value = lst[i])
  }, ignoreInit = TRUE)
  
  observeEvent(input$sus_only, {
    lst <- viewer_list()
    if (is.null(lst)) return()
    if (!length(lst)) {
      viewer_idx(1)
      updateNumericInput(session, "viewer_page", value = 1)
      return()
    }
    viewer_idx(1)
    updateNumericInput(session, "viewer_page", value = lst[1])
  }, ignoreInit = TRUE)
  
  observeEvent(input$prev_page, {
    lst <- viewer_list()
    if (is.null(lst)){
      p <- max(1, viewer_idx() - 1)
      viewer_idx(p)
      updateNumericInput(session, "viewer_page", value = p) # No lst available 
      return()
    }
    if (!length(lst)) return()
    i <- max(1, viewer_idx() - 1)
    viewer_idx(i)
    updateNumericInput(session, "viewer_page", value = lst[i])
  })
  
  observeEvent(input$next_page, {
    lst <- viewer_list()
    if (is.null(lst)){
      n <- n_pages()
      p <- viewer_idx() + 1
      if (!is.na(n)) p <- min(p, n)
      viewer_idx(p)
      updateNumericInput(session, "viewer_page", value = p)
      return()
    }
    if (!length(lst)) return()
    i <- min(length(lst), viewer_idx() + 1)
    viewer_idx(i)
    updateNumericInput(session, "viewer_page", value = lst[i])
  })
  
  observeEvent(input$prev_sus, {
    req(qc_df())
    sp <- sus_pages()
    if (!length(sp)) return()
    
    p <- as.integer(input$viewer_page %||% 1)
    prev_candidates <- sp[sp < p]
    
    target <- if (length(prev_candidates)) {
      max(prev_candidates)
    } else {
      sp[1] # No earlier suspicious pages; stay on the first one
    }
    
    updateNumericInput(session, "viewer_page", value = target)
    updateNumericInput(session, "pro_page", value = target)
  })
  
  observeEvent(input$next_sus, {
    req(qc_df())
    sp <- sus_pages()
    if (!length(sp)) return()
    
    p <- as.integer(input$viewer_page %||% 1)
    next_candidates <- sp[sp > p]
    
    target <- if (length(next_candidates)) {
      min(next_candidates)
    } else {
      sp[length(sp)] # No more suspicious pages; stay on the last one
    }
    
    updateNumericInput(session, "viewer_page", value = target)
    updateNumericInput(session, "pro_page", value = target)
  })
  
  output$viewer_ui <- renderUI({
    req(pdf_path())
    path <- pdf_path()
    n    <- n_pages()
    lst  <- viewer_list()
    zoom <- as.integer(input$viewer_zoom %||% 100)
    
    if (isTRUE(input$sus_only)){
      req(qc_df())
      if (is.null(lst) || !length(lst)) {
        return(tags$div("No suspicious pages available: scan first or uncheck 'Show suspicious pages only'."))
      }
      i <- max(1, min(viewer_idx(), length(lst)))
      p <- lst[i]
      label <- paste0("Suspicious pages only: ", i, "/", length(lst), " (Page ", p, ")")
    } else {
      p <- as.integer(input$viewer_page %||% 1)
      if (is.na(p)) p <- 1
      if (!is.na(n)) p <- min(p, n)
      label <- if (!is.na(n)) paste0("Page ", p, " / ", n) else paste0("Page ", p)
    }
    
    img <- tryCatch(
      raster_resize(path, p, input$dpi, input$resize_pct),
      error = function(err) NULL
    )
    if (is.null(img)) {
      return(tags$div(style = "color:#c00;", paste0("Failed to read page ", p, ".")))
    }
    
    tmp <- tempfile(fileext = ".png")
    image_write(img, tmp, format = "png")
    img_b64 <- base64encode(tmp) # Convert PNG file to Base64 for HTML display
    
    tagList(
      tags$div(style = "color:#666; margin-bottom:6px;", label),
      div(
        class = "viewer-box",
        div(
          class = "viewer-inner",
          tags$img(
            src = paste0("data:image/png;base64,", img_b64), 
            class = "viewer-img",
            #style = paste0("width:", zoom, "%; height:auto;")
            style = paste0("width:", zoom, "%; height:auto; max-width:none;")
          )
        )
      )
    )
  })
  
  output$proj_plot <- renderPlot({
    req(qc_cache())
    lst <- qc_cache()
    p <- as.integer(input$pro_page)
    if (is.na(p)) return(NULL)
    
    i <- which(vapply(lst, function(x) x$page, integer(1)) == p) # Find page p in QC cache
    if (!length(i)) return(NULL)
    
    v_prof <- lst[[i[1]]]$vertical_profile
    h_prof <- lst[[i[1]]]$horizontal_profile
    
    req(length(v_prof), length(h_prof))
    
    op <- par(mfrow = c(2, 1), mar = c(2, 2, 2, 2)) # Set 2-row plot layout and margins
    #on.exit(par(op)) # Restore original plot settings
    
    plot(
      v_prof,
      type = "l",
      lwd = 2,
      main = paste("Page", p, "Vertical Ink Distribution"),
      xlab = "Position from Top to Bottom",
      ylab = "Mean Ink Density"
    )
    
    plot(
      h_prof,
      type = "l",
      lwd = 2,
      main = paste("Page", p, "Horizontal Ink Distribution"),
      xlab = "Position from Left to Right",
      ylab = "Mean Ink Density"
    )
    
    observeEvent(input$go, {
      screenshot()
    })
    
  })
}

shinyApp(ui = ui, server = server)
