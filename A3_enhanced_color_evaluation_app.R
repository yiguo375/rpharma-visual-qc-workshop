library(shiny)
library(farver)
library(colorspace)
library(dichromat)

#--------------------------------------------------
# Step 1: Helper functions
#--------------------------------------------------

relative_luminance <- function(hex) {
  rgb <- hex2RGB(hex)@coords[1, ]
  
  f <- function(x) {
    ifelse(
      x <= 0.03928,
      x / 12.92,
      ((x + 0.055) / 1.055)^2.4
    )
  }
  
  0.2126 * f(rgb[1]) +
    0.7152 * f(rgb[2]) +
    0.0722 * f(rgb[3])
}


contrast_ratio <- function(hex1, hex2) {
  L1 <- relative_luminance(hex1)
  L2 <- relative_luminance(hex2)
  
  (max(L1, L2) + 0.05) /
    (min(L1, L2) + 0.05)
}


contrast_vs_white <- function(hex) {
  contrast_ratio(hex, "#FFFFFF")
}


# Luminance difference
deltaL <- function(cols) {
  lum <- sapply(cols, relative_luminance)
  
  outer(
    lum,
    lum,
    function(a, b) abs(a - b)
  )
}


deltaE2000 <- function(cols) {
  rgb_mat <- decode_colour(cols)
  
  compare_colour(
    rgb_mat,
    from_space = "rgb",
    method = "cie2000"
  )
}


deltaE2000_protan <- function(cols) {
  cb_cols <- dichromat(cols, type = "protan")
  rgb_mat <- decode_colour(cb_cols)
  
  compare_colour(
    rgb_mat,
    from_space = "rgb",
    method = "cie2000"
  )
}


deltaE2000_deutan <- function(cols) {
  cb_cols <- dichromat(cols, type = "deutan")
  rgb_mat <- decode_colour(cb_cols)
  
  compare_colour(
    rgb_mat,
    from_space = "rgb",
    method = "cie2000"
  )
}


deltaE2000_tritan <- function(cols) {
  cb_cols <- dichromat(cols, type = "tritan")
  rgb_mat <- decode_colour(cb_cols)
  
  compare_colour(
    rgb_mat,
    from_space = "rgb",
    method = "cie2000"
  )
}


#--------------------------------------------------
# Step 2: Helper functions for highlighting
#--------------------------------------------------

highlight_value <- function(value, abnormal, digits = 3) {
  
  txt <- format(
    round(value, digits),
    nsmall = digits,
    trim = TRUE
  )
  
  if (abnormal) {
    
    paste0(
      '<span style="
        background-color:#FADBD8;
        padding:3px 6px;
        border-radius:3px;
      ">',
      txt,
      '</span>'
    )
    
  } else {
    txt
  }
}


# Only highlight the upper triangle of pairwise matrices
highlight_matrix <- function(mat, threshold, digits = 2) {
  
  # Initialize an empty character matrix
  out <- matrix(
    "",
    nrow = nrow(mat),
    ncol = ncol(mat)
  )
  
  for (i in seq_len(nrow(mat))) {
    
    for (j in seq_len(ncol(mat))) {
      
      # Only evaluate upper triangle
      abnormal <- i < j && mat[i, j] < threshold
      
      out[i, j] <- highlight_value(
        mat[i, j],
        abnormal,
        digits
      )
    }
  }
  
  dimnames(out) <- dimnames(mat)
  out
}


#--------------------------------------------------
# Step 3: UI
#--------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Figure Color Evaluation"),
  
  textInput(
    "colors",
    "Hex colors (space-separated):",
    value = "#b06389 #a2c4c9 #f1e9d4"
  ),
  
  actionButton("run", "Run"),
  
  h4("Colors"),
  uiOutput("swatches"),
  
  h4("Luminance Difference"),
  tableOutput("deltaL"),
  
  h4("Delta E 2000"),
  tableOutput("deltaE"),
  
  h4("Contrast vs White"),
  tableOutput("contrast"),
  
  h4("Relative Luminance"),
  tableOutput("lum"),
  
  h4("Delta E 2000 - Protanopia"),
  tableOutput("deltaE_protan"),
  
  h4("Delta E 2000 - Deuteranopia"),
  tableOutput("deltaE_deutan"),
  
  h4("Delta E 2000 - Tritanopia"),
  tableOutput("deltaE_tritan")
)


#--------------------------------------------------
# Step 4: Server
#--------------------------------------------------

server <- function(input, output) {
  
  # Turn text input into a character vector
  cols <- eventReactive(input$run, {
    
    strsplit(
      trimws(input$colors),
      "\\s+"
    )[[1]]
    
  })
  
  
  #------------------------------------------------
  # Color swatches
  #------------------------------------------------
  
  output$swatches <- renderUI({
    
    color_boxes <- lapply(cols(), function(hex) {
      
      div(
        style = paste0(
          "width:50px;
           height:50px;
           display:inline-block;
           margin-right:8px;
           background-color:", hex, ";"
        )
      )
      
    })
    
    do.call(tagList, color_boxes)
  })
  
  
  #------------------------------------------------
  # Luminance Difference
  # Flag if < 0.05
  # Upper triangle only
  #------------------------------------------------
  
  output$deltaL <- renderTable({
    
    mat <- deltaL(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    # Keep only unique pairwise comparisons
    mat[lower.tri(mat, diag = TRUE)] <- NA
    
    highlight_matrix(
      mat,
      threshold = 0.05,
      digits = 3
    )
    
  },
  rownames = TRUE,
  sanitize.text.function = identity)
  
  
  #------------------------------------------------
  # Delta E 2000
  # Flag if < 10
  # Upper triangle only
  #------------------------------------------------
  
  output$deltaE <- renderTable({
    
    mat <- deltaE2000(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    # Keep only unique pairwise comparisons
    mat[lower.tri(mat, diag = TRUE)] <- NA
    
    highlight_matrix(
      mat,
      threshold = 10,
      digits = 2
    )
    
  },
  rownames = TRUE,
  sanitize.text.function = identity)
  
  
  
  #------------------------------------------------
  # Contrast vs White
  # Flag if < 2
  #------------------------------------------------
  
  output$contrast <- renderTable({
    
    values <- sapply(
      cols(),
      contrast_vs_white
    )
    
    display_values <- sapply(
      values,
      function(x) {
        
        highlight_value(
          x,
          abnormal = x < 2,
          digits = 2
        )
        
      }
    )
    
    data.frame(
      Color = cols(),
      Contrast = display_values
    )
    
  },
  sanitize.text.function = identity)
  
  
  #------------------------------------------------
  # Relative Luminance
  # Flag if > 0.90
  #------------------------------------------------
  
  output$lum <- renderTable({
    
    values <- sapply(
      cols(),
      relative_luminance
    )
    
    display_values <- sapply(
      values,
      function(x) {
        
        highlight_value(
          x,
          abnormal = x > 0.90,
          digits = 3
        )
        
      }
    )
    
    data.frame(
      Color = cols(),
      Luminance = display_values
    )
    
  },
  sanitize.text.function = identity)
  
  
  
  #------------------------------------------------
  # Protanopia
  # Flag if < 8
  # Upper triangle only
  #------------------------------------------------
  
  output$deltaE_protan <- renderTable({
    
    mat <- deltaE2000_protan(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    # Keep only unique pairwise comparisons
    mat[lower.tri(mat, diag = TRUE)] <- NA
    
    highlight_matrix(
      mat,
      threshold = 8,
      digits = 2
    )
    
  },
  rownames = TRUE,
  sanitize.text.function = identity)
  
  
  #------------------------------------------------
  # Deuteranopia
  # Flag if < 8
  # Upper triangle only
  #------------------------------------------------
  
  output$deltaE_deutan <- renderTable({
    
    mat <- deltaE2000_deutan(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    # Keep only unique pairwise comparisons
    mat[lower.tri(mat, diag = TRUE)] <- NA
    
    highlight_matrix(
      mat,
      threshold = 8,
      digits = 2
    )
    
  },
  rownames = TRUE,
  sanitize.text.function = identity)
  
  
  #------------------------------------------------
  # Tritanopia
  # Flag if < 8
  # Upper triangle only
  #------------------------------------------------
  
  output$deltaE_tritan <- renderTable({
    
    mat <- deltaE2000_tritan(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    # Keep only unique pairwise comparisons
    mat[lower.tri(mat, diag = TRUE)] <- NA
    
    highlight_matrix(
      mat,
      threshold = 8,
      digits = 2
    )
    
  },
  rownames = TRUE,
  sanitize.text.function = identity)
}


#--------------------------------------------------
# Run App
#--------------------------------------------------

shinyApp(ui, server)