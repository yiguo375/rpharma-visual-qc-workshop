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
# Step 2: UI
#--------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Figure Color Evaluation"),
  
  textInput(
    "colors",
    "Hex colors (space-separated):",
    value = "#b06389 #a2c4c9 #f1e9d4"
  ),
  
  actionButton("run", "Run"),
  
  h4("Relative Luminance"),
  tableOutput("lum"),
  
  h4("Contrast vs White"),
  tableOutput("contrast"),
  
  h4("Luminance Difference"),
  tableOutput("deltaL"),
  
  h4("Delta E 2000"),
  tableOutput("deltaE"),
  
  h4("Delta E 2000 - Protanopia"),
  tableOutput("deltaE_protan"),
  
  h4("Delta E 2000 - Deuteranopia"),
  tableOutput("deltaE_deutan"),
  
  h4("Delta E 2000 - Tritanopia"),
  tableOutput("deltaE_tritan")
)


#--------------------------------------------------
# Step 3: Server
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
  # Relative Luminance
  #------------------------------------------------
  
  output$lum <- renderTable({
    
    data.frame(
      Color = cols(),
      Luminance = sapply(
        cols(),
        relative_luminance
      )
    )
    
  })
  
  
  #------------------------------------------------
  # Contrast vs White
  #------------------------------------------------
  
  output$contrast <- renderTable({
    
    data.frame(
      Color = cols(),
      Contrast = sapply(
        cols(),
        contrast_vs_white
      )
    )
    
  })
  
  
  #------------------------------------------------
  # Luminance Difference
  #------------------------------------------------
  
  output$deltaL <- renderTable({
    
    mat <- deltaL(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    mat
    
  },
  rownames = TRUE)
  
  
  #------------------------------------------------
  # Delta E 2000
  #------------------------------------------------
  
  output$deltaE <- renderTable({
    
    mat <- deltaE2000(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    mat
    
  },
  rownames = TRUE)
  
  
  #------------------------------------------------
  # Protanopia
  #------------------------------------------------
  
  output$deltaE_protan <- renderTable({
    
    mat <- deltaE2000_protan(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    mat
    
  },
  rownames = TRUE)
  
  
  #------------------------------------------------
  # Deuteranopia
  #------------------------------------------------
  
  output$deltaE_deutan <- renderTable({
    
    mat <- deltaE2000_deutan(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    mat
    
  },
  rownames = TRUE)
  
  
  #------------------------------------------------
  # Tritanopia
  #------------------------------------------------
  
  output$deltaE_tritan <- renderTable({
    
    mat <- deltaE2000_tritan(cols())
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    mat
    
  },
  rownames = TRUE)
}


#--------------------------------------------------
# Run App
#--------------------------------------------------

shinyApp(ui, server)