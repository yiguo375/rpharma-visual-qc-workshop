library(shiny)
library(farver)
library(colorspace)
library(dichromat)

#--------------------------------------------------
# Step 1: Helper functions
# Already provided
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

# Luminance Difference
deltaL <- function(cols) {
  
  lum <- sapply(cols, relative_luminance)
  
  # Compare every luminance value with every other value
  outer(
    lum,
    lum,
    function(a, b) abs(a - b)
  )
}

# Color Difference 
deltaE2000 <- function(cols) {
  
  rgb_mat <- decode_colour(cols)
  
  compare_colour(
    rgb_mat,
    from_space = "rgb",
    method = "cie2000"
  )
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


#--------------------------------------------------
# Step 2: UI
# Already provided
#--------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Figure Color Evaluation"),
  
  textInput(
    "colors",
    "Hex colors (space-separated):",
    value = "#b06389 #a2c4c9 #f1e9d4"
  ),
  
  actionButton("run", "Run"),
  
  h4("Luminance Difference"),
  tableOutput("deltaL"),
  
  h4("Delta E 2000"),
  tableOutput("deltaE"),
  
  h4("Contrast vs White"),
  tableOutput("contrast")
)


#--------------------------------------------------
# Step 3: Server
# Complete the missing pieces
#--------------------------------------------------

server <- function(input, output) {
  
  # Example:
  # Turn the text input into a character vector
  # Recalculate only when the Run button is clicked
  
  cols <- eventReactive(input$run, {
    
    strsplit(
      trimws(input$colors),
      "\\s+"
    )[[1]]
    
  })
  
  
  #------------------------------------------------
  # Example: Luminance Difference
  #------------------------------------------------
  
  output$deltaL <- renderTable({
    
    # 1. Pass the reactive color vector
    #    to the helper function
    
    mat <- deltaL(cols())
    
    
    # 2. Add color labels to rows and columns
    
    dimnames(mat) <- list(
      cols(),
      cols()
    )
    
    
    # 3. Return the matrix
    
    mat
    
  },
  rownames = TRUE)
  
  
  #------------------------------------------------
  # Exercise 1: Delta E 2000
  #------------------------------------------------
  
  output$deltaE <- renderTable({
    
    # TODO:
    # 1. Use deltaE2000() to compare the colors

    
    
    # TODO:
    # 2. Add the colors as row and column names

    
    # TODO:
    # 3. Return the completed matrix
    
    
  },
  rownames = TRUE)
  
  
  #------------------------------------------------
  # Exercise 2: Contrast vs White
  #------------------------------------------------
  
  output$contrast <- renderTable({
    
    # TODO:
    # Create a data frame with:
    #
    #   Color
    #   Contrast
    #
    # Hint:
    # Apply contrast_vs_white()
    # to each color using sapply()
    
    # data.frame(
    #   Color = ,
    #   Contrast = sapply(__)
    # )
    
  })
  
}


#--------------------------------------------------
# Run App
#--------------------------------------------------

shinyApp(ui, server)