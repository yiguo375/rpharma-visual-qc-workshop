#install.packages("farver")
#install.packages("colorspace")
#install.packages("dichromat")

library(farver)
library(colorspace)
library(dichromat)

#--------------------------------------------------
# Relative Luminance
#--------------------------------------------------

relative_luminance <- function(hex) {
  rgb <- hex2RGB(hex)@coords[1, ]
  f <- function(x) {
    # Convert sRGB to linear RGB (gamma correction)
    ifelse(x <= 0.03928, x / 12.92, ((x + 0.055) / 1.055)^2.4)
  }
  0.2126 * f(rgb[1]) + 0.7152 * f(rgb[2]) + 0.0722 * f(rgb[3])
}

#--------------------------------------------------
# Luminance Difference
#--------------------------------------------------

deltaL <- function(cols) {
  lum <- sapply(cols, relative_luminance)
  outer(lum, lum, function(a, b)
    abs(a - b))
}

#--------------------------------------------------
# Contrast Ratio (vs White Background)
#--------------------------------------------------

contrast_vs_white <- function(hex) {
  contrast_ratio(hex, "#FFFFFF")
}

#--------------------------------------------------
# Color Difference
#--------------------------------------------------

deltaE2000 <- function(cols) {
  rgb_mat <- decode_colour(cols)
  compare_colour(rgb_mat, from_space = "rgb", method = "cie2000")
}

#--------------------------------------------------
# Color Difference under Color Vision Deficiency (CVD)
#--------------------------------------------------

#Protanopia (red deficiency)
deltaE2000_protan <- function(cols) {
  cb_cols <- dichromat(cols, type = "protan")
  rgb_mat <- decode_colour(cb_cols)
  compare_colour(rgb_mat, from_space = "rgb", method = "cie2000")
}

#Deuteranopia (green deficiency)
deltaE2000_deutan <- function(cols) {
  cb_cols <- dichromat(cols, type = "deutan")
  rgb_mat <- decode_colour(cb_cols)
  compare_colour(rgb_mat, from_space = "rgb", method = "cie2000")
}

#Tritanopia (blue-yellow deficiency)
deltaE2000_tritan <- function(cols) {
  cb_cols <- dichromat(cols, type = "tritan")
  rgb_mat <- decode_colour(cb_cols)
  compare_colour(rgb_mat, from_space = "rgb", method = "cie2000")
}

#--------------------------------------------------
# Test
#--------------------------------------------------

cols <- c("#b06389", "#a2c4c9", "#f1e9d4")

#Relative Luminance
sapply(cols, relative_luminance)

#Luminance Difference
deltaL(cols)

#Contrast Ratio
sapply(cols, contrast_vs_white)

#Color Difference
deltaE2000(cols)

#Color Difference under CVD
deltaE2000_protan(cols)
deltaE2000_deutan(cols)
deltaE2000_tritan(cols)
