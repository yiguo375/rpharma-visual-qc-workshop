# --------------------------------------------------
# Workshop Environment Setup
# Run this script once to install all R packages
# required for the two workshop examples.
# --------------------------------------------------

required_packages <- c(
  "shiny",
  "farver",
  "colorspace",
  "dichromat",
  "magick",
  "base64enc",
  "rlang",
  "pdftools",
  "shinyscreenshot"
)

# Identify packages that are not yet installed
missing_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  
  install.packages(missing_packages)
  
} else {
  message("All required R packages are already installed.")
}

# Display package versions for environment checking
cat("\nPackage versions:\n")

invisible(lapply(required_packages, function(pkg) {
  cat(sprintf(
    "%-18s %s\n",
    pkg,
    as.character(packageVersion(pkg))
  ))
}))

cat("\nR version: ", R.version.string, "\n")


# --------------------------------------------------
# Optional system dependencies
# --------------------------------------------------

# The workshop examples are designed to run in Posit Cloud.
#
# magick and pdftools rely on underlying system libraries
# (ImageMagick/Magick++ and Poppler, respectively).
# These are environment-level dependencies and normally
# do not require participant setup in the provided environment.
#
# OPTIONAL:
# The rtf_to_pdf() helper in Example 2 uses LibreOffice.
# LibreOffice is NOT required for the workshop hands-on exercises,
# since sample PDF files are provided.
#
# The current helper uses this macOS-specific executable path:
# /Applications/LibreOffice.app/Contents/MacOS/soffice
