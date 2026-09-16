# ============================================================
# Grayscale QC - Reusable Functions
# ============================================================

library(magick)
library(pdftools)

# ------------------------------------------------------------
# Optional: convert RTF to PDF using LibreOffice
# Note: the path below is for macOS and may differ by system.
# ------------------------------------------------------------
rtf_to_pdf <- function(rtf_path) {
  soffice <- "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  if (!file.exists(soffice)) stop("LibreOffice soffice not found")

  tmp <- tempfile(fileext = ".rtf")
  file.copy(rtf_path, tmp, overwrite = TRUE)

  out_dir <- tempfile("rtf_to_pdf_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- suppressWarnings(system2(
    soffice,
    args = c(
      "--headless", "--nologo", "--nolockcheck",
      "--convert-to", "pdf:writer_pdf_Export",
      "--outdir", out_dir,
      normalizePath(tmp, winslash = "/", mustWork = TRUE)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))

  pdfs <- list.files(out_dir, pattern = "\\.pdf$", full.names = TRUE)
  if (!length(pdfs)) {
    stop(paste0("RTF to PDF conversion failed:\n", paste(res, collapse = "\n")))
  }

  pdfs[1]
}

# ------------------------------------------------------------
# Get total number of pages in a PDF file
# ------------------------------------------------------------
pdf_n_pages <- function(path) {
  pdftools::pdf_info(path)$pages
}

# Defensive-programming alternative:
# pdf_n_pages <- function(path) {
#   if (requireNamespace("pdftools", quietly = TRUE)) {
#     as.integer(pdftools::pdf_info(path)$pages)
#   } else {
#     NA_integer_
#   }
# }

# ------------------------------------------------------------
# Page rasterization and downsampling
# ------------------------------------------------------------
raster_resize <- function(path, page, dpi, resize_pct) {
  # Page rasterization
  img <- image_read_pdf(path, pages = page, density = dpi)

  # Image downsampling
  if (!is.null(resize_pct) && resize_pct > 0 && resize_pct < 100) {
    img <- image_resize(img, paste0(resize_pct, "%"))
  }

  img
}

# ------------------------------------------------------------
# Grayscale conversion
# ------------------------------------------------------------
to_gray <- function(img) {
  image_convert(img, colorspace = "gray")
}

# ------------------------------------------------------------
# Crop page margins
# ------------------------------------------------------------
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


# ------------------------------------------------------------
# Extract grayscale pixel values
# ------------------------------------------------------------
gray_vals <- function(gray_img) {
  a <- image_data(gray_img, channels = "gray")
  strtoi(as.vector(a[1, , ]), 16L)
}

# ------------------------------------------------------------
# Vertical and horizontal ink profiles
# ------------------------------------------------------------
ink_profiles <- function(gray_img) {
  a <- image_data(gray_img, channels = "gray")

  # a[1, , ] is width x height
  mat <- apply(a[1, , ], c(1, 2), function(x) strtoi(x, 16L))

  # Ink = 255 - gray
  ink <- 255 - mat

  list(
    vertical = apply(ink, 2, mean),    # top to bottom
    horizontal = apply(ink, 1, mean)   # left to right
  )
}

# ------------------------------------------------------------
# Analyze one page using pixel-density metrics
# ------------------------------------------------------------
analyze_page <- function(
  path,
  page,
  dpi,
  resize_pct,
  white_thr,
  sus_thr,
  w_thr,
  top_crop_thr,
  bot_crop_thr,
  side_crop_thr,
  pct_region
) {
  # Page rasterization and downsampling
  img <- raster_resize(path, page, dpi, resize_pct)
  
  # Grayscale conversion
  g <- to_gray(img)

  # Crop page margins
  c_img <- crop_margins(
    g,
    top_crop_thr,
    bot_crop_thr,
    side_crop_thr
  )
  
  c_info <- image_info(c_img)
  cw <- c_info$width
  ch <- c_info$height

  # Center region as numeric pixel values
  center <- gray_vals(c_img)

  # Reference metrics from cropped center region
  mean_c <- mean(center)
  sd_c <- sd(center)

  # Secondary indicator from cropped center region
  overall_white_ratio <- mean(center > white_thr)

  # Primary indicators from cropped center region
  bot <- gray_vals(
    image_crop(
      c_img,
      geometry_area(
        cw,
        floor(ch * pct_region / 100),
        0,
        ch - floor(ch * pct_region / 100)
      )
    )
  )

  rgt <- gray_vals(
    image_crop(
      c_img,
      geometry_area(
        floor(cw * pct_region / 100),
        ch,
        cw - floor(cw * pct_region / 100),
        0
      )
    )
  )

  bottom_white_ratio <- mean(bot > white_thr)
  right_white_ratio  <- mean(rgt > white_thr)

  # Page detection
  status <- if (
    bottom_white_ratio > sus_thr ||
    right_white_ratio  > sus_thr ||
    overall_white_ratio > w_thr
  ) "CHECK" else "OK"

  # Diagnostic profiles
  prof <- ink_profiles(c_img)

  list(
    page = page,
    mean_c = mean_c,
    sd_c = sd_c,
    overall_white_ratio = overall_white_ratio,
    bottom_white_ratio = bottom_white_ratio,
    right_white_ratio = right_white_ratio,
    status = status,
    vertical_profile = prof$vertical,
    horizontal_profile = prof$horizontal
  )
}
