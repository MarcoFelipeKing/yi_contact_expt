
# Intro -------------------------------------------------------------------
# This is an algorithm for extracting the mass value from Yi's scales

# Packages ----------------------------------------------------------------

pacman::p_load("flexdashboard","tidyr","shiny","dplyr","janitor","magrittr",
               "data.table","personograph","vroom","DBI","duckdb","lubridate",
               "plotly","ggplot2","imager","tesseract","magick")

# Import Video ------------------------------------------------------------
imager::load.video(
  fname,
  maxSize = 1,
  skip.to = 0,
  frames = NULL,
  fps = NULL,
  extra.args = "",
  verbose = FALSE
)


# Split into frames -------------------------------------------------------
frames(im, index, drop = FALSE)

# Tesseract ---------------------------------------------------------------
#https://docs.ropensci.org/tesseract/articles/intro.html#preprocessing-with-magick-1
im<-magick::image_convert(image = "~/Desktop/NdHYc.jpg",format = "jpg")
text <- tesseract::ocr(image="~/Desktop/NdHYc.jpg")
tesseract::
tesseract_params('colour')
