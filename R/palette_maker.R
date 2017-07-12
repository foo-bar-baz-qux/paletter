#' @importFrom stats kmeans
#' @importFrom jpeg readJPEG
#' @importFrom scales show_col
#' @title make a palette from your image
#' @description processes a custom jpeg image producing a palett with a user-defined number of colours
#' @param image_path relative path to the custom image, including .jpeg extension
#' @param number_of_colors number of different colors desired for the resulting palette
#' @author Andrea Cirillo
#' @examples
#' palette_maker("data/nascita_venere.jpg",number_of_colors = 20)
#' @export
palette_maker <- function(image_path = NA, number_of_colors = 20){
  if (is.na(image_path)){stop("you must provide a jpg image to build your palette from")}
  painting     <- readJPEG(image_path)
  dimension    <- dim(painting)
  painting_rgb <- data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(painting[,,1]),
    G = as.vector(painting[,,2]),
    B = as.vector(painting[,,3])
  )
  k_means        <- kmeans(painting_rgb[,c("R","G","B")], centers = number_of_colors, iter.max = 30)
  colours_k      <- grDevices::rgb(k_means$centers[k_means$cluster,])
  colours_vector <- unique(colours_k)
  show_col(colours_vector)
  return(colours_vector)
}


#' Make a palette from your image in a particular color space
#'
#' Processes a custom jpeg image producing a palett with a user-defined number
#' of colours. This will allow any color space available in the `colorspace`
#' package. There may be conversion issues if the selected color space does not
#' have a valid representation of the all RGB values in the picture.
#'
#' @param image_path Relative path to the custom image, including .jpeg
#'   extension
#' @param number_of_colors Number of different colors desired for the resulting
#'   palette
#' @param colspace A valid color class from the `colorspace` package. Valid
#'   choices are 'RGB', 'LAB', 'LUV', 'XYZ', 'HLS', 'HSV', 'polarLUV',
#'   'polarLAB', 'sRGB'.
#' @param plot_colors Whether to display a rough plot of the selected colors
#'   using the `scales` package.
#' @inheritParams stats::kmeans
#' @author Victor Lei (based on Andrea Cirillo's original code)
#' @examples
#' palette_maker_cs('data/nascita_venere.jpg', number_of_colors = 20, colspace = "LAB", iter.max = 100)
#' @import colorspace
#' @export
palette_maker_cs <- function(image_path = NA, number_of_colors = 20, colspace = "RGB",
                          iter.max = 50, nstart = 1, plot_colors = TRUE) {
  if( !(colspace %in% c('RGB', 'LAB', 'LUV', 'XYZ', 'HLS', 'HSV', 'polarLUV', 'polarLAB', 'sRGB') ) ) {
    stop("Color space ", colspace, " not supported")
  }
  if (is.na(image_path)) {
    stop("You must provide a jpg image to build your palette from")
  }
  painting <- jpeg::readJPEG(image_path)
  painting_raw_rgb_data <- as.matrix(data.frame(R = as.vector(painting[,, 1]),
                             G = as.vector(painting[,, 2]),
                             B = as.vector(painting[,, 3])))

  painting_rgb <- methods::as(colorspace::RGB(painting_raw_rgb_data), colspace)

  k_means <- stats::kmeans(colorspace::coords(painting_rgb), centers = number_of_colors,
                           iter.max = iter.max, nstart = nstart)

  colours_k <- colorspace::hex(do.call(colspace, list(k_means$centers)))

  colors_vector <- sort(colours_k, decreasing = TRUE)

  if(plot_colors) scales::show_col(colors_vector)

  return(colors_vector)
}


#' Use a number of different methods to generate color palettes
#'
#' Clustering in different color spaces yield somewhat different color palettes.
#' This helper function automates the process of clustering your image in
#' different color spaces.
#'
#' @param img_path The path to your JPEG image file
#' @param out_prefix The filename output prefix to which the color space name is
#'   appended (e.g. 'myimage' results in 'myimage_RGB.png', 'myimage_LAB.png'
#'   etc.)
#' @param test_col_spaces Which color spaces to test. All valid color spaces in the `colorspace` package are supported. See \code{\link{palette_maker_cs}}.
#' @param num_colors Number of colors to produce in your palette
#' @param out_w Output image width in pixels
#' @param out_h Output image height in pixels
#' @param nstart Number of global re-runs of kmeans
#' @param iter.max Number of local iterations of kmeans
#' @examples
#'  run_test_paletter('data/nascita_venere.jpg', 'nascita_venere', num_colors = 9, nstart = 5)
#' @export
run_test_paletter <- function(img_path, out_prefix, test_col_spaces = c('RGB', 'LAB', 'HLS'), num_colors = 9, out_w = 500, out_h = 500, nstart = 1, iter.max = 100) {
  # Use the original paletter function
  message('Trying original RGB color space')
  grDevices::png(file = paste0(out_prefix, '_original.png'), width = out_w, height = out_h)
  ret = palette_maker(img_path, number_of_colors = num_colors)
  grDevices::dev.off()

  # Use the new paletter function with a variety of different color spaces
  for( cur_cs in test_col_spaces ) {
    message('Trying color space ', cur_cs)
    grDevices::png(file = paste0(out_prefix, '_', cur_cs, '.png'), width = out_w, height = out_h)
    ret = palette_maker_cs(img_path, number_of_colors = num_colors, colspace = "LAB", nstart = nstart, iter.max = iter.max)
    dev.off()
  }
  message('All tests completed!')
}