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
  colours_k      <- rgb(k_means$centers[k_means$cluster,])
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

  painting_rgb <- as(colorspace::RGB(painting_raw_rgb_data), colspace)

  k_means <- stats::kmeans(colorspace::coords(painting_rgb), centers = number_of_colors,
                           iter.max = iter.max, nstart = nstart)

  colours_k <- colorspace::hex(do.call(colspace, list(k_means$centers)))

  colors_vector <- sort(colours_k, decreasing = TRUE)

  if(plot_colors) scales::show_col(colors_vector)

  return(colors_vector)
}