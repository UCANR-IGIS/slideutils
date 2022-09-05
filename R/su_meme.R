#' Insert a meme
#'
#' @param meme The name of a meme (character)
#' @param height The image height (keyword or number)
#' @param quote_main The main quote (character)
#' @param data_uri Return a URI for the image instead of a file name (logical)
#' @param img_path The relative path from the HTML file to the image (ignored if data_uri is TRUE)
#' @param copy_to A directory to copy the image to if needed
#' @param tbl_width_px Table width in pixels
#' @param tbl_border_css CSS for the table border property
#' @param tbl_margin_css CSS for the table margin property
#' @param font_size_css CSS for the font-size property
#' @param font_color_css CSS for the font color property
#' @param img_borderradius_css CSS for the image border-radius property
#' @param img_boxshadow_css CSS for the image box-shadow property
#' @param debug Print debugging messages to the console, Logical
#'
#' @details This will return an HTML object that can be printed in a RMarkdown.
#'
#' \code{height} can be a keyword \code{small|medium|large}, which will make the meme image appear 160, 240, or 320 pixels
#' tall respectively. Or you can pass an integer. The width will be scaled automatically.
#'
#' Note that \code{img_path} is not the path to the image from the active directory, but rather the relative path
#' from the HTML file being rendered to the image. It will be added to the src attribute of the img tag.
#'
#' @returns html object
#'
#' @examples
#' \dontrun{
#' su_meme("yoda-sage", "medium", "Organize your work in RStudio Projects,<br/>and peace with your files you will have.")
#' }
#'
#' @importFrom htmltools HTML
#' @importFrom magick image_info image_resize image_read image_write
#' @importFrom knitr image_uri
#' @importFrom tools file_ext
#' @export

## TO DO
## add an attribute for the quotation_main column width
## add a layout attribute: side-by-side
## dynamically adjust the column width of the meme column based on the image size

su_meme <- function(meme = "yoda-sage", height = c("small", "medium", "large")[2], quote_main = NULL,
                    data_uri = FALSE, img_path = "./images/", copy_to = NULL,
                    tbl_width_px = 640, tbl_border_css = "none", tbl_margin_css = "0 auto",
                    font_size_css = "100%", font_color_css = "black",
                    img_boxshadow_css = c("none", "0px 0px 5px 1px #555")[1], img_borderradius_css = c("0", "5px")[1],
                    debug = FALSE) {

  if (!meme %in% c("yoda-sage", "face-frustrated")) stop("Unknown value for `meme` argument")

  ## copy_to or data_uri = TRUE
  if (is.null(copy_to)) {
    if (!data_uri) stop("You must either provide a value for `copy_to`, or else set data_uri = TRUE")
  } else {
    if (!dir.exists(copy_to)) stop("`copy_to` must be an existing directory")
  }

  ## Compute the meme output height
  if (is.null(height)) {
    stop("Height should be a small|medium|large or a number of pixels")

  } else {
    err_msg <- "Unknown value for `height` argument. Valid values are small, medium, large, or number of pixels"

    if (is.character(height)) {
      if (!height %in% c("small", "medium", "large")) stop(err_msg)
      height_use <- list(large = 320, medium = 240, small = 160)[[height]]

    } else if (is.numeric(height)) {
      height_use <- height

    } else {
      stop(err_msg)
    }
  }

  ## Get the file name to the image in the package
  img_in_fn <- list.files(system.file("memes", package = "slideutils"),
                       pattern = paste0("^", meme), full.names = TRUE)

  if (length(img_in_fn) != 1) stop(paste0("Can't find meme image: ", meme))

  ## Read in the package image
  img_in_magk <- image_read(img_in_fn)

  ## Get the package image dimensions
  img_in_dim_tbl <- image_info(img_in_magk)[1, c("width", "height")]

  ## Resize the image if needed
  if (img_in_dim_tbl$height == height_use) {
    img_out_magk <- img_in_magk
    img_out_dim_tbl <- image_info(img_out_magk)[1, c("width", "height")]
  } else {
    img_out_magk <- image_resize(img_in_magk, geometry = paste0("x", height_use))
    img_out_dim_tbl <- image_info(img_out_magk)[1, c("width", "height")]
  }

  ## Get the data uri or copy the file to copy_to
  if (data_uri) {
    img_tmp_fn <- tempfile(fileext = paste0(".", file_ext(img_in_fn)))
    image_write(img_out_magk, path = img_tmp_fn)
    on.exit(unlink(img_tmp_fn))
    img_uri <- knitr::image_uri(img_tmp_fn)
    #stop("Sorry, data_uri is not yet supported")

  } else {
    ## Generate the output fn
    img_out_fn <- gsub(paste0(".", file_ext(img_in_fn), "$"),
                       paste0("_", img_out_dim_tbl$width, "x", img_out_dim_tbl$height, ".", file_ext(img_in_fn)),
                       basename(img_in_fn))

    img_out_pathfn <- file.path(copy_to, img_out_fn)

    if (!file.exists(img_out_pathfn)) image_write(img_out_magk, path = img_out_pathfn)

    #file.copy(from = img_in_fn, to = copy_to, overwrite = FALSE)

    img_uri <- paste0(img_path, basename(img_out_fn))
  }

  ## Prepare the HTML for the <td> with the quote
  if (is.null(quote_main)) {
    quote_main_html <- ""
  } else {
    quote_main_html <- paste0("<td style='text-align:center; font-size:", font_size_css, "; font-style:normal; font-family:Comic Sans MS, Comic Sans, cursive; line-height:180%; padding:1em; color:", font_color_css, ";'>", quote_main, "</td>")
  }


  res <- paste0("<table style='max-width:", tbl_width_px, "px; margin:", tbl_margin_css, "; border:", tbl_border_css,
                ";'><tr style='vertical-align:middle;'><td><img src='", img_uri, "' style='width:", img_out_dim_tbl$width,
                "px; height:", img_out_dim_tbl$height, "px; box-shadow:", img_boxshadow_css, "; border-radius:", img_borderradius_css,
                ";'></img></td>", quote_main_html, "</tr></table>")

  if (debug) cat(res, "\n")

  ## We don't want to return an invisible object, because we actually want it to print
  HTML(res)

}
