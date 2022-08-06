#' Make PNG images transparent backgrounds
#'
#' @param x Vector of image file names or a directory
#' @param pattern Pattern to match if x is a directory
#' @param out_dir Output directory
#' @param out_fn A filename template
#' @param overwrite Overwrite existing files
#'
#' @details This will take one or more PNG files (e.g., saved from PowerPoint slides) and create copies where white becomes
#' transparent. This can be useful if you want to use the images as a build.
#'
#' \strong{TIP}: if the source of your PNG files are a series of PowerPoint slides (that you convert to PNG with
#' File >> Save As...), set the size of your PowerPoint slides to produce the desired output dimensions.
#' When you save PowerPoint slides to raster formats, it saves them as 96 dpi (unless you've tweaked that).
#' So for example if you want to create PNG files that are 1000 x 562 (roughly 16:9 that will display fine on
#' a single slide), set the size of your PowerPoint presentation to 10.4" x 5.9".
#'
#' \code{x} can be a directory that contains image files, or a vector of filenames. If \code{x} is a directory,
#' you can use \code{pattern} to pass a regular expression.
#'
#' \code{out_dir} is the output directory (optional). \code{out_fn} is a file name template that will be passed
#' to \code{sprintf} for evaluation. Example: \code{out_fn = \"slide_\%02d.png\"} will result in output files named
#' slide_01.png, slide_02.png, slide_03.png, etc. If \code{out_fn} is NULL, the original file names
#' appended with '_trnsbg.png' will be used for the output files.
#'
#' @seealso \code{\link{su_img_build}}
#'
#' @importFrom magick image_read image_write image_transparent
#' @export

su_img_maketrans <- function(x, pattern = ".png$", out_dir = NULL, out_fn = "img_%03d.png", overwrite = FALSE) {

  # if (!requireNamespace("magick", quietly = TRUE)) stop("Sorry, this function requires the magick package. Please install it then try again.")

  if (length(x) == 1 && file.info(x[[1]])[["isdir"]]) {

    ## x is a directory
    img_in_fns <- list.files(x,
                             pattern = pattern,
                             full.names = TRUE,
                             ignore.case = TRUE)

    if (length(img_in_fns) == 0) stop("No image files found that match the pattern")

    ## Define out_dir_use
    if (is.null(out_dir)) {
      out_dir_use <- x
    } else {
      if (!dir.exists(out_dir)) stop(paste0(out_dir, " does not exist"))
      out_dir_use <- out_dir
    }

  } else {
    ## x should be a vector of file names
    if (FALSE %in% file.exists(x)) stop("image(s) not found")
    img_in_fns <- x

    ## Define out_dir_use
    if (is.null(out_dir)) {
      out_dir_use <- unique(dirname(img_in_fns))
      if (length(out_dir_use) != 1) stop("if the source images come from multiple folders, please specify `out_dir`")

    } else {
      if (!dir.exists(out_dir)) stop(paste0(out_dir, " does not exist"))
      out_dir_use <- out_dir
    }

  }

  ## Generate the output file names
  if (is.null(out_fn)) {
    out_fn_use <- gsub(".png$", "_trnsbg.png", basename(img_in_fns), ignore.case = TRUE)
  } else {
    out_fn_use <- sprintf(out_fn, 1:length(img_in_fns))
  }

  if (!overwrite) {
    if (TRUE %in% file.exists(file.path(out_dir_use, out_fn_use))) stop("One or more output files already exist. Set `overwrite = TRUE`, or change `out_dir` or `out_fn`.")
  }

  ## Export the images
  for (i in 1:length(img_in_fns)) {
    img_in <- image_read(img_in_fns[i])
    image_write(img_in %>% image_transparent('white'),
                path = file.path(out_dir_use, out_fn_use[i]),
                format = "png")
  }

  invisible(out_fn_use)

}
