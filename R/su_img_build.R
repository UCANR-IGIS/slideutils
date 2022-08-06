#' Generate a list of HTML tags to show stacked PNGs
#'
#' @param img_fns A vector of image files names
#' @param display_first Display the first image initially
#' @param img_class The class that image tags will be assigned
#' @param center Center the DIV on the document
#' @param border A valid CSS style for the DIV border
#' @param include_css Include the CSS for the images
#' @param base_dir Where to look for the files
#'
#' @details This will return a list of HTML tags that when inserted into a R markdown HTML document
#' will display the PNG files as a build. You can use this function within code chunks
#' in R markdown to create a build effect from a set of PNG files. The PNG
#' files will be added to the DIV in the order received. All images should have the same
#' dimensions, and images after the first one should either have transparent backgrounds
#' or contain the earlier elements in them.
#'
#' To use this function, add the following style definition to your Rmarkdown document. Alternately,
#' you can pass \code{include_css = TRUE}, but note that this will insert the style definition into
#' your HTML document each time you insert an image build, so you really only need to add
#'  \code{include_css = TRUE} on the first build in the slide deck.
#'
#' \preformatted{
#' ```{css echo = FALSE}
#' img.stackme {
#'   position:absolute;
#'   top:0;
#'   left:0;
#' }
#' ```
#' }
#'
#' Insert the build into your Markdown with something like:
#'
#' \preformatted{
#' ```{r echo = FALSE, results = "asis"}
#' paste0("./images/myslide_0", 1:6, ".png") \%>\%
#'   slideutils::su_img_build(display_first = TRUE, center = FALSE) \%>\%
#'   htmltools::tagList()
#' ```
#' }
#'
#'
#' @importFrom htmltools img HTML
#' @importFrom magick image_info image_read
#'
#' @export

su_img_build <- function(img_fns, display_first = TRUE, img_class = "stackme", center = FALSE,
                         border=c("none", "1px solid gray")[1], include_css = FALSE,
                         base_dir = NULL) {

  # if (!requireNamespace("magick", quietly = TRUE)) stop("Sorry, this function requires the magick package. Please install it then try again.")

  if (is.null(base_dir)) {
    img_fns_use <- img_fns
  } else {
    if (!dir.exists(base_dir)) stop("base_dir does not exist.")
    ## Strip off any final slash
    base_dir_use <- gsub("/$|\\\\$", "", base_dir)
    img_fns_use <-file.path(base_dir_use, basename(img_fns))
  }

  if (FALSE %in% file.exists(img_fns_use)) stop("Image file(s) not found")

  if (include_css) {
    res <- list(HTML("<style>img.stackme {position:absolute; top:0; left:0;}</style>"))
  } else {
    res <- list()
  }

  ## Get the dimensions of all the images as a data frame
  prp_df <- do.call(rbind, lapply(img_fns_use, function(x) image_info(image_read(x))[ , c("width","height")]))
  h_max <- max(prp_df$height)

  if (center) {
    center_css <- paste0("width:", max(prp_df$width), "px; margin:0 auto; ")
  } else {
    center_css <- ""
  }

  res[[length(res) + 1]] <- HTML(paste0("<div style='position:relative; border:",
                                        border, "; height:", h_max, "px;", center_css, "'>"))

  # res <- list(HTML(paste0("<div style='position:relative; border:", border, "; height:", h_max, "px;",
  #                         center_css, "'>")))

  if (display_first) {
    res[[length(res) + 1]] <- img(src = img_fns_use[1])
    start_idx <- 2
  } else {
    start_idx <- 1
  }

  for (i in start_idx:length(img_fns_use)) {
    res[[length(res) + 1]] <- img(src = img_fns_use[i], class = paste("incremental", img_class))
  }

  res[[length(res) + 1]] <- HTML("</div>")

  invisible(res)

}
