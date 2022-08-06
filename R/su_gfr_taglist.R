#' Generate a snippet of HTML code for summary report
#'
#' @param gfr_elems Google Form Summary Elements object
#'
#' @details This will return a list of HTML tags for the elements in gfr_elems
#'
#' @seealso \code{\link{su_gfr_elems}}
#'
#' @importFrom magrittr extract2
#'
#' @export

su_gfr_taglist <- function(gfr_elems) {

  ## The following packages are needed by the Rmd file so we check for their existence here
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Sorry, this function requires the ggplot2 package. Please install it then try again.")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("Sorry, this function requires the htmltools package. Please install it then try again.")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Sorry, this function requires the knitr package. Please install it then try again.")

  html_lst <- list()

  for (i in 1:length(gfr_elems)) {

    if (inherits(gfr_elems[[i]], "ggplot_with_params")) {

      ## Get the plotting parameters: width in px | height in px | dpi
      plt_wdt_hgt_dpi <- gfr_elems[[i]][[2]] %>%
        strsplit("\\|") %>%
        extract2(1) %>%
        trimws() %>%
        as.numeric()

      ## Create a temp PNG file
      ggtmp_fn <- tempfile("gfs_ggplot-", fileext = ".png")

      ## Save the ggplot to the temp file. We compute the width and height in terms of inches
      ## so that the text elements will size correctly
      ggplot2::ggsave(filename = ggtmp_fn,
             plot = gfr_elems[[i]][[1]],
             device = "png",
             width = plt_wdt_hgt_dpi[1] / plt_wdt_hgt_dpi[3],
             height = plt_wdt_hgt_dpi[2] / plt_wdt_hgt_dpi[3],
             units = "in",
             dpi = plt_wdt_hgt_dpi[3])

      ## Create the img tag using a image_uri
      ## print(img(src=knitr::image_uri(ggtmp_fn)))

      html_lst[[length(html_lst) + 1]] <- htmltools::img(src=knitr::image_uri(ggtmp_fn))

      ## Delete the temp file
      unlink(ggtmp_fn)

    } else if (inherits(gfr_elems[[i]], "leaflet")) {

      html_lst[[length(html_lst) + 1]] <- htmltools::HTML("<div style='margin:0 1em;'>")
      html_lst[[length(html_lst) + 1]] <- gfr_elems[[i]]
      html_lst[[length(html_lst) + 1]] <- htmltools::HTML("</div>")

    } else {
      ## Some other kind of HTML tag

      html_lst[[length(html_lst) + 1]] <- gfr_elems[[i]]
    }
  }


  invisible(html_lst)

}
