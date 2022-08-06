#' Generate a snippet of HTML code for summary report
#'
#' @param gfr_elems Google Form Summary Elements object
#' @param output_fn Report filename
#' @param output_dir Output directory
#' @param overwrite Overwrite an existing file, Logical
#'
#' @details This generates the same HTML content as \code{\link{su_gfr_report}},
#' however only the report elements are saved to the HTML file. In other words
#' this function doesn't produce a full HTML report
#'
#'
#' @seealso \code{\link{su_gfr_elems}}, \code{\link{su_gfr_report}}
#' @importFrom knitr image_uri
#' @importFrom htmltools tagList img
#' @importFrom magrittr extract2
#'
#' @export

su_gfr_snippet <- function(gfr_elems, output_fn, output_dir = ".", overwrite = TRUE) {

  if (!file.exists(output_dir)) stop("Output directory does not exist")

  out_fn <- normalizePath(file.path(output_dir, output_fn), mustWork = FALSE)

  if (file.exists(out_fn) && !overwrite) stop(paste0(out_fn, " exists. Set `overwrite = TRUE` or save to a new file."))

  html_taglist <- su_gfr_taglist(gfr_elems)

  sink(file = out_fn)
  on.exit(sink())
  print(htmltools::tagList(html_taglist))
  sink()
  on.exit()
  message(paste0("Report elements written to HTML snippet:\n", out_fn))

  invisible(out_fn)

}
