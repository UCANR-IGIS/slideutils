#' Generate a complete Google Form Summary HTML Report
#'
#' @param gfr_elems Google Form Summary Elements object
#' @param output_fn Report filename
#' @param output_dir Output directory
#' @param rpt_title The title for the report
#' @param rpt_rmd The Rmd file for the report
#'
#' @seealso \code{\link{su_gfr_elems}}
#'
#' @importFrom rmarkdown render
#' @export

su_gfr_report <- function(gfr_elems, output_fn, output_dir = ".",
                          rpt_title = "Google Form Summary", rpt_rmd = NULL) {

  ## Get the Rmd template
  if (is.null(rpt_rmd)) {
        rpt_rmd <- system.file("gfs-report.Rmd", package="slideutils")
      }
  if (!file.exists(rpt_rmd)) stop("Cant find the report template")

  report_fn <- render(input = rpt_rmd,
                      output_dir = output_dir, output_file = output_fn,
                      params = list(rpt_title = rpt_title,
                                    rpt_elemnts = gfr_elems))

  invisible(report_fn)

}
