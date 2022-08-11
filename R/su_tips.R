#' Generate small HTML files
#'
#' Generate small HTML files based on content from a spreadsheet
#'
#' @param gsheet_id A Google Spreadsheet ID
#' @param gsheet_sheet The name of the sheet in the Google Spreadsheet
#' @param tips_dir Directory where to save the generated HTML files
#' @param url_base The base of the URL
#' @param templates_dir Directory where the HTML template is saved
#' @param bitly Create a bitly link, Logical
#' @param open_html Open the local HTML file
#' @param quiet Suppress messages
#'
#'
#' This will generate small HTML files that you can use as hints, tips, or solutions for R exercises.
#' In addition to generating the local HTML file, it will also generate the URL, and optionally a bitly URL,
#' that you can embed in your .Rmd or .R file.
#'
#' The way this works is you have a spreadsheet with columns that contain the content of the questions. Currently,
#' only Google Sheets are supported. You can model your spreadsheet after the following:
#'
#' https://docs.google.com/spreadsheets/d/1iEOGHh4hC4KbiD5pn4kBTBMt3zrJga-rvSilR98ei00/copy
#'
#' The spreadsheet does not have to be open to anyone with the link. You'll be prompted to for your credentials using
#' the googlesheets4 package.
#'
#' This script goes through the sheet and
#'    i) renders (creates HTML file) for all rows where 'needs_rendering' is blank or TRUE
#'    ii) msg_html will be injected into a <div>
#'   iii) updates the Google Sheet
#'   needs_rendering - set to FALSE
#'   url - populated
#'   bitly - populated
#'
#'
#'
#' @returns A vector of HTML files rendered
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @importFrom googlesheets4 read_sheet range_write gs4_formula
#' @importFrom crayon silver yellow
#' @importFrom utils browseURL
#' @export

## TO DO
# su_tips <- function(gsheet_id = "19L5BX7yGjGTq2eYzjR5kZuLZknCkWq7ZHBmqf9DsjRw",
#                     gsheet_sheet = "Sheet1",
#                     tips_dir = "D:/Workshops/R-Spatial/rspatial_mod/outputs/rspatial_bgs22/docs/tips",
#                     url_base = "https://ajlyons.github.io/rspatial_bgs22/tips",
#                     templates_dir = "D:/GitHub/workshop_utils/slideutils/inst/tips_templates",
#                     bitly = TRUE,
#                     open_html = TRUE,
#                     quiet = FALSE) {

su_tips <- function(gsheet_id, gsheet_sheet, tips_dir, url_base,
                    templates_dir = system.file("tips_templates", package = "slideutils"),
                    bitly = TRUE, open_html = TRUE, quiet = FALSE) {

  if (bitly) {
    if (!requireNamespace("urlshorteneR")) stop("To create bitly links, please install `urlshorteneR`")
  }


  ## Delete final slashes from directories and URLs
  tips_dir <- gsub("\\\\$|/$", "", tips_dir)
  templates_dir <- gsub("\\\\$|/$", "", templates_dir)
  url_base <- gsub("\\\\$|/$", "", url_base)

  if (!dir.exists(tips_dir)) stop("tips_dir does not exist")
  if (!dir.exists(templates_dir)) stop("templates_dir does not exist")

  res <- character(0)

  wintitle_find_chr <- "<!--- wintitle-goes-here --->"
  title_find_chr <- "<!--- title-goes-here --->"
  msg_find_chr <- "<!--- msg-goes-here --->"

  ## Import the tips spreadsheet
  tips_tbl <- read_sheet(ss = gsheet_id, sheet = gsheet_sheet)

  ## Verify required fields are present
  req_names <- c("wintitle", "title", "all_done", "template", "msg_html", "fn_base", "last_rendered", "url")
  if (bitly) req_names <- c(req_names, "bitly")
  if (FALSE %in% (req_names %in% names(tips_tbl))) {
    stop(paste0("Required field(s) missing: ", paste(req_names[!req_names %in% names(tips_tbl)], collapse = ", ")))
  }

  ## Get some column numbers we might need later
  fn_base_colltr <- LETTERS[which(names(tips_tbl) == "fn_base")]
  all_done_colltr <- LETTERS[which(names(tips_tbl) == "all_done")]
  lastrendered_colltr <- LETTERS[which(names(tips_tbl) == "last_rendered")]
  url_colltr <- LETTERS[which(names(tips_tbl) == "url")]
  bitly_colltr <- LETTERS[which(names(tips_tbl) == "bitly")]

  ## Find rows to (re)render (all_done is blank or FALSE)
  rend_idx <- which(is.na(tips_tbl$all_done) | !tips_tbl$all_done)

  if (length(rend_idx) > 0) {

     for (i in rend_idx) {

       ## Get the HTML template with placeholders
       html_template_base <- tips_tbl[i, "template", drop = TRUE]
       if (is.na(html_template_base)) stop("'template' is missing")
       html_template_fn <- file.path(templates_dir, paste0(html_template_base, ".html"))
       if (!file.exists(html_template_fn)) stop(paste0("Can not find ", html_template_fn))

       html_txt <- readLines(html_template_fn)

       ## Swap out wintitle
       if (TRUE %in% grepl(wintitle_find_chr, html_txt, fixed = TRUE) ) {
         wintitle_replace_chr <- tips_tbl[i, "wintitle", drop = TRUE]
         if (is.na(wintitle_replace_chr)) wintitle_replace_chr <- ""
         html_txt <- gsub(wintitle_find_chr, wintitle_replace_chr, html_txt, fixed = TRUE)
       }

       ## Swap out the title
       if (TRUE %in% grepl(title_find_chr, html_txt, fixed = TRUE) ) {
         title_replace_chr <- tips_tbl[i, "title", drop = TRUE]
         if (is.na(title_replace_chr)) title_replace_chr <- ""
         html_txt <- gsub(title_find_chr, title_replace_chr, html_txt, fixed = TRUE)
       }

       ## Swap out the msg
       if (!TRUE %in% grepl(msg_find_chr, html_txt, fixed = TRUE) ) {
         stop(paste0("Can't find '", msg_find_chr, "' in ", basename(html_template_fn)))
       }
       msg_replace_chr <- tips_tbl[i, "msg_html", drop = TRUE]
       html_txt <- gsub(msg_find_chr, msg_replace_chr, html_txt, fixed = TRUE)

       ## Generate a file name
       fn_base <- tips_tbl[i, "fn_base", drop = TRUE]
       if (is.na(fn_base)) {
         fn_base <- paste0(sample(letters, 5, replace = TRUE), collapse = "")

         ## Write the new fn_base to the Google Sheet
         range_write(ss = gsheet_id,
                     sheet = gsheet_sheet,
                     data = data.frame(fn_base = fn_base),
                     range = paste0(fn_base_colltr, i + 1),
                     col_names = FALSE)

         if (!quiet) message(silver(paste0(" - generated new fn_base: ", fn_base, ".html")))
       }

       ## Write to disk
       tip_basename <- paste0(fn_base, ".html")
       tip_path_fn <- file.path(tips_dir, tip_basename)
       cat(html_txt, file = tip_path_fn, sep = "\n")
       if (!quiet) message(silver(paste0(" - Just wrote ", tip_basename, "\n")))

       ## Update all_done in Google Sheet
       range_write(ss = gsheet_id,
                   sheet = gsheet_sheet,
                   data = data.frame(all_done = TRUE),
                   range = paste0(all_done_colltr, i + 1),
                   col_names = FALSE)

       ## Update last_rendered in Google Sheet
       range_write(ss = gsheet_id,
                   sheet = gsheet_sheet,
                   data = data.frame(last_rendered = Sys.Date()),
                   range = paste0(lastrendered_colltr, i + 1),
                   col_names = FALSE)


       ## Construct tip URL
       tip_url <- paste0(url_base, "/", tip_basename)

       ## Update the tip URL in Google Sheet if needed
       new_url_yn <- !identical(tip_url, tips_tbl[i, "url", drop = TRUE])
       if (new_url_yn) {
         range_write(ss = gsheet_id,
                     sheet = gsheet_sheet,
                     data = tibble(url = gs4_formula(paste0("=HYPERLINK(\"", tip_url, "\")"))),
                     range = paste0(url_colltr, i + 1),
                     col_names = FALSE)
       }

       ## Update bitly if needed
       if (bitly && new_url_yn) {
         bitly_url <- urlshorteneR::bitly_create_bitlink(long_url = tip_url)$link
         range_write(ss = gsheet_id,
                     sheet = gsheet_sheet,
                     data = tibble(url = gs4_formula(paste0("=HYPERLINK(\"", bitly_url, "\")"))),
                     range = paste0(bitly_colltr, i + 1),
                     col_names = FALSE)
       }

       res <- c(res, tip_path_fn)

     }


  } else {
    if (!quiet) message(yellow(" - no rows to render!"))
  }

  if (length(res) > 0 && open_html) {
    sapply(res, browseURL)
  }

  invisible(res)

}


