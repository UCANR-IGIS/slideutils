#' Generate elements for a Google Form Summary
#'
#' @param resp_tbl Responses tibble
#' @param rpt_tbl Report template
#' @param show_msg Show additional messages
#'
#' @details This function takes two tibbles: i) the responses from a Google Form Survey
#' (or any kind of survey, where the format of the responses mirrors that of Google Forms),
#' and ii) a tibble that specifies how the questions should be summarized and presented
#' in the output report.
#'
#' This function does not download any data from Google Sheets. It is presumed you have
#' already done that. It also doesn't do anything terribly useful with the summaries.
#' It is presumed you'll pass the object returned by this function to \code{su_gfr_report()}, or a similar
#' function that renders or analyzes the results.
#'
#' For an example of \code{rpt_tbl}, please see \href{http://www.google.com}{here}.

#' @export
#' @importFrom crayon silver red green yellow
#' @importFrom magrittr %>% extract extract2
#' @importFrom stats na.omit setNames
#' @import dplyr
#' @import htmltools

su_gfr_elems <- function(resp_tbl, rpt_tbl, show_msg = TRUE) {


  res <- list()

  ## Loop through the report elements
  for (i in 1:nrow(rpt_tbl)) {

    ## Extract some properties of this report element
    elem_prp <- as.list(rpt_tbl[i,])
    elem_type <- elem_prp$include_as
    elem_col <- elem_prp$column
    if (is.na(elem_prp$question_or_text)) {
      elem_qtext <- elem_col
    } else {
      elem_qtext <- elem_prp$question_or_text
    }

    if (show_msg) message(silver(paste0(" - row ", i + 1, ". ", substr(elem_qtext, 1, 50))))

    if (is.na(elem_type)) {
      message(yellow(paste0(" - row ", i + 1, ": report element type is missing")))

    } else {

      if (grepl("^h[1-3]", elem_type)) {
        ## Heading
        if (is.na(elem_qtext)) {
          message(yellow(paste0(" - row ", i + 1, ": question text missing")))
        } else {

          if (!is.na(elem_prp$find_replace)) {
            if (grepl("survey_size", elem_prp$find_replace)) {
              elem_qtext <- elem_qtext %>%
                gsub("\\[n\\]", nrow(resp_tbl), .)
            }
          }


          ## I can't add an element to res with c(), list(), or append(), because they
          ## all take the elements of the shing.tag (also a list) and make them separate elements
          ## res[[length(res) + 1]] <- get(elem_type, pos = "package:htmltools")(elem_qtext)
          res[[length(res) + 1]] <- get(elem_type)(elem_qtext)
        }


      } else if (elem_type == "p_desc") {
        ## Paragraph text
        if (is.na(elem_qtext)) {
          message(yellow(paste0(" - row ", i + 1, ": question text missing")))
        } else {

          if (!is.na(elem_prp$find_replace)) {
            if (grepl("survey_size", elem_prp$find_replace)) {
              elem_qtext <- elem_qtext %>%
                gsub("\\[n\\]", nrow(resp_tbl), .)
            }
          }

          ## res[[length(res) + 1]] <- p(elem_qtext, class = "desc")
          res[[length(res) + 1]] <- HTML(paste0("<p class='desc'>", elem_qtext, "</p>"))
        }

      } else if (elem_type == "bullets") {
        ## Bullets
        if (elem_prp$column %in% names(resp_tbl)) {

          # res[[length(res) + 1]] <- p(elem_qtext, class = "qtext")
          res[[length(res) + 1]] <- HTML(paste0("<p class='qtext'>", elem_qtext, "</p>"))

          ## Get the raw text values
          comments_li <- resp_tbl %>%
            pull(elem_prp$column) %>%
            na.omit() %>%
            trimws() %>%
            paste0("<li>", ., "</li>")

          ## Sort them if needed
          if (!is.na(elem_prp$sort)) {
            if (!elem_prp$sort %in% c("ASC", "DESC")) message(red(paste0(" - row ", i + 1, ": unknown value for sort_vals: ", elem_prp$sort)))
            comments_li <- sort(comments_li, decreasing = (elem_prp$sort == "DESC"))
          }

          res[[length(res) + 1]] <- HTML(paste0("<ul>",
                                                paste(comments_li, collapse = "\n"),
                                                "</ul>"))

        } else {
          message(yellow(paste0(" - row ", i + 1, ": could not find the column `", elem_prp$column, "`")))
        }

      } else if (substr(elem_type, 1, 5) == "histo" || elem_type == "pie") {

        ## This is a histogram (vertical or horizontal) or Pie chart
        if (elem_prp$column %in% names(resp_tbl)) {

          ## Initialize a placeholder for 'other' values
          other_vals_chr <- NA
          select_many_lbl <- NULL

          if (identical(elem_prp$multi_select, TRUE)) {

            if (identical(elem_prp$howmany_lbl, TRUE)) {
              select_many_lbl <- p("Select all that apply.", class = "howmany")
            }

            ## Need to get all the values
            if (is.na(elem_prp$all_vals)) {
              message(red(paste0(" - row ", i + 1, ": can not process a multi-select histo if all_vals is missing")))

            } else {
              ## Get the unparsed values
              unparsed_resp_vec <- resp_tbl %>% pull(elem_prp$column)

              ## Get a vector of all possible values
              all_vals_vec <- elem_prp$all_vals %>%  strsplit("\\|") %>% extract2(1) %>% trimws()

              vals_tab_df <- do.call(rbind,
                                     lapply(all_vals_vec,
                                            function(x) data.frame(resp = x,
                                                                   cnt = sum(grepl(x, unparsed_resp_vec, fixed = TRUE)))))
              ## fixed = TRUE is needed if the response has an embedded '(' or other character that is used in regex expresssions

              vals_tab_lst <- setNames(split(vals_tab_df[,2], seq(nrow(vals_tab_df))), vals_tab_df$resp )

              ## Get the 'other' values
              if ("Other…" %in% all_vals_vec) {

                # THIS APPROACH DIDN'T WORK - YOU CAN'T RELIABLY SPLIT ANSWERS AT COMMAS, BECAUSE SOME OF THE STANDARD
                # CHOICES MIGHT HAVE COMMAS IN THEM!!
                # unparsed_resp_split <- unparsed_resp_vec %>%
                #   strsplit(",", fixed = TRUE) %>%
                #   unlist() %>%
                #   trimws()
                #
                # other_vals_vec <- unparsed_resp_split[!unparsed_resp_split %in% all_vals_vec]
                #
                # if (length(other_vals_vec) > 0) {
                #   other_vals_chr <- paste(other_vals_vec, collapse = ", ")
                # }

                ## Delete all the known values from ther responses. If anything is left, that's the 'Other' responses
                resp_others <- unparsed_resp_vec

                ## Add ', ' after the last character for all non-empty responses
                resp_notempty_idx <- which(nchar(resp_others) > 0)
                resp_others[resp_notempty_idx] <- paste0(resp_others[resp_notempty_idx], ", ")

                ## Delete all known responses
                for (j in 1:length(all_vals_vec)) {
                  resp_others <- resp_others %>%
                    gsub(paste0(all_vals_vec[j], ", "), "", ., fixed = TRUE)
                }

                ## Delete those that are empty
                resp_others <- resp_others[resp_others!=""]

                ## If there's anything left, that's an 'other'
                if (length(resp_others) > 0) {
                  other_vals_chr <- paste("<li>", gsub(", $", "", resp_others) , "</li>", collapse = "\n")
                }

              }

            }


          } else {

            ## Not a multi-select column
            vals_tab_lst <- resp_tbl %>%
              pull(elem_prp$column) %>%
              na.omit() %>%
              table() %>%
              as.list()

            if (identical(elem_prp$howmany_lbl, TRUE)) {
              select_many_lbl <- p("Pick one.", class = "howmany")
            }


          }

          ## Add missing values to the frequency list
          if (!is.na(elem_prp$all_vals)) {
            ## Add any missing values to the list
            all_vals_vec <- elem_prp$all_vals %>%  strsplit("\\|") %>% extract2(1) %>% trimws()
            for (some_val in all_vals_vec) {
              if (!some_val %in% names(vals_tab_lst)) {
                vals_tab_lst[[some_val]] <- 0
              }
            }

            if (length(vals_tab_lst) > length(all_vals_vec)) {
              message(red(paste0(" - row ", i + 1, ": there are more unique responses than expected. Should this be multi-select?")))
            }

            ## Order the elements of vals_tab_lst in the same order as all_vals_vec
            # vals_tab_lst <- vals_tab_lst[ match(names(vals_tab_lst), all_vals_vec) ]
            vals_tab_lst <- vals_tab_lst[ match(all_vals_vec, names(vals_tab_lst)) ]

            ## Change the labels if needed
            if (!is.na(elem_prp$all_lbls)) {
              all_lbl_vec <- elem_prp$all_lbls %>%  strsplit("\\|") %>% extract2(1) %>% trimws()
              names(vals_tab_lst) <- all_lbl_vec
            }

          }  ## if !is.na(elem_prp$all_vals)

          if (!is.na(elem_prp$sort)) {
            if (!elem_prp$sort %in% c("ASC", "DESC")) message(red(paste0(" - row ", i + 1, ": unknown value for sort_vals: ", elem_prp$sort)))
            order_idx <- vals_tab_lst %>% unlist() %>% order(decreasing = (elem_prp$sort == "DESC"))
            vals_tab_lst <- vals_tab_lst[order_idx]
          }

          ## Create the frequeny table
          vals_tab_df <- data.frame(label = factor(names(vals_tab_lst), levels = names(vals_tab_lst)),
                                    cnt = vals_tab_lst %>% unlist() %>% as.numeric())

          if (elem_type == "pie") {
            histopie_plot <- ggplot2::ggplot(vals_tab_df, ggplot2::aes(x = "", y = cnt, fill = label)) +
              ggplot2::geom_bar(stat = "identity", width = 1, color="white") +
              ggplot2::coord_polar("y", start = 0) +
              ggplot2::theme_void() +
              ggplot2::theme(legend.title = ggplot2::element_blank()) +
              ggplot2::scale_fill_brewer(palette="Dark2")

          } else {
            histopie_plot <- ggplot2::ggplot(data = vals_tab_df, ggplot2::aes(x = label, y = cnt)) +
              ggplot2::geom_bar(stat="identity") +
              ggplot2::theme_gray() +
              ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank())

            ## Flip the axis if needed
            if (grepl("_v$", elem_type)) {
              histopie_plot <- histopie_plot + ggplot2::coord_flip()
            }

          }

          ## Add elements to res
          res[[length(res) + 1]] <- HTML(paste0("<p class='qtext'>", elem_qtext, "</p>"))

          if (!is.null(select_many_lbl)) {
            res[[length(res) + 1]] <- select_many_lbl
          }

          if (is.na(elem_prp$plot_params)) {
            plot_size_params <- "680|480|120"
          } else {
            ## Specific output parameters have been passed, so pass a custom list object
            plot_size_params <- elem_prp$plot_params

          }

          ggplot_specs <- list(histopie_plot, plot_size_params)
          class(ggplot_specs) <- c("list", "ggplot_with_params")
          res[[length(res) + 1]] <- ggplot_specs

          if (!is.na(other_vals_chr)) {
            res[[length(res) + 1]] <- HTML("<p style=\"margin-top:1em;\">Other:</p><ul>", other_vals_chr, "</ul>")
          }


        } else {
          message(yellow(paste0(" - row ", i + 1, ": could not find the column `", elem_prp$column, "`")))
        }


      } else if (elem_type == "map") {

        if (!requireNamespace("leaflet", quietly = TRUE)) stop("Sorry, this function requires the leaflet package. Please install it then try again.")

        if (elem_prp$column %in% names(resp_tbl)) {

          ## Turn the vector of csv coords (lon,lat) into a data frame
          coords_df <- resp_tbl %>%
            pull(elem_prp$column) %>%
            strsplit(",") %>%
            unlist() %>%
            as.numeric() %>%
            matrix(ncol = 2, byrow = TRUE) %>%
            as.data.frame() %>%
            setNames(c("lon", "lat"))

          ## Create a basic leaflet map with clusters
          m <- leaflet::leaflet(coords_df) %>%
            leaflet::addTiles() %>%
            leaflet::addCircleMarkers(stroke = FALSE, fillOpacity = 0.5, clusterOptions = 1)

          ## Add elements to res
          res[[length(res) + 1]] <- HTML(paste0("<p class='qtext'>", elem_qtext, "</p>"))
          res[[length(res) + 1]] <- m

        } else {
          message(yellow(paste0(" - row ", i + 1, ": could not find the column `", elem_prp$column, "`")))
        }



      } else if (elem_type == "skip") {



      } else {
        message(red(paste0(" - row ", i + 1, ": unknown report element type: ", elem_type)))

      }

    }

  }

  invisible(res)

}
