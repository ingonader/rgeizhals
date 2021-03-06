#' Join details to listpage data
#'
#' Joins the data from the product detail pages to
#' the data from the category pages.
#'
#' @param dat_listpage A tibble (data.frame) containing
#'   all the data from the scraped geizhals category
#'   listing pages, with the join column
#'   \code{detailpage_url}.
#' @param dat_detailpage A tibble (data.frame) containing
#'   the data from corresponding detail pages, with the
#'   column specifying the \code{url}.
#'
#' @return A tibble (data.frame) containing both the data
#'   from the category pages as well as the corresponding
#'   detail page data (or \code{NA} if no match is present).
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' ## fetch html of all listing pages:
#' listpagehtml_list <- fetch_all_listpages(url_geizhals, max_pages = 2)
#' ## and parse information of these listing pages:
#' dat_listpage <- parse_all_listpages(listpagehtml_list)
#' ## get all (or some) detailpages:
#' detailpagehtml_list <- fetch_all_detailpage_html(dat_listpage$detailpage_url,
#'                                                  max_items = 5)
#' dat_detailpage <- parse_all_detailpages(detailpagehtml_list)
#' dat_geizhals <- join_details_to_listpage(dat_listpage,
#'                                          dat_detailpage)
#' head(dat_geizhals)
#' }
#' @export
join_details_to_listpage <- function(dat_listpage, dat_detailpage) {
  ret <- dplyr::left_join(dat_listpage,
                          dat_detailpage,
                          by = c("detailpage_url" = "url"))
  return(ret)
}


#' Get data from geizhals list and detail pages
#'
#' Starting from an url, get the information on all items in this
#' list (and the following pages), as well as the information
#' in the detail pages that correspond to these items.
#'
#' @inheritParams parse_detailpage_urls
#' @inheritParams fetch_all_listpages
#' @inheritParams fetch_all_detailpage_html
#' @param firstlistpageurl The url of a single geizhals page listing
#'   items in a selected category.
#' @param max_pages Maximal number of pages to be scraped. Default is 10.
#' @param domain Character vector of length one specifying the domain.
#'   If omitted, domain is extracted from \code{firstlistpageurl}.
#'
#' @return A tibble (data.frame) with all the information in
#'   the list page and the corresponding detail pages.
#'   Each row corresponds to one product.
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' dat_gh <- get_geizhals_data(url_geizhals, max_pages = 1)
#' head(dat_gh)
#'
#' dat_gh <- get_geizhals_data(url_geizhals, max_items = 3,
#'   delay_listpage = 1, delay_detailpage = 1)
#' head(dat_gh)
#' }
#'
#' @export
get_geizhals_data <- function(firstlistpageurl,
                              max_pages = 10,
                              max_items = Inf,
                              delay_listpage = NA,
                              delay_detailpage = NA,
                              domain = NA) {
  ## get domain, if none is specified:
  if (is.na(domain)) domain <- extract_domain(firstlistpageurl)

  ## get all listpages:
  listpagehtml_list <- fetch_all_listpages(firstlistpageurl,
                                          max_pages = max_pages,
                                          delay_listpage = delay_listpage,
                                          domain = domain)
  dat_listpage <- parse_all_listpages(listpagehtml_list,
                                      domain = domain)

  ## get all detailpages:
  detailpagehtml_list <- fetch_all_detailpage_html(
    parse_all_listpages(listpagehtml_list,
                        domain = domain)$detailpage_url,
    max_items = max_items,
    delay_detailpage = delay_detailpage
  )
  dat_detailpage <- parse_all_detailpages(detailpagehtml_list)

  ## join listpage data to detailpage data:
  dat_geizhals <- join_details_to_listpage(dat_listpage,
                                           dat_detailpage)
  return(dat_geizhals)
}

#' Get summary table for some feature
#'
#' Produces a frequency table of all features in a specific column.
#' Especially useful if one column contains a list of features.
#'
#' @param dat_gh A tibble (data.frame) containing the data.
#' @param col The column name that should be summarized
#'   (character vector of length 1).
#' @param sep The separator to use as split, if the column contains
#'   a list of features. Character vector of length 1. Default is ",".
#'
#' @return A frequency table of features.
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=acam35"
#' dat_gh <- get_geizhals_data(url_geizhals, max_pages = 1)
#' get_feature_summary(dat_gh, col = "Typ")
#' }
#'
#' @export
get_feature_summary <- function(dat_gh, col, sep = ",") {
  dat_gh[[col]] %>%
    stringr::str_split(sep) %>%
    purrr::map(stringr::str_trim) %>%
    unlist() %>%
    table() %>%
    sort(decreasing = TRUE)
}

#' Extract a binary indicator for some feature
#'
#' Some features from product detailpages contain a delimited
#' list with multiple features. This function extracts a
#' given feature from this list, by searching for a regex
#' expression in that given column.
#'
#' @param dat_gh A tibble (data.frame), usually obtained via
#'   \code{get_geizhals_data}.
#' @param col A character vector of length one, specifying
#'   the name of the column in \code{dat_gh} that should be
#'   parsed for the feature.
#' @param regex A character vector of length one with a
#'   regular expression. The column \code{col} is scanned
#'   for that regular expression.
#'
#' @return A vector of length \code{nrow(dat_gh)}, containing
#'   1 if a feature is present in a given product (i.e., if the
#'   regular expression is found), 0 if the feature is not found,
#'   and \code{NA} if that column is missing (i.e., that category
#'   was not present in the detailed product description page).
#'
#' @examples
#' \dontrun{
#' url_geizhals <- "https://geizhals.at/?cat=hwaeschtr"
#' dat_gh <- get_geizhals_data(url_geizhals, max_pages = 1)
#' extract_feature_ind(dat_gh, col = "Ausstattung", regex = "wartungsfreier Kondensator")
#' extract_feature_ind(dat_gh, col = "Ausstattung", regex = "Anschlussmöglichkeit")
#' extract_feature_ind(dat_gh, col = "Ausstattung",
#'   regex = "Anschlussmöglichkeit.*Kondenswasserablauf")
#' }
#'
#' @export
extract_feature_ind <- function(dat_gh, col, regex) {
  dat_gh[[col]] %>%
    stringr::str_detect(regex) %>%
    as.numeric()
}

#' Extract the domain from the full URL
#'
#' Extracts the domain (usually \code{https://geizhals.at} or
#' \code{https://geizhals.at}, but may also be the path to a
#' local file for testing or debugging.)
#'
#' @param url_str URL of some (geizhals) page.
#'   Character vector of length 1.
#'
#' @return The domain up to (but not including) the last "/"
#'   character, as a character vector of length 1.
#' @examples
#' extract_domain("https://geizhals.eu/?cat=monlcd19wide")
#' extract_domain(paste0("https://geizhals.at/?cat=hwaeschtr&",
#'   "xf=1027_W%E4rmepumpentrockner%7E1296_10%7E1747_8%7E7641_40%7E7653_9"))
#' extract_domain(paste0("https://geizhals.at/lg-electronics-x-",
#'   "screen-k500n-weiss-a1419167.html?hloc=at"))
#' extract_domain(system.file("extdata", "gh-lst-trockn-01.html",
#'   package = "rgeizhals"))
#'
#' @export
extract_domain <- function(url_str) {
  ## get everything up to the last slash:
  ret <- stringr::str_extract(url_str, "^.*/")
  ## now remove that last character (slash):
  ret <- stringr::str_sub(ret, 1, -2)
  return(ret)
}
