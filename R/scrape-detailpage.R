
## get categories:
#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_detailpage_categories <- function(detailpagehtml) {
  ret <- detailpagehtml %>%
    html_nodes(css = ".gh-data-table__key") %>%
    html_text()
  return(ret)
}
#get_detailpage_categories(detailpagehtml)


#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_keyval_tbl <- function(detailpagehtml) {
  ## get keys (categories):
  keys_from_table <- get_detailpage_categories(detailpagehtml)

  ## get text of all table rows (some containing keys, some not):
  keys_value_text_all <- detailpagehtml %>%
    html_nodes(css = ".gh-data-table__row") %>% html_text()

  ## get keyval-text that contain keys:
  keys_value_text_sel <- stringr::str_subset(keys_value_text_all, paste(keys_from_table, collapse = "|"))

  ## get keys from keyvalue text:
  keys <- stringr::str_extract(keys_value_text_sel,  paste(keys_from_table, collapse = "|"))

  ## remove keys to get values:
  vals <- stringr::str_replace_all(keys_value_text_sel,  paste(keys, collapse = "|"), "")

  ## remove unnecessary characters:
  vals <- stringr::str_replace_all(vals, "[\n]", "")

  ## make data.frame:
  ret <- tibble::tibble(key = keys, value = vals)

  ## remove duplicates:
  ret <- ret[!duplicated(ret), ]
  return(ret)
}
#get_keyval_tbl(detailpagehtml)


#' Title
#'
#' @param detailpagehtml
#'
#' @return
#' @export
#'
#' @examples
get_prices <- function(detailpagehtml) {
  ## get prices:
  ret <- detailpagehtml %>%
    html_nodes(css = ".offer__price") %>%            ## get prices
    html_text()
  ## remove first entry (table header):
  ret <- ret[-1]

  ## convert to numerical:
  ret <- ret %>% stringr::str_replace_all("[^0-9,]", "") %>%  ## get numerical parts
    stringr::str_replace_all(",", "\\.") %>%                  ## "," comma to "." comma
    as.numeric()
  return(ret)
}
#get_prices(detailpagehtml)
