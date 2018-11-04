context("scrape listpage")

# setwd("./tests/testthat/")  # nolint
listpagehtml_trockner_01 <- xml2::read_html(
  system.file("extdata", "gh-lst-trockn-01.html", package = "rgeizhals"))
listpagehtml_trockner_02 <- xml2::read_html(
  system.file("extdata", "gh-lst-trockn-02.html", package = "rgeizhals"))
listpagehtml_nas_01 <- xml2::read_html(
  system.file("extdata", "gh-lst-nas-01.html", package = "rgeizhals"))

listpagehtml_list_trockner <- list(
  listpagehtml_trockner_01,
  listpagehtml_trockner_02
)

## ========================================================================= ##
## parse_product_names
## ========================================================================= ##

test_that("parse_product_names()
          identifies product names correctly", {
            r <- parse_product_names(listpagehtml_trockner_02)
            expect_equal(
              r,
              c("Miele TCF620 WP Eco Wärmepumpentrockner",
                "AEG Electrolux T97689IH3 Wärmepumpentrockner",
                "Miele TCF630 WP Eco Wärmepumpentrockner",
                "Miele TWF620 WP Eco Wärmepumpentrockner",
                "Miele TMM840 WP Wärmepumpentrockner",
                "Miele TMM843 WP SFinish&Eco Wifi Wärmepumpentrockner"
              )
            )
            r <- parse_product_names(listpagehtml_nas_01)
            expect_equal(
              r,
              c("Asustor AS6210T, 4x Gb LAN (90IX00X1-BW3S10)",
                "QNAP Turbo Station TS-EC1080 Pro, 4x Gb LAN"
              )
            )
          })


## ========================================================================= ##
## parse_ratings
## ========================================================================= ##

test_that("parse_ratings()
          identifies product ratings correctly", {
            r <- parse_ratings(listpagehtml_trockner_02)
            expect_equal(r,
                         c(NA, NA, 5.0, NA, NA, 4.9)
            )
            r <- parse_ratings(listpagehtml_nas_01)
            expect_equal(r,
                         as.numeric(c(NA, NA))
            )
          })

## ========================================================================= ##
## parse_ratings_n
## ========================================================================= ##

test_that("parse_ratings_n()
          identifies number of product ratings correctly", {
            r <- parse_ratings_n(listpagehtml_trockner_02)
            expect_equal(r,
                         c(NA, NA, 4, NA, NA, 8)
            )
            r <- parse_ratings_n(listpagehtml_nas_01)
            expect_equal(r,
                         as.numeric(c(NA, NA))
            )
          })

## ========================================================================= ##
## parse_offers_n
## ========================================================================= ##

test_that("parse_offers_n()
          identifies number of offers correctly", {
            r <- parse_offers_n(listpagehtml_trockner_02)
            expect_equal(r,
                         c(9, 3, 22, 20, 1, 16)
            )
            r <- parse_offers_n(listpagehtml_nas_01)
            expect_equal(r,
                         c(13, 1)
            )
          })

## ========================================================================= ##
## parse_listprice
## ========================================================================= ##

test_that("parse_listprice()
          parses prices correctly", {
            r <- parse_listprice(listpagehtml_trockner_02)
            expect_equal(r,
                         c(1248.99, 1249.00, 1249.00, 1249.00, 1649.00, 1649.00)
            )
            r <- parse_listprice(listpagehtml_nas_01)
            expect_equal(r,
                         c(920.89, 2309.55))
          })

## ========================================================================= ##
## parse_detailpage_urls
## ========================================================================= ##

## Note:
## Downloaded pages contain the full URL, instead the shortened URL starting
## with a "." and only the last part of the URL.
## Hence, this function adds the domain to the URL found in the html file,
## which, in this case, is duplicated.

test_that("parse_detailpage_urls()
          parses urls correctly", {
            r <- parse_detailpage_urls(listpagehtml_trockner_02)
            expect_equal(r, c(
              "https://geizhals.at/https://geizhals.at/miele-tcf620-wp-eco-waermepumpentrockner-a1599561.html?hloc=at",       # nolint
              "https://geizhals.at/https://geizhals.at/aeg-electrolux-t97689ih3-waermepumpentrockner-a1669120.html?hloc=at",  # nolint
              "https://geizhals.at/https://geizhals.at/miele-tcf630-wp-eco-waermepumpentrockner-a1519938.html?hloc=at",       # nolint
              "https://geizhals.at/https://geizhals.at/miele-twf620-wp-eco-waermepumpentrockner-a1614538.html?hloc=at",       # nolint
              "https://geizhals.at/https://geizhals.at/miele-tmm840-wp-waermepumpentrockner-a1397066.html?hloc=at",           # nolint
              "https://geizhals.at/https://geizhals.at/miele-tmm843-wp-sfinish-eco-wifi-waermepumpentrockner-a1522652.html?hloc=at")  # nolint
            )
            r <- parse_detailpage_urls(listpagehtml_nas_01)
            expect_equal(r, c(
              "https://geizhals.at/https://geizhals.at/asustor-as6210t-90ix00x1-bw3s10-a1472925.html?hloc=at",                # nolint
              "https://geizhals.at/https://geizhals.at/qnap-turbo-station-ts-ec1080-pro-a1136118.html?hloc=at")               # nolint
            )
          })

## ========================================================================= ##
## parse_single_listpage
## ========================================================================= ##

## Note:
## Downloaded pages contain the full URL, instead the shortened URL starting
## with a "." and only the last part of the URL.
## Hence, this function adds the domain to the URL found in the html file,
## which, in this case, is duplicated.

test_that("parse_single_listpage()
          parses a complete listpage correctly", {
            r <- parse_single_listpage(
              listpagehtml_trockner_02,
              domain = system.file("extdata", "", package = "rgeizhals"))
            expect_equal(
              r$prodname,
              c("Miele TCF620 WP Eco Wärmepumpentrockner",
                "AEG Electrolux T97689IH3 Wärmepumpentrockner",
                "Miele TCF630 WP Eco Wärmepumpentrockner",
                "Miele TWF620 WP Eco Wärmepumpentrockner",
                "Miele TMM840 WP Wärmepumpentrockner",
                "Miele TMM843 WP SFinish&Eco Wifi Wärmepumpentrockner")
            )
            expect_equal(r$rating,
                         c(NA, NA, 5.0, NA, NA, 4.9))
            expect_equal(r$rating_n,
                         c(NA, NA, 4, NA, NA, 8))
            expect_equal(r$offers_n,
                         c(9, 3, 22, 20, 1, 16))
            expect_equal(r$listprice,
                         c(1248.99, 1249.00, 1249.00,
                           1249.00, 1649.00, 1649.00))
            expect_equal(
              r$detailpage_url,
              c(
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/miele-tcf620-wp-eco-",       # nolint
                       "waermepumpentrockner-a1599561.html?hloc=at"),
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/aeg-electrolux-t97689ih3-",  # nolint
                       "waermepumpentrockner-a1669120.html?hloc=at"),
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/miele-tcf630-wp-eco-",       # nolint
                       "waermepumpentrockner-a1519938.html?hloc=at"),
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/miele-twf620-wp-eco-",       # nolint
                       "waermepumpentrockner-a1614538.html?hloc=at"),
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/miele-tmm840-wp-",           # nolint
                       "waermepumpentrockner-a1397066.html?hloc=at"),
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/miele-tmm843-wp-sfinish-",   # nolint
                       "eco-wifi-waermepumpentrockner-",
                       "a1522652.html?hloc=at"))
            )
            expect_equal(dim(r),
                         c(6, 6))

            r <- parse_single_listpage(
              listpagehtml_nas_01,
              domain = system.file("extdata", "", package = "rgeizhals"))
            expect_equal(r$prodname,
                         c("Asustor AS6210T, 4x Gb LAN (90IX00X1-BW3S10)",
                           "QNAP Turbo Station TS-EC1080 Pro, 4x Gb LAN"))
            expect_equal(r$rating,
                         as.numeric(c(NA, NA)))
            expect_equal(r$rating_n,
                         as.numeric(c(NA, NA)))
            expect_equal(r$offers_n,
                         c(13, 1))
            expect_equal(r$listprice,
                         c(920.89, 2309.55))
            expect_equal(
              r$detailpage_url,
              c(
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/asustor-as6210t-",   # nolint
                       "90ix00x1-bw3s10-a1472925.html?hloc=at"),
                paste0(system.file("extdata", "", package = "rgeizhals"),
                       "/https://geizhals.at/qnap-turbo-",        # nolint
                       "station-ts-ec1080-pro-a1136118.html?hloc=at"))
              )
            expect_equal(dim(r),
                         c(2, 6))
            })

## ========================================================================= ##
## parse_next_listpage_url
## ========================================================================= ##

test_that("parse_next_listpage_url()
          identifes next listpage correctly", {
            r <- parse_next_listpage_url(listpagehtml_trockner_01)
            expect_equal(r,
                         paste0("https://geizhals.at/?cat=hwaeschtr&",
                                "xf=1027_W%E4rmepumpentrockner~1296_10",
                                "~1747_8~7641_40~7653_9&pg=2#productlist")) # nolint
          })

test_that("parse_next_listpage_url()
          identifies non-existing next listpage correctly as NA", {
            r <- parse_next_listpage_url(listpagehtml_trockner_02)
            expect_equal(r, NA)

            r <- parse_next_listpage_url(listpagehtml_nas_01)
            expect_equal(r, NA)
            })


## ========================================================================= ##
## fetch_next_listpage
## ========================================================================= ##

## ========================================================================= ##
## fetch_all_listpages
## ========================================================================= ##

## ========================================================================= ##
## parse_all_listpages
## ========================================================================= ##

test_that("parse_all_listpages()
          parses the list of htmls corretly", {
            r <- parse_all_listpages(listpagehtml_list_trockner, domain = "")
            expect_equal(r$rating,
                         c(NA, 5.0, 4.9, 4.9, 4.6, 5.0, NA, 4.5, 4.8, 5.0,
                           5.0, 5.0, NA, NA, 4.9, NA, 4.0, 4.8, 1.0, NA, 5.0,
                           5.0, 5.0, 5.0, NA, 4.8, 4.9, NA, NA, NA, NA, NA,
                           5.0, NA, NA, 4.9))
            expect_equal(r$rating_n,
                         c(NA, 1, 10, 8, 47, 2, NA, 66, 54, 3, 1, 1, NA, NA,
                           35, NA, 1, 2, 1, NA, 1, 8, 1, 7, NA, 29, 10, NA,
                           NA, NA, NA, NA, 4, NA, NA, 8))
            expect_equal(r$offers_n,
                         c(4, 20, 6, 3, 5, 12, 14, 1, 4, 2, 3, 17, 9, 10, 3,
                           3, 2, 1, 3, 8, 1, 24, 18, 6, 19, 25, 3, 2, 3, 4,
                           9, 3, 22, 20, 1, 16))
            expect_equal(r$listprice,
                         c(459.08, 495.00, 542.23, 549.00, 549.00, 599.00,
                           629.00, 633.75, 639.00, 649.00, 649.00, 649.00,
                           649.00, 649.00, 669.00, 719.00, 775.00, 839.00,
                           849.00, 899.00, 936.76, 999.00, 999.00, 999.00,
                           1049.00, 1049.00, 1099.00, 1099.00, 1149.00,
                           1198.99, 1248.99, 1249.00, 1249.00, 1249.00,
                           1649.00, 1649.00))
            expect_equal(dim(r),
                         c(36, 6))
          })
