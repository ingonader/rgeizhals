context("scrape listpage")

# setwd("./tests/testthat/")
listpagehtml_trockner_01 <- xml2::read_html(
  system.file("extdata", "gh-lst-trockn-01.html", package = "rgeizhals"))
listpagehtml_trockner_02 <- xml2::read_html(
  system.file("extdata", "gh-lst-trockn-02.html", package = "rgeizhals"))
listpagehtml_nas_01 <- xml2::read_html(
  system.file("extdata", "gh-lst-nas-01.html", package = "rgeizhals"))

## ========================================================================= ##
## parse_product_names()
## ========================================================================= ##

test_that("parse_product_names()
          identifies product names correctly", {
            r <- parse_product_names(listpagehtml_trockner_02)
            expect_equal(r,
                         c("Miele TCF620 WP Eco Wärmepumpentrockner",
                           "AEG Electrolux T97689IH3 Wärmepumpentrockner",
                           "Miele TCF630 WP Eco Wärmepumpentrockner",
                           "Miele TWF620 WP Eco Wärmepumpentrockner",
                           "Miele TMM840 WP Wärmepumpentrockner",
                           "Miele TMM843 WP SFinish&Eco Wifi Wärmepumpentrockner")
            )
            r <- parse_product_names(listpagehtml_nas_01)
            expect_equal(r,
                         c("Asustor AS6210T, 4x Gb LAN (90IX00X1-BW3S10)",
                           "QNAP Turbo Station TS-EC1080 Pro, 4x Gb LAN")
            )
          })


## ========================================================================= ##
## parse_ratings()
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
## parse_ratings_n()
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
## parse_offers_n()
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
## parse_listprice()
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
## parse_detailpage_urls()
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
              "https://geizhals.at/https://geizhals.at/miele-tcf620-wp-eco-waermepumpentrockner-a1599561.html?hloc=at",
              "https://geizhals.at/https://geizhals.at/aeg-electrolux-t97689ih3-waermepumpentrockner-a1669120.html?hloc=at",
              "https://geizhals.at/https://geizhals.at/miele-tcf630-wp-eco-waermepumpentrockner-a1519938.html?hloc=at",
              "https://geizhals.at/https://geizhals.at/miele-twf620-wp-eco-waermepumpentrockner-a1614538.html?hloc=at",
              "https://geizhals.at/https://geizhals.at/miele-tmm840-wp-waermepumpentrockner-a1397066.html?hloc=at",
              "https://geizhals.at/https://geizhals.at/miele-tmm843-wp-sfinish-eco-wifi-waermepumpentrockner-a1522652.html?hloc=at")
            )
            r <- parse_detailpage_urls(listpagehtml_nas_01)
            expect_equal(r, c(
              "https://geizhals.at/https://geizhals.at/asustor-as6210t-90ix00x1-bw3s10-a1472925.html?hloc=at",
              "https://geizhals.at/https://geizhals.at/qnap-turbo-station-ts-ec1080-pro-a1136118.html?hloc=at")
            )
          })

## ========================================================================= ##
## parse_single_listpage()
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
            expect_equal(r$prodname,
                         c("Miele TCF620 WP Eco Wärmepumpentrockner",
                           "AEG Electrolux T97689IH3 Wärmepumpentrockner",
                           "Miele TCF630 WP Eco Wärmepumpentrockner",
                           "Miele TWF620 WP Eco Wärmepumpentrockner",
                           "Miele TMM840 WP Wärmepumpentrockner",
                           "Miele TMM843 WP SFinish&Eco Wifi Wärmepumpentrockner"))
            expect_equal(r$rating,
                         c(NA, NA, 5.0, NA, NA, 4.9))
            expect_equal(r$rating_n,
                         c(NA, NA, 4, NA, NA, 8))
            expect_equal(r$offers_n,
                         c(9, 3, 22, 20, 1, 16))
            expect_equal(r$listprice,
                         c(1248.99, 1249.00, 1249.00, 1249.00, 1649.00, 1649.00))
            expect_equal(r$detailpage_url,
                         c(
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/miele-tcf620-wp-eco-waermepumpentrockner-a1599561.html?hloc=at"),
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/aeg-electrolux-t97689ih3-waermepumpentrockner-a1669120.html?hloc=at"),
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/miele-tcf630-wp-eco-waermepumpentrockner-a1519938.html?hloc=at"),
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/miele-twf620-wp-eco-waermepumpentrockner-a1614538.html?hloc=at"),
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/miele-tmm840-wp-waermepumpentrockner-a1397066.html?hloc=at"),
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/miele-tmm843-wp-sfinish-eco-wifi-waermepumpentrockner-a1522652.html?hloc=at")))
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
            expect_equal(r$detailpage_url,
                         c(
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/asustor-as6210t-90ix00x1-bw3s10-a1472925.html?hloc=at"),
                           paste0(system.file("extdata", "", package = "rgeizhals"),
                                  "/https://geizhals.at/qnap-turbo-station-ts-ec1080-pro-a1136118.html?hloc=at")))
            expect_equal(dim(r),
                         c(2, 6))
            })

## ========================================================================= ##
## parse_next_listpage_url()
## ========================================================================= ##

test_that("parse_next_listpage_url()
          identifes next listpage correctly", {
            r <- parse_next_listpage_url(listpagehtml_trockner_01)
            expect_equal(r, "https://geizhals.at/?cat=hwaeschtr&xf=1027_W%E4rmepumpentrockner~1296_10~1747_8~7641_40~7653_9&pg=2#productlist")
          })

test_that("parse_next_listpage_url()
          identifies non-existing next listpage correctly as NA", {
            r <- parse_next_listpage_url(listpagehtml_trockner_02)
            expect_equal(r, NA)

            r <- parse_next_listpage_url(listpagehtml_nas_01)
            expect_equal(r, NA)
            })


## ========================================================================= ##
## fetch_next_listpage()
## ========================================================================= ##

## ========================================================================= ##
## fetch_all_listpages()
## ========================================================================= ##

## ========================================================================= ##
## parse_all_listpages
## ========================================================================= ##


