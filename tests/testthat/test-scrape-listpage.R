context("scrape listpage")

# setwd("./tests/testthat/")
listpagehtml_trockner_01 <- xml2::read_html("gh-lst-trockn-01.html")
listpagehtml_trockner_02 <- xml2::read_html("gh-lst-trockn-02.html")
listpagehtml_nas_01 <- xml2::read_html("gh-lst-nas-01.html")

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



