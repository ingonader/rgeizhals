context("scrape listpage")

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



