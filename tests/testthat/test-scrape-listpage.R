context("test-scrape-listpage")

listpagehtml_trockner_01 <- xml2::read_html("geizhals-listpage-trockner-01.html")
listpagehtml_trockner_02 <- xml2::read_html("geizhals-listpage-trockner-02.html")
listpagehtml_nas_01 <- xml2::read_html("geizhals-listpage-nas-01.html")

test_that("product names are correctly identified", {
  r <- get_product_names(listpagehtml_trockner_02)
  expect_equal(r,
               c("Miele TCF620 WP Eco Wärmepumpentrockner",
                 "AEG Electrolux T97689IH3 Wärmepumpentrockner",
                 "Miele TCF630 WP Eco Wärmepumpentrockner",
                 "Miele TWF620 WP Eco Wärmepumpentrockner",
                 "Miele TMM840 WP Wärmepumpentrockner",
                 "Miele TMM843 WP SFinish&Eco Wifi Wärmepumpentrockner")
               )
  r <- get_product_names(listpagehtml_nas_01)
  expect_equal(r,
               c("Asustor AS6210T, 4x Gb LAN (90IX00X1-BW3S10)",
                 "QNAP Turbo Station TS-EC1080 Pro, 4x Gb LAN")
  )
})
