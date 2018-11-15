context("scrape detailpage")

# setwd("./tests/testthat/")   # nolint
detailpagehtml_trockner_01 <- xml2::read_html(
  system.file("extdata", "gh-dp-trockn-01.html", package = "rgeizhals"))
detailpagehtml_trockner_02 <- xml2::read_html(
  system.file("extdata", "gh-dp-trockn-02.html", package = "rgeizhals"))
detailpagehtml_nas_01 <- xml2::read_html(
  system.file("extdata", "gh-dp-nas-01.html", package = "rgeizhals"))

## ========================================================================= ##
## parse_detailpage_categories
## ========================================================================= ##

#paste0("\"", r, "\"") %>% paste(collapse = ", ") %>% paste0("c(", ., ")") %>% cat()  # nolint
#paste0("\"", r[["value"]], "\"") %>% paste(collapse = ", ") %>% paste0("c(", ., ")") %>% cat()  # nolint
#detailpagehtml <- detailpagehtml_trockner_02                                         # nolint

test_that("parse_detailpage_categories()
          parses categories correctly", {
            r <- parse_detailpage_categories(detailpagehtml_trockner_01)
            expect_equal(
              r,
              c("Energieeffizienzklasse", "Kondensationseffizienzklasse",
                "Farbe", "Fassungsvermögen", "Programmanzahl", "Programme",
                "Zusatzoptionen", "Display", "Ausstattung", "Sicherheit",
                "Wartungshinweise", "Geräuschentwicklung", "Standardprogramm",
                "Energieverbrauch", "Bauart", "Abmessungen (HxBxT)", "Gewicht",
                "Gelistet seit")
            )
            r <- parse_detailpage_categories(detailpagehtml_nas_01)
            expect_equal(
              r,
              c("Festplatte", "Intern", "Extern", "Zusätzliche Anschlüsse",
                "RAID-Level", "CPU", "RAM", "Lüfter", "Leistungsaufnahme",
                "Abmessungen (BxHxT)", "Gewicht", "Besonderheiten",
                "Herstellergarantie", "Gelistet seit")
            )
          })

## ========================================================================= ##
## parse_keyval_tbl
## ========================================================================= ##

test_that("parse_keyval_tbl()
          parses values and keys correctly", {
            r <- parse_keyval_tbl(detailpagehtml_trockner_01)
            expect_equal(dim(r),
                         c(18, 2)
            )
            expect_equal(
              r[["key"]],
              parse_detailpage_categories(detailpagehtml_trockner_01)
            )
            expect_equal(
              r[["value"]][1:12],                       ## fix this
              c("A+++", "A", "weiß", "8.00kg", "12",
                "Baumwolle, Pflegeleicht, Feinwäsche, Oberhemden, Finish Wolle, Express, Jeans, Outdoor, Imprägnieren, Lüften warm, Vorbügeln", # nolint
                "Knitterschutz, Schonen plus", "ja",
                "Restlaufzeitanzeige, Startzeitvorwahl 24h, Signal am Programmende, Trommelbeleuchtung, Anschlussmöglichkeit und Schlauch für direkten Kondenswasserablauf, Reversierautomatik, Duftautomatik (FragranceDos), wartungsfreier Kondensator",  # nolint
                "Kindersicherung",
                "Anzeige für Filter reinigen, Kondensatbehälter leeren",
                "64dB(A)") #,
                #"177min", "171kWh/\vJahr",
                #"Standgerät, unterbaufähig, säulenfähig",
                #"850x596x636mm", "59.00kg", "27.03.2017, 13:03")
            )

            r <- parse_keyval_tbl(detailpagehtml_nas_01)
            expect_equal(dim(r),
                         c(14, 2)
            )
            expect_equal(
              r[["key"]],
              parse_detailpage_categories(detailpagehtml_nas_01)
            )
            expect_equal(
              r[["value"]][-c(1, 2, 4, 5)],            ## fix this
              c(# "N/A", "10x 2.5\"/3.5\", SATA 6Gb/s, Hot-Swap",
                "4x Gb LAN",
                # "3x USB-A 3.0 (Host), 2x USB-A 2.0 (Host), 2x eSATA 6Gb/s, 1x HDMI 1.4, 1x S/PDIF (optisch)",   # nolint
                # "0/1/5/6/10/JBOD/Single",
                "Intel Celeron N3160, 4x 1.60GHz",
                "4GB DDR3L SO-DIMM (max. 8GB)", "2x 120mm",
                "68.3W (Betrieb), 34.4W (Leerlauf)", "293x215.5x230mm",
                "6.20kg",
                "LCD, Kodi, iSCSI, 256bit AES-Verschlüsselung, FTP-Server, iTunes-Server, BitTorrent-Client, WebDAV, vier IP-Kamera Lizenzen", # nolint
                "drei Jahre (Abwicklung über ASUS)", "08.07.2016, 15:11")
            )
          })


## ========================================================================= ##
## parse_prices
## ========================================================================= ##

test_that("parse_prices()
          parses prices correctly", {
            r <- parse_prices(detailpagehtml_trockner_01)
            expect_equal(
              r,
              c(1248.99, 1249.00, 1249.00, 1299.00, 1408.00)
            )

            r <- parse_prices(detailpagehtml_nas_01)
            expect_equal(
              r,
              c(920.29, 920.30, 920.30, 920.30, 923.92, 925.21, 929.00,
                952.00, 979.27, 1000.90, 1019.66, 1028.10)
            )
          })

## ========================================================================= ##
## calc_price_summary
## ========================================================================= ##

test_that("calc_price_summary()
          works as expected", {
            r <- calc_price_summary(detailpagehtml_trockner_01)
            expect_equal(
              r %>%
                dplyr::filter(key == "price_min") %>%
                dplyr::pull(value),
              min(parse_prices(detailpagehtml_trockner_01))
            )
            expect_equal(
              r %>%
                dplyr::filter(key == "price_median") %>%
                dplyr::pull(value),
              median(parse_prices(detailpagehtml_trockner_01))
            )
            expect_equal(
              r %>%
                dplyr::filter(key == "price_2nd_min") %>%
                dplyr::pull(value),
              sort(parse_prices(detailpagehtml_trockner_01),
                   decreasing = FALSE)[2]
            )
            expect_equal(
              r %>%
                dplyr::filter(key == "price_3rd_min") %>%
                dplyr::pull(value),
              sort(parse_prices(detailpagehtml_trockner_01),
                   decreasing = FALSE)[3]
            )

            r <- calc_price_summary(detailpagehtml_nas_01)
            expect_equal(
              r[["value"]],
              c(920.290, 920.300, 920.300, 927.105)
            )
          })

## ========================================================================= ##
## parse_single_detailpage
## ========================================================================= ##

test_that("parse_single_detailpage()
          works as expected", {
            r <- parse_single_detailpage(detailpagehtml_trockner_01)
            expect_equal(
              r[["key"]],
              c(parse_keyval_tbl(detailpagehtml_trockner_01)[["key"]],
                calc_price_summary(detailpagehtml_trockner_01)[["key"]])
            )
            expect_equal(
              r[["value"]],
              c(parse_keyval_tbl(detailpagehtml_trockner_01)[["value"]],
                calc_price_summary(detailpagehtml_trockner_01)[["value"]])
            )

            r <- parse_single_detailpage(detailpagehtml_nas_01)
            expect_equal(
              r[["key"]],
              c(parse_keyval_tbl(detailpagehtml_nas_01)[["key"]],
                calc_price_summary(detailpagehtml_nas_01)[["key"]])
            )
            expect_equal(
              r[["value"]],
              c(parse_keyval_tbl(detailpagehtml_nas_01)[["value"]],
                calc_price_summary(detailpagehtml_nas_01)[["value"]])
            )
          })
