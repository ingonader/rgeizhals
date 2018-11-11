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
