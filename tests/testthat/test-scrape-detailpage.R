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
