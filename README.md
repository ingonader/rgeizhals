# `rgeizhals`: An R package to scrape [geizhals.at](https://geizhals.at/)

This is an R package to scrape [https://geizhals.at/](https://geizhals.at/). 

## Installation

Install the package using 

```r
devtools::install_github("ingonader/rgeizhals")
```

## Usage

Pick a categoriy on [https://geizhals.at/](https://geizhals.at/), until there is a listing page of items. Choose filters of your liking (to limit the request made to [https://geizhals.at/](https://geizhals.at/) during scraping). Copy the URL and use it as parameter in the functions of the package:

```r
url_geizhals <- "https://geizhals.at/?cat=acam35"
dat_gh <- get_geizhals_data(url_geizhals, max_pages = 1)
head(dat_gh)
```

```
# A tibble: 6 x 18
  prodname                                    rating rating_n offers_n
  <chr>                                        <dbl>    <dbl>    <dbl>
1 Fujifilm Instax Mini 9 hellblau                4.8       28       46
2 Fujifilm Instax Mini 9 weiß                    4.5       11       53
3 Polaroid OneStep 2 weiß                        5          1       33
4 Fujifilm Instax Mini 90 Neo Classic schwarz    4.9        8       59
5 Fujifilm Instax Square SQ6 grau (16581410)     4          1       17
6 Fujifilm Instax Square SQ6 weiß (16581393)    NA         NA       19
  detailpage_url                                                                        Aufnahmeformat
  <chr>                                                                                 <chr>         
1 https://geizhals.at/fujifilm-instax-mini-9-hellblau-a1600543.html?hloc=at             62x46mm       
2 https://geizhals.at/fujifilm-instax-mini-9-weiss-a1600541.html?hloc=at                62x46mm       
3 https://geizhals.at/polaroid-onestep-2-weiss-a1690635.html?hloc=at                    62x46mm       
4 https://geizhals.at/fujifilm-instax-mini-90-neo-classic-schwarz-a1032357.html?hloc=at 62x46mm       
5 https://geizhals.at/fujifilm-instax-square-sq6-grau-16581410-a1817585.html?hloc=at    62x62mm       
6 https://geizhals.at/fujifilm-instax-square-sq6-weiss-16581393-a1817586.html?hloc=at   62x62mm       
  Besonderheiten Blitz                   Fokus    `Gelistet seit`   Gewicht Objektiv           price_2nd_min
  <chr>          <chr>                   <chr>    <chr>             <chr>   <chr>              <chr>        
1 NA             integriert (0.6 - 2.7m) 0.6m - ∞ 29.03.2017, 11:25 307g    60mm, f/​12.7       67.92        
2 NA             integriert (0.6 - 2.7m) 0.6m - ∞ 29.03.2017, 11:24 307g    60mm, f/​12.7       69.31        
3 USB            integriert              0.6m - ∞ 14.09.2017, 15:01 460g    106mm              99.9         
4 NA             integriert              0.3m - ∞ 19.11.2013, 14:19 296g    60mm, f/​12.7       107.81       
5 NA             integriert (0.3 - 2.7m) 0.3m - ∞ 15.05.2018, 08:28 393g    65mm, 75mm, f/​12.6 129.56       
6 NA             integriert (0.3 - 2.7m) 0.3m - ∞ 15.05.2018, 08:33 393g    65mm, 75mm, f/​12.6 127.91       
  price_3rd_min price_median price_min Typ              Verschlusszeit
  <chr>         <chr>        <chr>     <chr>            <chr>         
1 68.84         77.35        66.9      Sofortbildkamera 1/​60s         
2 69.31         78.95        69.29     Sofortbildkamera 1/​60s         
3 99.9          119.59       99.9      Sofortbildkamera NA            
4 107.81        122.17       107.8     Sofortbildkamera 1/​400s        
5 129.75        138          129.55    Sofortbildkamera 1/​6s - 1/​400s 
6 129.55        139          127.9     Sofortbildkamera 1/​6s - 1/​400s 
```



