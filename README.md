# econpanel

## Summary

This is an R package for collecting and formatting response data from several surveys of economists conducted by the [Booth School of Business](https://www.chicagobooth.edu/) at the University of Chicago and the [Centre for Macroeconomics](http://cfmsurvey.org/).

I will update the dataset semesterly (but feel free to poke me if i seem to have forgotten!). The functions used to scrape the websites are not exported as a way of discouraging unnecessary scraping.

## Installation

Install the package as follows:

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("corybrunson/econpanel", build_vignettes = TRUE)
```

## Datasets

The package contains the following datasets:

* `igm` contains question statements and responses from the [Initiative on Global Markets Economic Experts Panel](http://www.igmchicago.org/igm-economic-experts-panel).
* `eigm` contains the corresponding data from the [IGM European panel](http://www.igmchicago.org/european-economic-experts-panel).
* `cfm` contains question statements and responses from the [Centre for Macroeconomics](http://cfmsurvey.org/).
* `planetmoney` contains question statements and response tallies (not individual responses) from [a 2016 story by Planet Money](http://www.npr.org/sections/money/2016/02/26/468298576/economists-on-candidates-proposals-mostly-bad) on economists' opinions about the U.S. presidential candidates' economic proposals. The panelists were drawn from the IGM pool.

Load the datasets as follows:

```r
data(planetmoney, package = "econpanel")
```

The vignette "exploration" illustrates some questions and answers pursuable through these datasets. Access it as follows:

```r
vignette(topic = "exploration", package = "econpanel")
```

## Acknowledgments

I borrowed some insight from code at [Chris Said's `economist_poll` repo](https://github.com/csaid/economist_poll). Also invaluable have been [Hadley Wickam's `rvest` package](http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/) and the [SelectorGadget](http://selectorgadget.com/) Chrome extension.
