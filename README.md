# econpanel

## Summary

This is an R package for updating and formatting survey data from the [Initiative on Global Markets Economic Experts Panel] [igm], collected and maintained by the Booth School of Business at the University of Chicago; and from the [Centre for Macroeconomics] [cfm].

Install the package as follows:

```r
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("corybrunson/econpanel")
```

I will update the dataset semesterly (but feel free to poke me if i seem to have forgotten!). The code used to scrape the website is not exported (i.e. it is not visible by executing the function name in an R session) as a way of discouraging unnecessary scraping.

[igm]: http://www.igmchicago.org/igm-economic-experts-panel
[cfm]: http://cfmsurvey.org/

## Datasets

The package contains three datasets, stored as `data.frame`s:

* `igm` contains question statements and responses from the [Initiative on Global Markets] [igm].
* `cfm` contains question statements and responses from the [Centre for Macroeconomics] [cfm].
* `planetmoney` contains question statements and response tallies (not individual responses) from [a 2016 story by Planet Money] [planetmoney] on economists' opinions about the U.S. presidential candidates' economic proposals. The panelists were drawn from the IGM pool.

[planetmoney]: http://www.npr.org/sections/money/2016/02/26/468298576/economists-on-candidates-proposals-mostly-bad

To load one of the datasets, use the `data()` function:

```r
library(econpanel)
data(planetmoney)
```

The vignette "exploration" illustrates some questions and answers pursuable through these datasets. Access it like this:

```r
vignette(topic = "exploration", package = "econpanel")
```

## Acknowledgments

I borrowed some insight from code at [Chris Said's `economist_poll` repo] [chris-said]. Also invaluable have been [Hadley Wickam's `rvest` package] [rvest] and the [SelectorGadget] [selectorgadget] Chrome extension.

[chris-said]: https://github.com/csaid/economist_poll
[rvest]: http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/
[selectorgadget]: http://selectorgadget.com/
