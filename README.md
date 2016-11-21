# econpanel

## Summary

This is an R package for updating and formatting survey data from the [Initiative on Global Markets Economic Experts Panel] [igm], collected and maintained by the Booth School of Business at the University of Chicago; and from the [Centre for Macroeconomics] [cfm].

I will update the dataset included with the package as frequently as possible (as new survey responses are posted). The code used to scrape the website is not exported (i.e. it is not visible by executing the function name in an R session) as a way of discouraging unnecessary scraping.

[igm]: http://www.igmchicago.org/igm-economic-experts-panel
[cfm]: http://cfmsurvey.org/

## Acknowledgments

I borrowed some insight from code at [Chris Said's `economist_poll` repo] [chris-said]. Also invaluable have been [Hadley Wickam's `rvest` package] [rvest] and the [SelectorGadget] [selectorgadget] Chrome extension.

[chris-said]: https://github.com/csaid/economist_poll
[rvest]: http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/
[selectorgadget]: http://selectorgadget.com/
