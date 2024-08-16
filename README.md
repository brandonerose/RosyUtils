
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RosyUtils <img src="man/figures/logo.png" align="right" height="200" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A staging package to demonstrate various custom R functions. Part R
packages maintained by Brandon Rose’s “Rosyverse” and some functions
used in other packages like RosyREDCap

## Installation

You can install the development version of RosyUtils like so:

``` r
# install remotes package if you don't have it
# install.packages("remotes") 
remotes::install_github("brandonerose/RosyUtils")
```

If you have any issues above download the most recent version of R at
RStudtio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## Microsoft Outlook Clean Up Example

The following functions represent the core functions of the package.

``` r

DB <- update_DB(DB) # update from redcap by checking log and using saved object 

DB <- transform_DB(DB) # transform to most basic forms, can be modified

DB <- clean_DB(DB)

DB <- summarize_DB(DB) #can use for subsets!

DB <- DB %>% drop_redcap_dir() #drops excel files with links to directory

DB <- summarize_DB(DB)

DB %>% save_summary() # will save summary data, look at the tabs!
```

## Future plans

- Future versions will demonstrate more advanced features already
  included!
- Documentation needs to be updated
- Need to add vignettes
- Open to collaboration/feedback

## Links

The RosyUtils package is at
[github.com/brandonerose/RosyUtils](https://github.com/brandonerose/RosyUtils "RosyUtils R package")
See instructions above. Install remotes and install RosyUtils

Donate if I helped you out and want more development (anything helps)!
[account.venmo.com/u/brandonerose](https://account.venmo.com/u/brandonerose "Venmo Donation")

For more R coding visit
[thecodingdocs.com/](https://www.thecodingdocs.com/ "TheCodingDocs.com")

For correspondence/feedback/issues, please email
<TheCodingDocs@gmail.com>!

Follow us on Twitter
[twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")

Follow me on Twitter
[twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")

[![TheCodingDocs.com](man/figures/TCD.png)](http://www.thecodingdocs.com)
