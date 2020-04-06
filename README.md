
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doppelstudent

This is a (quick and dirty) tool for exploring plagiarism in open-ended
exam questions. It is designed for use with testvision csv output, but
could pretty easily be adapted for any open-ended exam data.

The shiny app can be fired up, and anyone can use it to upload a csv
file with the testvision output. The app does not store the data that
has been uploaded, but other than that no thought has been put into GDPR
issues.

## How to use

The package should be easily installable from github, using the devtools
package. For the current version, the development version of
tokenbrowser also needs to be installed.

``` r
devtools::install_github('kasperwelbers/tokenbrowser')
devtools::install_github('kasperwelbers/doppelstudent')

library(doppelstudent)
run_doppelstudent()
```
