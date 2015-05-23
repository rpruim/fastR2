
## Test environments

* local OS X install, R version 3.2.0 Patched (2015-04-26 r68264)
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There has been one intermittant NOTE:

Found the following (possibly) invalid URLs:
  URL: http://lib.stat.cmu.edu/DASL/
  
  When I check the URL manually, I have no problems loading the page.
  
Also, win-builder flags AMS and Pruim as possible misspellings, but they
are correct.

## Downstream dependencies

Suggested by mosaic.  This update is driven by the recent mosaic update.
