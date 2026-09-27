
## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

Outstanding NOTEs on 
https://ftp.uni-erlangen.de/cran/web/checks/check_results_zmisc.html
have been fixed.

- Authors@R field has been added

- R >= 4.1.0 dependency has been added


## Downstream dependencies

There are currently no downstream dependencies for this package.

A `revdepcheck` reports no regressions.


## Release summary

* This is the 0.3.0 release of zmisc

* The release adds a family of argument check functions (`chk_*()`),
  as well as `glue_vector()`, `asciify()`, `yencode()` and `ydecode()`

* The release includes breaking changes to `lookup()`, `lookuper()` and
  `zingle()`, and renames `ll_assert_labelled()` to `ll_chk_labelled()`,
  as described in NEWS.md

* Package has been checked locally, on r-hub, and on winbuilder

* R CMD check ran without errors, warnings or notes
