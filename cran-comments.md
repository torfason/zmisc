
## R CMD check results

There were no ERRORs or WARNINGs.

On r-hub.io Linux platforms and winbuilder, there were no NOTEs:

On r-hub.io, Windows Server 2022, R-devel, 64 bit platform, there was one note relating to MiKTeX:

    +---
    | * checking for detritus in the temp directory ... NOTE
    | Found the following files/directories:
    |   'lastMiKTeXException'
    +---

This second note is not reproducible locally or on other platforms, and seems not to affect the output, because the check reports no errors related to the PDF version of the manual:

    #> * checking PDF version of manual ... OK


## Downstream dependencies

There are currently no downstream dependencies for this package.


## Release summary

* This is the 0.2.2 release of zfit

* The release is motivated by a change in roxygen2 which caused issues with vignette building. The release fixes those issues.

* Package has been checked locally, on r-hub.io, and on winbuilder

* R CMD check ran without errors, warnings or notes, apart from "lastMiKTeXException" note (see above)
