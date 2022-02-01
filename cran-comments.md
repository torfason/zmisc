
## R CMD check results

There were no ERRORs or WARNINGs.

On r-hub.io Linux platforms and winbuilder, there was one NOTE:

    +---
    | * checking CRAN incoming feasibility ... NOTE
    | Maintainer: ‘Magnus Thor Torfason <m@zulutime.net>’
    | 
    | New submission
    +---

On r-hub.io, Windows Server 2022, R-devel, 64 bit platform, there was one additional note relating to MiKTeX:

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

* This is the 0.2.1 release of zfit

* This is the first version that has been released to CRAN

* All functions that are used in examples are now exported

* Package has been checked locally, on r-hub.io, and on winbuilder

* R CMD check ran without errors, warnings or notes, apart from "lastMiKTeXException" note (see above)
