
## R CMD check results

There were no ERRORs or WARNINGs.


NOTE 1: On r-hub.io, non-windows platforms, there was one note:

+---------
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
+---------

This seems to be a system configuration relating to the unix tidy program (not any tidyverse packages) and I believe it can be safely ignored.


NOTE 2: On r-hub.io, Windows Server 2022, R-devel, 64 bit, there was one note relating to MiKTeX:

+---------
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
+---------

This note is not reproducible locally or on other platforms, and seems not to affect the output, because the check reports no errors related to the PDF version of the manual:

+---------
* checking PDF version of manual ... [12s] OK
+---------



## Downstream dependencies

There are currently no downstream dependencies for this package.



## Release summary

* This is the 0.2.3 release of zmisc

* The release includes a new function and adds a default parameter to another

* Package has been checked locally, on r-hub.io, and on winbuilder

* R CMD check ran without errors, warnings or notes, apart from the notes described above
