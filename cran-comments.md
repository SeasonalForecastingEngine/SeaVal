## R CMD check results


0 errors | 0 warnings | 4 notes

Of the 4 notes, three were already present in the previous versions and are difficult to fix. They are

* Found the following (possibly) invalid URLs:
  URL: https://library.wmo.int/idurl/4/56227
    From: DESCRIPTION
    Status: 404
    Message: Not Found
    
(The url does exist and this is an important source for the package)


* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
  
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

(Both of these occur inconsistently and seem to depend on environment settings. 
In my environment, they only occur when devtools::check() is run more than once and 
disappear when restarting RStudio. The files named in the notes do not exist.)

The new note is:

* checking installed package size ... NOTE
  installed size is 19.1Mb
  sub-directories of 1Mb or more:
    extdata  18.0Mb

The package now contains a high-resolution spatial map of the Greater Horn of Africa.
Including this map is a feature request by several package-users.

## revdepcheck results

There were no reverse dependencies.
