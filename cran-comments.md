## R CMD check results


0 errors | 0 warnings | 2-4 notes

The notes are the same as in version 1.1.0.
The two notes that always appear are
* (possibly) invalid URL - the URL is valid and an important link for the package.
* sub-directories of 1MB or more - the package includes a high-resolution map for plotting.

Additionally, there are 2 notes that occur inconsistently and seem to depend on environment settings. 
In my environment, they only occur when devtools::check() is run more than once and 
disappear when restarting RStudio. The notes are:

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
  
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

The files named in the notes do not exist.
Of the 4 notes, three were already present in the previous versions and are difficult to fix. They are

## revdepcheck results

There were no reverse dependencies.
