# Some notes for releasing to CRAN

* you can use usethis::use_version to control the package version.
* It is convenient to have a scripts folder in the SeaVal package that is published to github.
However, this spawns a NOTE in devtools::check, so for releasing a new version you should copy the SeaVal directory to some other place, remove unnecessary files (also .Rproj files etc.) and then build the tarball there.
* useful links:
** https://r-pkgs.org/release.html
** https://www.r-bloggers.com/2019/04/checking-reverse-dependencies-the-tiny-way/ (for checking all dependency packages)
