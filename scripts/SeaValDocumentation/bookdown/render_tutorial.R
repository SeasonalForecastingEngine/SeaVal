### This script renders the SeaVal documentation

# It essentially knits all .rmd files in this folder, starting with index.rmd and then following the naming order (so the file names should start with a number)
setwd('~/pkg/SeaVal/scripts/SeaValDocumentation/bookdown/') # adjust to your local repository!
bookdown::render_book('index.rmd',
                      'bookdown::gitbook',
                      new_session = T)


# We need to copy paste, the output_dir option of render_book does not work, see https://github.com/rstudio/bookdown/issues/804
output_dir = '/nr/common/www/virtual/files.nr.no/htdocs/samba/CONFER/SeaVal/'# this directory is shared at http://files.nr.no/samba/CONFER/SeaVal/




file.copy(list.files('./_book/',full.names = TRUE), to = output_dir, recursive = TRUE)

