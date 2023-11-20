

#' Check if all elements of x are within tolerance eps of any element in y
#'
#' Auxiliary function, used for checking whether spatial grids are regular, with allowing for rounding errors.
#'
#' @param x A numeric vector, sorted in increasing order.
#' @param y A numeric vector, sorted in increasing order.
#' @param eps The tolerance within which we consider two values to be equal.
#' @return A boolean value, TRUE if all x are in y within tolerance eps, FALSE otherwise.

are_all_elements_within_eps = function(x, y, eps) {
  # Find the index of the intervals in y that are just less than elements of x
  idx = findInterval(x, y)

  # Check the boundary conditions
  too_low = idx == 0
  too_high = (idx == length(y))

  # If all x are outside the bounds of y, then they can't all be within tolerance
  if (all(too_low | too_high)) {
    return(FALSE)
  }

  # Calculate the distance from x to the y just below and just above
  lower_diff = abs(x - y[idx])
  upper_diff = abs(x - y[pmin(idx + 1, length(y))])

  # The element is within eps if either the lower_diff or upper_diff is within eps
  within_tolerance = (lower_diff <= eps) | (upper_diff <= eps)

  # Return TRUE if all elements are within tolerance, FALSE otherwise
  all(within_tolerance)
}

#' Set Spatial Grid Attributes to a Data Table
#'
#' This function creates the spatial grid attribute for a data table.
#' If the data table already has such an attribute, missing information is filled in.
#' In particular, the function checks whether a grid is regular, allowing for rounding errors in the grid coordinates, see details below.
#' By default the grid coordinates are rounded to a regular grid if they are very close to being regular.
#' While this sounds dangerous, it is almost always desirable to treat coordinates like that when working with data tables.
#'
#' The grid attribute is a named list with (some of) the following pages:
#' \itemize{
#' \item[coor_cns]: Character vector of length two specifying the names of the data-table-columns containing the spatial grids (in order x,y).
#' \item[x,y]: Numeric vectors of all unique x- and y-coordinates in increasing order (NAs not included).
#' \item[regular]: Logical. Is the grid *regular*? See details below.
#' \item[dx,dy]: Step sizes of the regular grid (only contained if `regular = TRUE`). By convention we set `dx` to 9999 if only one x-coordinate is present, likewise for `dy`.
#' \item[complete]: Logical. Is the regular grid *complete*? See details below.
#' }
#'
#'
#' @param dt A data table object.
#' @param coor_cns Optional character vector of length two indicating the names of the spatial coordinates
#'   within the data table in order `x`,`y`. Default (`NULL`) makes the function guess based on column names.
#' @param check_regular A logical indicating whether to check for regularity of the grid. This should essentially always be done but can be suppressed for speed.
#'   Defaults to `TRUE`.
#' @param regular_tolerance Value >= 0 specifying the amount of rounding error we allow for still recognizing a grid as regular.
#' Given in percent of the minimum of `dx` and `dy`. Default is 1. Based on this value coordinates are rounded to the smallest after-comma-digit making them regular,
#' as long as this rounding introduces less error than `min(dx,dy)*regular_tolerance/100`.
#' Set this to `NULL` if you are absolutely certain that you don't want to round/change the grid. Doing this or decreasing this below 1 is not recommended, see details below.
#' @param verbose Logical. If `TRUE`, the grid information is printed out (by a call to \code{\link{grid_info}}).
#'
#' @return Nothing, the attributes of dt are set in the parent environment. Moreover, the grid coordinates may be rounded If `regular`
#'
#' @details We call a grid *regular* if there is a coordinate `(x0,y0)` and positive values `dx`, `dy`,
#' such that each coordinate of the grid can be written as `(x0 + n*dx,y0 + m*dy)` for integers `n`,`m`.
#' Importantly, a regular grid does not need to be "a complete rectangle", we allow for missing coordinates, see details below.
#' We call it a *regular complete grid* if the grid contains these numbers for all integers `n`, `m` between some limits `n_min` and `n_max`,
#' respectively `m_min`, `m_max`.
#'
#' Checking regularity properly is a difficult problem, because we allow for missing coordinates
#' in the grid and allow for rounding errors.
#' For the treatment of rounding errors it is not recommended to set `regular_tolerance` to `NULL` or a very small value
#' (e.g. 0.1 or smaller). In this case, grids that are regular in praxis are frequently not recognized as regular:
#' Take for example the three x-coordinates 1, 1.5001, 2.4999. They are supposed to be rounded to 1 digit after
#' the comma and then the grid is regular with `dx = 0.5`. However, if `regular_tolerance` is NULL, the grid will be marked as irregular.
#' Similarly, if `regular_tolerance` is too small, the function is not allowed to make rounding errors of 0.0001
#' and the grid will also not be recognized as regular.
#'
#' When it comes to the issue of missing values in the grid, we are (deliberately) a bit sloppy and only check whether
#' the coordinates are part of a grid with `dx` being the minimum `x`-difference between two coordinates,
#' and similar `dy`. This may not detect regularity, when we have data that is sparse on a regular grid.
#' An example would be the three lon/lat coordinates `c(0,0)`, `c(2,0)`, `c(5,0)`. They clearly lie on the regular integer-lon/lat-
#' grid. However, the grid would show as not regular, because `dx` is not checked for smaller values than 2.
#' This choice is on purpose, since for most applications grids with many (or mostly) holes should be treated as irregular (e.g. plotting, upscaling, etc.).
#' The most important case of regular but not complete grids is gridded data that is restricted to a certain region, e.g. a country
#' or restricted to land. This is what we think of when we think of a regular incomplete grid, and for such data the check works perfectly.
#'
#' Note that at the very bottom it is the definition of regularity itself that is a bit tricky:
#' If we allow `dx`, `dy` to go all the way down to the machine-delta,
#' then pretty much any set of coordinates represented in a computer is part of a regular grid.
#' This hints at testing and detecting regularity actually depending on how small you're willing to make your `dx`,`dy`.
#' An example in 1 dimension: consider the three 1-dimensional coordinates `0`, `1`, and `m/n`, with `m` and `n` integers
#' without common divisors and `m>n`. It is not difficult to see that these coordinates are part of a regular grid and that the
#' largest `dx` for detecting this is 1/n. This shows that you can have very small coordinate sets that are in theory regular, but their regularity
#' can be arbitrarily hard to detect. An example of a grid that is truely not regular are the three `x`-coordinates 0,1,a with a irrational.
#'
#'
#' @examples
#' dt = data.table(lon = 1:4, lat = rep(1:2,each = 2), some_data = runif(4))
#' print(dt)
#' attr(dt,'grid')
#'
#' set_spatial_grid(dt)
#' attr(dt,'grid')
#'
#' @export

set_spatial_grid = function(dt,
                            coor_cns = NULL,
                            check_regular = TRUE,
                            regular_tolerance = 1,
                            verbose = FALSE)
{
  coor_cnscols = NULL

  # get grid if already present (else get NULL):
  grid = attr(dt,'grid')

  #### coor_cns ####
  # first case:
  if(!is.null(grid$coor_cns) & !is.null(coor_cns)) {
    if(!interactive()) {
      warning(paste0('coor_cns is changed from ',paste(grid$coor_cns,collapse = ', '),' to ',paste(coor_cns,collapse = ', ')))
      grid$coor_cns = coor_cns
    } else if(interactive()) {
        check = menu(title = strwrap(paste0("Current coor_cns-attribute is ",paste(grid$coor_cns,collapse = ', '),". Do you want to change to ",paste(coor_cns,collapse = ", "),"?")),
             choices = c("yes","no"))
        if(check == 1) grid$coor_cns = coor_cns
    }
  }
  # second case:
  if(is.null(grid$coor_cns) & !is.null(coor_cns)){
    # correct ordering if necessary:
    if(identical(tolower(coor_cns), c('lat','lon'))) coor_cns = rev(coor_cns)
    grid$coor_cns = coor_cns
  }
  # third case:
  if(is.null(grid$coor_cns) & is.null(coor_cns)){
    # guess based on space_dimvars:
    coor_cns = space_dimvars(dt)
    if(length(coor_cns) == 0) stop(strwrap("No spatial coordinates recognized.\n
                            Check the column names of the data table, I expected it to contain columns 'lon', 'lat' or similar. \n
                            Run space_dimvars() to see which column names are currently recognized."))
    if(length(coor_cns) == 1) stop(strwrap(paste0("I recognized only one spatial coordinate: ",coor_cns,".\n
                                        Check the column names of the data table, I expected it to contain columns 'lon', 'lat' or similar. \n
                                        Run space_dimvars() to see which column names are currently recognized.")))

    if(length(coor_cns) >2 ) {
      if(all(c('lon','lat') %in% coor_cns)){
        warning(strwrap(paste0("I recognized more than two spatial coordinates (",paste(coor_cns,collapse = ', '),").\n
                              Setting lon/lat as spatial grid.")))
        coor_cns = c('lon','lat')
      } else {
        stop(strwrap(paste0("I recognized more than two spatial coordinates (",paste(coor_cnscols,collapse = ', '),"),\n
                        and wasn't able to determine the columns containing the spatial grid.")))
      }
    }
    # follow (x,y)-format:
    if(identical(tolower(coor_cns), c('lat','lon'))) coor_cns = rev(coor_cns)
    grid$coor_cns = coor_cns

  }

  #### x,y ####
  if(is.null(grid$x)) {
    grid$x = sort(unique(dt[,get(grid$coor_cns[1])]))
    grid$x = grid$x[!is.na(grid$x)]
  }
  if(is.null(grid$y)) {
    grid$y = na.omit(sort(unique(dt[,get(grid$coor_cns[2])])))
    grid$y = grid$y[!is.na(grid$y)]
  }

  ### regular, and the rest ###
  if(check_regular & is.null(grid$regular)){
      x = grid$x
      y = grid$y

      # get distances between neighbouring x and y coordinates:
      #edge case: only 1 x coordinate. Convention: dx = 9999
      if(length(x) == 1) {
        dists_x = 9999
      } else {
      dists_x = x[2:length(x)] - x[1:(length(x)-1)]
      }

      if(length(y) == 1) {
        dists_y = 9999
      } else {
      dists_y = y[2:length(y)] - y[1:(length(y)-1)]
      }

      # rounding digits if regular_tolerance != NULL:
      if(!is.null(regular_tolerance))
      {
        # threshold for rounding:
        thresh = min(c(dists_x,dists_y)) * regular_tolerance/100
        # highest digit that we check for rounding, based on thresh:
        max_rounding_digit = round(1 - log(thresh/5,base = 10)) # some math done on paper, not complicated

        # one challenge is that, in the presence of rounding errors, there is usually a 'correct'
        # number of digits to round to, and rounding to a later digit results in non-regular grids:
        # e.g. the coordinates 1.000, 1.499, 2.501 are supposed to be rounded to 1 digit after comma.
        # If your tolerance allows you to round to 3 digits after comma, regularity is no longer recognized.

        rounding_done = FALSE
        rounding_digit = 0
        while(! rounding_done)
        {
          x_rounded = round(x,rounding_digit)
          y_rounded = round(y,rounding_digit)
          max_rounding_error = max(c(abs(x - x_rounded),abs(y - y_rounded)))
          if(max_rounding_error < thresh) {
            rounding_done = TRUE
          } else if(rounding_digit == max_rounding_digit){
            rounding_done = TRUE
          } else{
            rounding_digit = rounding_digit + 1
          }
        }

        if(sum(abs(x-x_rounded)) + sum(abs(y-y_rounded)) >0){ #don't check 'identical' here, because integer-grids become numeric by rounding.
          warning(paste0('Grid coordinates have been rounded to ',rounding_digit,' digits'))
          dt[,(coor_cns[1]):=round(get(coor_cns[1]),rounding_digit)]
          dt[,(coor_cns[2]):=round(get(coor_cns[2]),rounding_digit)]

          # adjust grid-values:
          x = x_rounded
          y = y_rounded
          grid$x = x
          grid$y = y

          # redo grid distance calculations above:
          if(length(x) == 1) {
            dists_x = 9999
          } else {
            dists_x = x[2:length(x)] - x[1:(length(x)-1)]
            dists_x = round(dists_x,rounding_digit) # one might think that wouldn't be necessary, but one would be wrong...
          }

          if(length(y) == 1) {
            dists_y = 9999
          } else {
            dists_y = y[2:length(y)] - y[1:(length(y)-1)]
            dists_y = round(dists_y,rounding_digit) # one might think that wouldn't be necessary, but one would be wrong...
          }
        }
      }

      x_expanded = seq(min(x),max(x),by = min(dists_x))
      y_expanded = seq(min(y),max(y),by = min(dists_y))

      # regular? Annoyingly, you cannot use %in% here because R produces rounding errors order of magnitude 1e-11
      # grid$regular = all(x %in% x_expanded) && all(y %in% y_expanded)
      grid$regular = (are_all_elements_within_eps(x,x_expanded,eps = 1e-10) && are_all_elements_within_eps(x,x_expanded,eps = 1e-10))


      # dx,dy
      if(grid$regular){
        if(is.null(grid$dx)){
          dists_x = grid$x[2:length(grid$x)] - grid$x[1:(length(grid$x)-1)]
          if(!is.null(regular_tolerance)) dists_x = round(dists_x, rounding_digit) # you'd think that's not necessary anymore, but you'd be wrong...
          grid$dx = min(dists_x)

        }
        if(is.null(grid$dy)){
          dists_y = grid$y[2:length(grid$y)] - grid$y[1:(length(grid$y)-1)]
          if(!is.null(regular_tolerance)) dists_y = round(dists_y, rounding_digit)
          grid$dy = min(dists_y)
        }
      }

      # complete?
      if(grid$regular & is.null(grid$complete)){
        coords = unique(dt[,.SD,.SDcols = grid$coor_cns,])
        grid$complete = (coords[,.N] == length(grid$x)*length(grid$y))
      }

    }

  setattr(dt,name = 'grid',value = grid)
  if(verbose) grid_info(dt)
}


#' Retrieve spatial grid information from a data table
#'
#' This function prints out spatial grid information from a data table. If the grid-attribute does not exist
#' \code{\link{set_spatial_grid}} is called first.
#'
#' @param dt A data table
#' @return This function does not return a value; instead, it prints a message to the console
#'         with the grid information.
#' @examples
#'
#'  dt = data.table(lon = runif(10), lat = runif(10))
#'  grid_info(dt)
#' @export

grid_info = function(dt){
  if(is.null(attr(dt,'grid'))) set_spatial_grid(dt)

  grid = attr(dt,'grid')

  if(is.null(grid$regular)){
    regularity_string = 'Regularity of the grid has not been checked.'
  }else{
    regularity_string = paste0('The grid is ',ifelse(grid$regular,yes = '',no = 'not '),'regular',ifelse(grid$complete,yes = ' and complete',no = ' (but not complete)'))
  }

  message_string = paste(c('Spatial grid information:',
                          paste0('grid columns: ',paste(grid$coor_cns,collapse = ', ')),
                          paste0('x-coordinate: min = ',min(grid$x),', max = ',max(grid$x),', number of unique values = ',length(grid$x)),
                          paste0('y-coordinate: min = ',min(grid$y),', max = ',max(grid$y),', number of unique values = ',length(grid$y)),
                          regularity_string,
                          ifelse(grid$regular,yes = paste0('dx = ',grid$dx,', dy = ',grid$dy),no = '')),
                         collapse = '\n')

  message(message_string)
}


#' Expand Regular Spatial Grid
#'
#' First checks whether the spatial coordinates in a data table are part of a *regular grid*.
#' If they are, the function returns the smallest *regular complete grid* including all coordinates.
#' See \code{\link{set_spatial_grid}} for more information.
#'
#' @param dt A data table object containing the spatial grid with coordinates.
#'
#' @return A data table with the completed spatial grid. Has the grid-attribute.
#'
#' @examples
#'   dt = data.table(lon = c(1, 2, 3), lat = c(1, 2, 3))
#'   completed_grid = complete_regular_grid(dt)
#'   print(completed_grid)
#'
#'
#' @export

complete_regular_grid = function(dt){

  set_spatial_grid(dt)
  grid = attr(dt,"grid")
  if(!grid$regular) stop("The grid needs to be regular.")

  all_x = seq(min(grid$x),max(grid$x),by = grid$dx)
  all_y = seq(min(grid$y),max(grid$y),by = grid$dy)

  complete_grid = data.table(expand.grid(all_x,all_y))
  setnames(complete_grid,grid$coor_cns)

  # set grid-attribute for the complete grid:
  grid$complete = TRUE
  setattr(complete_grid,'grid',grid)

  return(complete_grid)
}

