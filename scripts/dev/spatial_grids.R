# it is better if we have one function that sets the grid attribute and then we have a

#' Get the spatial coordinates grid from a data table.
#'
#' This function extracts the spatial coordinates grid from a given data table.
#' If the data table does not have this grid stored as attribute, the function creates it.
#' In this case, it identifies columns that represent spatial dimensions (e.g., longitude and latitude)
#' based on a list of recognized column names (see \code{\link{space_dimvars}}).
#' Depending on `only_cns`, the function either returns the column names of the coordinate columns
#' (in (`x,y`)-order, so most often `c('lon','lat')`)
#' or the full grid with all coordinates present in `dt` (but not filling in coordinates with missing values,
#' see \code{\link{expand_regular_grid}}).
#'
#' @param dt A data table.
#' @param only_cns A logical parameter indicating whether to return only the column names
#' (`TRUE`) or the spatial grid as data table (`FALSE`).
#' @param xycols An optional vector of column names representing the spatial coordinates.
#' If `NULL`, the function will attempt to identify the spatial dimensions automatically.
#'
#' @return Returns a vector of column names if `only_cns` is `TRUE`, or a data table with unique combinations of spatial coordinates if `only_cns` is `FALSE`.
#'
#' @details If no spatial coordinates are recognized, or if there is an issue with the number of recognized coordinates,
#' the function will throw an error with a message to guide the user.
#' The function expects the spatial coordinates to be named in a certain way (e.g., 'lon', 'lat' or similar),
#' and it uses the `space_dimvars()` function to identify these columns.
#'
#' @seealso \code{\link{space_dimvars}}
#'
#' @examples
#' # data table with spatial coordinates:
#' dt1 = data.table(lon = c(0,1),lat = c(0,0))
#' print(attr(dt1,'grid'))
#'
#' get_spatial_grid(dt1,only_cns = TRUE)
#'
#' print(attr(dt1,'grid'))
#'
#' get_spatial_grid(dt1,only_cns = FALSE)
#'
#' print(attr(dt1,'grid'))
#'
#'
#' @export

# grid attributes:
# named list:
# xy = character vector of length two with the coordinate names:
# x = unique x-values
# y = unique y-values
# regular = boolean
#
# for regular grids:
# dx,dy
# complete = boolean

#' Set Spatial Grid Attributes to a Data Table
#'
#' This function creates the spatial grid attribute for a data table.
#' If the data table already has such an attribute, missing information is filled in.
#' The grid attribute is a named list with (some of) the following pages:
#' \itemize{
#' \item[xy]: Character vector of length two specifying the names of the data-table-columns containing the spatial grids (in order x,y).
#' \item[x,y]: Numeric vectors of all unique x- and y-coordinates in increasing order (NAs not included).
#' \item[regular]: Logical. Is the grid *regular*?
#' \item[dx,dy]: Step sizes of the regular grid, only allowed if `regular = TRUE`. By convention we set `dx` to 9999 if only one x-coordinate is present, likewise for `dy`
#' \item[complete]: Logical. Is the regular grid *complete*?
#' }
#'
#' We call a grid *regular* if there is a coordinate `(x0,y0)` and positive values `dx`, `dy`,
#' such that each coordinate of the grid can be written as `(x0 + n*dx,y0 + m*dy)` for integers `n`,`m`.
#' Importantly, a regular grid does not need to be "a complete rectangle", we allow for missing coordinates, see details below.
#' We call it a *regular complete grid* if the grid contains these numbers for all integers `n`, `m` between some limits `n_min` and `n_max`,
#' respectively `m_min`, `m_max`.
#'
#' @param dt A data table object which the spatial grid will be set on.
#' @param coor_cns A character vector of length two indicating the names of the spatial coordinates
#'   within the data table, default is `NULL` which will cause the function to attempt to
#'   infer the spatial dimensions.
#' @param check_regular A logical indicating whether to check for regularity of the grid.
#'   Defaults to `TRUE`.
#'
#' @return Nothing, the attributes of dt are set in the parent environment
#'
#' @details For checking regularity: Doing this properly is a difficult problem, because we allow for missing coordinates
#' in the grid. We are (deliberately) a bit sloppy and only check whether the coordinates are part of a grid with
#' `dx` being the minimum `x`-difference between two coordinates, and similar `dy`. This may not detect regularity, when
#' we have data that is sparse on a regular grid, but for most technical purposes such data should also not be considered regular.
#' An example would be the three lon/lat coordinates `c(0,0)`, `c(2,0)`, `c(5,0)`. They clearly lie on the regular integer-lon/lat-
#' grid. However, the grid would show as not regular, because `dx` is not checked for smaller values than 2.
#' But it is actually reasonable to consider such data (which is mostly gaps) as irregular for most purposes.
#' The most important case of regular but not complete grids is gridded data that is restricted to a certain region, e.g. a country
#' or land. For such data the check works perfectly.
#'
#' Note that at the very bottom it is the definition of regularity itself that is a bit tricky: If we allow `dx`, `dy` to go all the way down to the machine-delta,
#' then pretty much any set of coordinates represented in a computer is part of a regular grid.
#' This hints that, testing and detecting regularity depends on how small you're willing to make your `dx`,`dy`.
#' An example in 1 dimension: consider the three 1-dimensional coordinates 0, 1, and m/n, with m and n integers
#' without common divisors and m>n. It is not difficult to see that these coordinates are part of a regular grid and that the
#' largest `dx` for detecting this is 1/n. So you can have very small coordinate sets that are in theory regular, but regularity
#' can be arbitrarily hard to detect.
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
                            check_regular = TRUE)
{
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
  if(!check_regular){
    setattr(dt,'grid',grid)
  } else {
      x = grid$x
      y = grid$y

      # this regularity check is not 100% foolproof:
      # It does not recognize the case when a coarse grid could be contained in a finer grid:
      # c(0,2,5) would be such a case, which would be nested in the regular grid 0:5

      #edge case: only 1 x coordinate. Convention: dx = 9999
      if(length(x) == 1) {
        dists_x = 9999
      } else {
      dists_x = x[2:length(x)] - x[1:(length(x)-1)]
      }
      x_expanded = seq(min(x),max(x),by = min(dists_x))

      if(length(y) == 1) {
        dists_y = 9999
      } else {
      dists_y = y[2:length(y)] - y[1:(length(y)-1)]
      }
      y_expanded = seq(min(y),max(y),by = min(dists_y))

      # regular?
      if(is.null(grid$regular)){grid$regular = all(x %in% x_expanded) && all(y %in% y_expanded)

      # dx,dy
      if(grid$regular & is.null(grid$dx)){
        grid$dx = min(dists_x)
      }
      if(grid$regular & is.null(grid$dy)){
        grid$dy = min(dists_y)
      }
    }
    # dx,dy
    if(grid$regular & is.null(grid$dx)){
      dists_x = grid$x[2:length(grid$x)] - grid$x[1:(length(grid$x)-1)]
      grid$dx = min(dists)
    }
    if(grid$regular & is.null(grid$dy)){
      dists_y = grid$y[2:length(grid$y)] - grid$y[1:(length(grid$y)-1)]
      grid$dy = min(dists)
    }
    # complete?
    if(grid$regular & is.null(grid$complete)){
    coords = unique(dt[,.SD,.SDcols = grid$coor_cns,])
    grid$complete = (coords[,.N] == length(grid$x)*length(grid$y))
    }
  }

  setattr(dt,name = 'grid',value = grid)
}



#' Expand Regular Spatial Grid
#'
#' First checks whether the spatial coordinates in a data table are part of a *regular grid*.
#' If they are, the function returns the smallest *regular complete grid* including all coordinates.
#' Stores the information in the `grid`-attribute of `dt`.
#'
#'
#' @param dt A data table object containing the spatial grid with coordinates.
#' @param return_expanded_grid Logical, if `TRUE` returns the expanded grid, otherwise returns a check of regularity.
#'
#' @return If `return_expanded_grid` is `FALSE`, returns a logical value indicating if the original grid is regular.
#'         If `TRUE` and the grid is regular, returns an expanded data table grid. If the grid is not regular,
#'         the function stops with an error message.
#'
#' @examples
#'   dt = data.table(lon = c(1, 2, 3), lat = c(1, 2, 3))
#'   expanded_grid = expand_regular_grid(dt)
#'   print(expanded_grid)
#'
#'   # To check if the grid is regular without expanding
#'   is_regular = expand_regular_grid(dt, return_expanded_grid = FALSE)
#'   print(is_regular)
#'
#' @export

expand_regular_grid = function(dt){

  set_spatial_grid(dt)
  grid = attr(dt,"grid")
  if(!grid$regular) stop("The grid needs to be regular.")


  dists_x = xvals[2:length(xvals)] - xvals[1:(length(xvals)-1)]
  xvals_expanded = seq(min(xvals),max(xvals),by = min(dists_x))

  dists_y = yvals[2:length(yvals)] - yvals[1:(length(yvals)-1)]
  yvals_expanded = seq(min(yvals),max(yvals),by = min(dists_y))

  regularity_check = all(xvals %in% xvals_expanded) && all(yvals %in% yvals_expanded)

  # update grid attribute
  grid_temp = attr(dt,'grid')
  grid_temp$regular = regularity_check
  setattr(dt,'grid',grid_temp)

  if(!return_expanded_grid) {
    return(regularity_check)
  } else {
    if(!regularity_check) stop("The spatial grid does not seem to be regular, see check_grid_regular() for details.")
    grid_dt = data.table(expand.grid( lats_expanded, lons_expanded))
    setnames(grid_dt,xycols)
    # store grid properties as attribute for use by other functions:
    grid_properties = list(regular = TRUE,
                           full = TRUE,
                           x = list(name = xycols[1],
                                    vals = lons_expanded,
                                    d = min(dists_lon)),
                           y = list(name = xycols[2],
                                    vals = lats_expanded,
                                    d = min(dists_lat)))
    setattr(grid_dt, 'grid', grid_properties)

    return(grid_dt)
  }
}

