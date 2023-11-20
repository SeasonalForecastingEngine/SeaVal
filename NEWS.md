# SeaVal 1.1.0

* Added a `NEWS.md` file to track changes to the package.

* A high resolution map of the Greater Horn of Africa is now included as part of the package.
This is the standard map used during GHACOFs at ICPAC. This map is used by the new plotting functions
`ggplot_dt_gha_map` (which else pretty much works as `ggplot_dt`) and `tercile_plot`.

* A new function `set_spatial_grid` has been added. The function allows to store a grid-attribute with a data table.
This attribute contains information about the spatial grid of a data table, e.g. whether the grid is regular or complete. 
The new function `grid_info` prints this information out for a data table.

* `upscale_regular_lon_lat` now allows to specify a required fraction of coverage and excludes all coarse grid cells 
that have less than this required coverage.

* Several small bugs have been fixed.
