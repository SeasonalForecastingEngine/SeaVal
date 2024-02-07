# SeaVal 1.1.1

* Fixed an issue where `add_country()`, `restrict_to_country()` and `restrict_to_GHA()` did not recognize Somaliland as part of Somalia.

* Fixed an issue where requiring minimal coverage in `upscale_regular_lon_lat()` did not work correctly,
if the data table contained grid cells with missing values

* Changed the default behavior of CHIRPS upscaling to only include gridcells with at least 50% coverage. Fixed a warning that was always printed at the end of 
`download_chirps_monthly()`

* Renamed `ggplot_dt_gha_map()` and `ggplot_dt_shf()` into `gha_plot()`.

* Fixed a bug where `ggplot_dt()` and `gha_plot()` crashed when the provided data table has a column named `"country"` as its first non-dimvar column.

* Added package-level documentation.

* Deprecated the old functions `ggplot_dt_gha_map()`, `ggplot_dt_shf()`, and `restrict_to_confer_region()`.


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
