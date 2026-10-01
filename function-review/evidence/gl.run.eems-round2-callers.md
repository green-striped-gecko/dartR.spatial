# Round 2 caller check (API3), 2026-10-01

Searched `~/dartR.base`, `dartR.popgen`, `dartR.captive`, `dartR.sexlinked`, `dartR.sim`, `dartR.data`, `~/dartr2shiny` and `~/dartr_shiny` for `gl.run.eems`.

- **Sibling packages:** no callers.
- **Shiny signature use:** `dartr2shiny/shiny_fun/Fun_gl.run.eems.R:242` (generated into `dartr_shiny/src/app/view/Fun_gl.run.eems.R`) calls with named arguments only. The new `habitat` argument sits last, so the call is unaffected. The app passes `buffer` (default 1000), which now means 1 km on the ground.
- **Shiny GeoTIFF export, affected by change 12:** the module builds a raster from `mrates02$data` and hard-codes a Mercator CRS (`dartr2shiny/config/slot_exceptions.csv:326`; generated `dartr_shiny/src/app/view/Fun_gl.run.eems.R:257`). After change 12 the raster values are longitude/latitude, so the downloaded TIF would be georeferenced as Mercator metres and land near 0,0. Required follow-up in dartr2shiny: set `crs(MyData2) <- "EPSG:4326"` and regenerate. `coord_equal()` added by the app (line 259) still draws, but stretches east-west in degrees.
- **dismo:** 167 generated modules list `dismo[Mercator]` in `box::use()`. The platform image installs CRAN `dartR` and `dartRverse`, whose CRAN dartR.spatial still imports dismo, so dismo stays installed for now. Once a CRAN dartR.spatial without dismo is released, the image needs dismo installed explicitly or the `box::use()` entry removed.

No downstream repository was changed.
