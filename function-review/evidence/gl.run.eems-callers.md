# Downstream caller check

Searched local `/Users/mijangos/dartR.*` siblings and `/Users/mijangos/dartr2shiny` for `gl.run.eems` in R/Rmd/Qmd sources. No production callers were found in sibling packages.

The generated Shiny caller at `dartr2shiny/shiny_fun/Fun_gl.run.eems.R:225` supplies named arguments and consumes the returned plot list (`$mrates02` at line 241). It catches errors through `run_data`, saves its own displayed plot, and does not read `eems.RDS` or assume raw EEMS filenames. Its function signature remains compatible. Ploidy-1 input with the default diploid setting now receives the approved error. The generator retains a source snapshot under `input_generator/dartR.spatial/gl.run.eems.r`; refreshing that snapshot is separate generator maintenance.

No downstream repository was changed. No generated Shiny app was run.
