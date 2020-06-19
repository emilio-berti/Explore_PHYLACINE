# Author

Emilio Berti

# Installation requirements

You need the following R packages:

  1. shiny
  2. raster
  3. tmap
  4. sf
  5. DT

  * You also need to download the PHYLACINE database in the parent folder.
  * Be sure the path to the ranges is: 'PHYLACINE_1.2/PHYLACINE_1.2-master/Data/Ranges' or change the file-path in *app.R*.
  * Traits table is read from the online GitHub repo, so no need to check that.

# Usage

Just open an R terminal in the parent folder (where *app.R* is located) and run: `shiny::runApp("app.R")`.
