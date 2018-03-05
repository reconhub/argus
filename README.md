# Welcome the the *RECON* scripts for *Argus*

This project hosts the development of scripts used for generating analysis
reports for WHO's *Argus* system.

## R packages
R libraries are managed by `packrat`.

## Other requirements
1. Chrome browser for downloading the svg images for the report. In order to avoid loading `plotly` dependencies with the report the plots are rendered and saved using `Plotly.downloadImage()` via `htmlwidgets::onRender()` (see `?plotly::export` for more information). This requires a web browser, with browser's download setting location set to `[PROJECT_DIRECTORY]/src/assets` folder.

2. For Unix-alikes, GDAL (>= 2.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 4.8.0) are required.

`sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable`

`sudo apt-get update`

`sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`

3. Pandoc (>= 1.16.0.2) (for `rmarkdown`)

4. R (>= 3.4.1)

## Workflow

1. Generate dashboard raw input
Produce R object that are further prepocessed to produce the final output (charts and tables) for the reports.

Run `[PROJECT_DIRECTORY]/src/dashboard_input_script/argus_dashboard_raw_input_script.R` this will produce 2 RData raw inputs for administrative and epidemiological report in `[PROJECT_DIRECTORY]/src/assets`.

2. Preprocessing
In order to minimize the size of the report charts and tables are preprocessed outside the Rmd files.

Run `[PROJECT_DIRECTORY]/src/dashboard_input_scripts/epidemiological_dashboard_input` to generate input for epidemoiological report.

Run `[PROJECT_DIRECTORY]/src/dashboard_input_scripts/adimistrative_dashboard_input` to generate input for administrative report.

3. Rendering the dashboards
In order to render the dashboards please knit the `[PROJECT_DIRECTORY]/src/reports/administrative_dashboard.Rmd` and `[PROJECT_DIRECTORY]/src/reports/epidemiological_dashboard.Rmd`.

### Contributors

- [Thibaut Jombart](http://github.com/thibautjombart)
- [José Guerra](http://github.com/SNSteamLyon)
