# Welcome the the *RECON* scripts for *Argus*

This project hosts the development of scripts used for generating analysis
reports for WHO's *Argus* system.

## R packages
R libraries are managed by `packrat` (https://rstudio.github.io/packrat/)

## Other requirements

1. For Unix-alikes, GDAL (>= 2.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 4.8.0) are required for `sf` package.

`sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable`

`sudo apt-get update`

`sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`

2. For Unix-alikes, Pandoc (>= 1.16.0.2) (for `rmarkdown`)

3. R (>= 3.4.1)

## Internationalization

It is possible to generate the reports in different languages. The internationalization is done by `shiny.i18n` package. User needs to provide the translations in the csv file:
`src/translations/translations.csv`. Raw column contains the alias for the translation for R. This shouldn't be modified in the csv (unless also modified in R). To add new translation add new column with the language and provide the translations.

## Database
In order to fetch data for `argus_dashboard_raw_input_script.R` add password to `db/db_config` file.

## Workflow

Whole workflow is controlled using `src/master.R` file.
Master file contains comments for every block of logic.

**First please set required constant variables.**
In `src/constants.r`please set api connection constants, translation constant and country constant.

**To run the workflow:**
`Rscript src/master.R`

### Master script steps:

1. Generating dashboard raw input
Produce R object that are further prepocessed to produce the final output (charts and tables) for the reports.

Run `[PROJECT_DIRECTORY]/src/dashboard_input_script/argus_dashboard_raw_input_script.R` this will produce 2 RData raw inputs for administrative and epidemiological report in `[PROJECT_DIRECTORY]/src/assets`.

2. Preprocessing
In order to minimize the size of the report charts and tables are preprocessed outside the Rmd files.

Run `[PROJECT_DIRECTORY]/src/dashboard_input_scripts/epidemiological_dashboard_input` to generate input for epidemiological report.

Run `[PROJECT_DIRECTORY]/src/dashboard_input_scripts/adimistrative_dashboard_input` to generate input for administrative report.

3. Rendering the dashboards
In order to render the dashboards knit the `[PROJECT_DIRECTORY]/src/reports/administrative_dashboard.Rmd` and `[PROJECT_DIRECTORY]/src/reports/epidemiological_dashboard.Rmd` files.

## Contributors

- [Olga Mierzwa-Sulima](https://github.com/olgamie)
- [José Guerra](http://github.com/SNSteamLyon)
