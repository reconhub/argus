# Welcome the the *RECON* scripts for *Argus*

This project hosts the development of scripts used for generating analysis
reports for WHO's *Argus* system.

## R packages
R libraries are managed by `packrat` (https://rstudio.github.io/packrat/) *# JG TO DO: please remove several packages not needed for the project*  

## Other requirements

1. Selenium & Docker *# JG: as we don't use additional features of plotly, we should use ggplot 2 to produce the plots and export them in SVG, this would remove the need for Selenium and Docker*  
In order to render plotly to SVG the docker image with Chrome and Selenium is used.

To install docker please see: https://ropensci.github.io/RSelenium/articles/RSelenium-docker.html

If the docker is already installed pull the image `selenium/standalone-chrome`:
`docker pull selenium/standalone-chrome` from the dockerhub.

To run the docker with the project the project folder (left side) needs to be visible to the docker under the same path as locally:
`docker run -d -p 4445:4444 -v ~/[PROJECT_DIRECTORY]:/[PROJECT_DIRECTORY] selenium/standalone-chrome`

2. For Unix-alikes, GDAL (>= 2.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 4.8.0) are required *# JG: please detail why they are needed*.  

`sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable`

`sudo apt-get update`

`sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`

3. Pandoc (>= 1.16.0.2) (for `rmarkdown`) *# JG: is it installed by default with Rstudio or with some of the needed packages?, there was no need for additional installation on my computer*  

4. R (>= 3.4.1)

## Internationalization

It is possible to generate the reports in different languages. The internationalization is done by `shiny.i18n` package. User needs to provide the translations in the csv file:
`src/translations/translations.csv`. Raw column contains the alias for the translation for R. This shouldn't be modified in the csv (unless also modified in R). To add new translation add new column with the language and provide the translations.

## Workflow

Whole workflow is controlled using `src/master.R` file.
Master file contains comments for every block of logic.

**First please set required environment variables.**  
You can do this using export from your terminal (in unix-like systems; setx VARIABLE "Value" /M in windows admin command prompt)

ARGUS_DASHBOARD_LOCALE="en"` *# JG: can't we put it instead in the constant file*  

`ARGUS_DASHBOARD_LOCALE` - is the language of the reports. This needs to correspond to column names in the `src/translations/translations.csv`. Use "en" to translate to English.

**Run docker with Selenium and Chrome**
`docker run -d -p 4445:4444 -v ~/[PROJECT_DIRECTORY]:/[PROJECT_DIRECTORY] selenium/standalone-chrome`

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
