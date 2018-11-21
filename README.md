# R scripts for admnistrative and epidemiological dashboards in Argus


## Argus IT-tool for public health surveillance
The World Health Organization (WHO) has developed Argus, an open source IT tool to support public health surveillance for early detection and response. It uses Short Message Service (SMS) technology for the transmission of information between the local healthcare facilities and all levels of the public health surveillance system via a mobile application. A web platform complements the application for data management and analysis.

## R and Argus
R has been integrated in the server to enhance Argus data management and data analysis capacities.
**This repository aims to produce in a regular manner an administrative and an epidemiological dasboard displayed in the Argus web platform.**

---
## First time installation
### Required software and packages
1. R (>=3.4.1)
2. R tools
3. Pandoc (>= 1.16.0.2) (for `rmarkdown`)
4. For Unix-alikes, GDAL (>= 2.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 4.8.0) are required for `sf` package.
`sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable`
`sudo apt-get update`
`sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`

Needed R packages are available as zip binaries for windows in the `PROJECT_DIRECTORY\argus\packages` folder.
All packages can be installed using the below R script.
`install.packages(list.files("PROJECT_DIRECTORY/packages/", full.names = T), repos=NULL, type="win.binary")`
Should the packages be updated, or new packages required, an R script is available to facilitate the process `PROJECT_DIRECTORY\packages_to_install.R`

### Internationalization

It is possible to generate the reports in different languages. The internationalization is done by `shiny.i18n` package. User needs to provide the translations in the csv file: `PROJECT_DIRECTORY\dashboards\translations\translations.csv`. Raw column contains the alias for the translation for R. This shouldn't be modified in the csv (unless also modified in R). To add new translation add new column with the language and provide the translations.

### Integration of R and Argus
1. Create the appropriate folders in the server
    - Open `C:\xampp\htdocs` and create a folder named `ScriptsR`
    - Open `C:\xampp\htdocs\ScriptsR` and create a folder named `argus` to have the hierarchical folder `C:\xampp\htdocs\ScriptsR\argus`
2. Download all this repository: https://github.com/reconhub/argus
3. Copy its content to the `C:\xampp\htdocs\ScriptsR\argus` folder.

### Set required constants
1. Update the information to access the local database
    - Update and save the file `PROJECT_DIRECTORY\db\db_config.yaml` with correct database name, host, port number, user and password.
2. Update local constants
    - Update and save the file `PROJECT_DIRECTORY\dashboards\config.yaml` with the language, country name, first day of the week and update frequency in minutes.
        - Language: e.g. "fr" for French; "en" for English. Use the same abbreviation as in the first row of the file: `PROJECT_DIRECTORY\dashboards\translations\translations.csv`. 
        - Country name (used to produce the maps): e.g. "Togo". Use names displayed in the file `PROJECT_DIRECTORY\dashboards\countryNames.csv`.
        - First day of the week: either 1 if Monday, or 7 if Sunday.
        - Frequency of the dashboards update: e.g. 15. Don't put less than 15 minutes.
3. Check the working directory path at the top of `PROJECT_DIRECTORY\dashboards\master.R`, update it if needed

### Create a windows scheduled task (or CRON job) to produce the dashboards
For windows:
- run the `dashboards\master.r` once
- you can then use the following command in R to create the scheduled task:
`taskscheduleR::taskscheduler_create(taskname="ArgusR", rscript=paste0(getwd(),"/dashboards/master.R"), schedule="MINUTE", startdate = format(today(), "%d/%m/%Y"), starttime=format(Sys.time() + 120, "%H:%M"),
modifier=config$freqCron)`

- If an error message such as: `ERROR: Incorrect Start Date` appears:
    - Run the following command in the R console:
`taskscheduleR::taskscheduler_create(taskname="ArgusR", rscript=paste0(getwd(),"/dashboards/master.R"),
schedule="ONCE", startdate = format(today(), "%d/%m/%Y"), starttime=format(Sys.time() + 120, "%H:%M"))`
    - Then open the application `Task scheduler` in Windows:
        - Go in the left panel to `Task scheduler library`
        - Select the task `ArgusR` in the middle panel.
        - Click on properties in the right panel.
        - Go to the `Triggers` tab and select in `Advanced settings`: "Repeat task every `15 minutes` for a duration of `Indefinitely`"
        - Press OK
        - Press “Run” in the right panel.
---
## Workflow

Whole workflow is controlled through the `PROJECT_DIRECTORY\dashboards\master.R` script.

### Master script steps:

1. Generate data objects for the dashboards: run `[PROJECT_DIRECTORY]/dashboards/scripts/argus_dashboard_raw_input_script.R`

2. Generate the charts and tables of the dashboards:
    - Run `[PROJECT_DIRECTORY]/dashboards/scripts/administrative_dashboard_input` for administrative dashboard.
    - Run `[PROJECT_DIRECTORY]/dashboards/scripts/epidemiological_dashboard_input` for epidemiological dashboard. 
 
3. Produce the HTML dashboards: knit the `[PROJECT_DIRECTORY]/dashboards/reports/administrative_dashboard.Rmd` and `[PROJECT_DIRECTORY]/dashboards/reports/epidemiological_dashboard.Rmd` files.

---
## Contributors

- [José Guerra](http://github.com/SNSteamLyon)
- [Olga Mierzwa-Sulima](https://github.com/olgamie)