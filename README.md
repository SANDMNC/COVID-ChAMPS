# COVID-ChAMPS
# COVID Child and Adolescent Mental health and Parenting Survey

Initial data recorded using Qualtrics.
Currently stored on secure server through unimelb.
To create this data file, download from qualtrics, delete rows 2 and 3 (additional headers), and delete the 'email' row to maintain confidentiality.

# COVID-ChAMPS.Rproj:
R project for this repository
# covid_scoring.R: 
Data cleaning and questionnaire scoring for COVID-ChAMPS project
# covid_desc_figures.Rmd/html:
Contains means,sds, histograms, piecharts of the scored data. 
# covid_missing.R: 
Runs the multiple imputation. Also creates the child file that 'listwise deletion' and robust lme models are run on.
# analyses.R: 
Data analysis code
# Analysis_plots.Rmd/html: 
Code and plots illustrating significant associations from analyses.R
# Folders
## raw_data
Contains raw data file (ignored on public github repository)
## scored_data
Contains scored data files (ignored on public github repository)
## figures
Contains figures created in Analysis_plots.Rmd and covid_desc_figures
## reproducible_tables folder:
Contains covid_tables.Rmd and associated files. Uses papaja package to create reproducible APA style tables.

