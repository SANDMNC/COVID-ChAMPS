# COVID-ChAMPS
# COVID Child and Adolescent Mental health and Parenting Survey

Initial data recorded using Qualtrics.
Currently stored on secure server through unimelb.
To create this data file, download from qualtrics, delete rows 2 and 3 (additional headers), and delete the 'email' row to maintain confidentiality.

# covid_scoring.R: 
Data cleaning, processing and Questionnaire scoring for COVID-ChAMPS project
# covid_missing.R: 
Included some visualisations of missing data and runs the multiple imputation. Also creates the files child file that 'listwise deletion' and robust lme models are run on.
# analyses.R: 
Data analysis code
# covid_tables.Rmd: 
Uses papaja package to create reproducible APA style tables
# Plots.R and Analysis_plots.RMD/html: 
Code and plots illustrating significant associations from analyses.R
