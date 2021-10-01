# CEMRA (COVID-19 Exposure Model and Risk App)

## Introduction
CEMRA originated from the following Quantitative Microbial Risk Assessment model by Jones, 2020 - https://www.tandfonline.com/doi/full/10.1080/15459624.2020.1784427

CEMRA is a web application developed using the original model, which allows the user to modify the virus load, emissions, controls, patients behaviour and susceptible behaviour.

The general steps for use of CEMRA are as follows:
1. Choose which scenario fits the location that you are interested in (or upload your own)
2. Choose which parameters you would like to modify 
3. Run the model 
4. Compare the model to the baseline scenario
5. Interpret results

## To run the app

golem::run_dev()

## To run the package functions

CEMRA::run_model("Test", 10)

## To add a package
1. Run print(sessionInfo()) to get package number
2. Add that to the DESCRIPTION file

## To change an input parameter
1. Go to the covidinfectioncalculator.R and modify the function arguments and the function code
2. Load the "/runs" file and modify 
3. Load the metadata.csv and modify and save as an RData file


## Issues to fix
Q1. Currently 1000 runs takes 94 seconds which is too slow for the app
A1. Currently 1000 runs takes 33 seconds which is too slow for the app (update)

Q3. Need to have file of parameters from comparison model saved to output
Q4. Need to get IOM image working
Q5. Need to re-write information html
Q6. Re-do scenario files so that they are more specific
Q7. Always keep the same colours for the waffle plot
Q8. Add in starting concentrations in air, surfaces etc
Q9. Inormation - how to use tab - describe scenario files
Q10. Put in download button to get parameters
Q11. route to 100 algorithm - fix
