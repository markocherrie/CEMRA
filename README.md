# CEMRA (COVID-19 Exposure Model and Risk App)

## Introduction
CEMRA originated from the following Quantitative Microbial Risk Assessment model by Jones, 2020 - https://www.tandfonline.com/doi/full/10.1080/15459624.2020.1784427

CEMRA is a web application developed using the original model, which allows the user to modify the virus load, emissions, controls, patients behaviour and susceptible behaviour.

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
Q1. Add in starting concentrations in air, surfaces etc?
Q2. Set duration via side bar?
Q3. Have to update the metadata file if non-hospital scenario picked
Q4. Check input settings are same as new version of app
