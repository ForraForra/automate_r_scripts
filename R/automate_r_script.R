
###########################################################
###########################################################
## Automate R scripts with GitHub Actions: Deploy a model
###########################################################
###########################################################


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 7: Load the readmission model
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##


##---------------------------------------------------------------------------##
## Load required packages and readmission model
##---------------------------------------------------------------------------##

## Load required packages

library(googlesheets4)
library(readr)
library(dplyr)
library(parsnip)
library(recipes)
library(glmnet)
library(workflows)
library(emayili)
library(gargle)
library(tidyr)


## Load the prediction model


##---------------------------------------------------------------------------##
## Decrpyt authorization to Google Drive and Google Sheet
##---------------------------------------------------------------------------##

## googledrive De-auth setup


## googlesheet De-auth setup


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 8: Load the data and make predictions
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Read in patient data and make predictions 
##---------------------------------------------------------------------------##

## Load the googlesheet where data is stored


## Load the patient raw data from the first sheet


## Read the second sheet that will contain the patient data and prediction


## Make predictions for the patient data (sheet 1)


## Save/write the prediction on the second sheet


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 9: Format prediction columns as percentages
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Helper function to format the N and O columns as percentages
##---------------------------------------------------------------------------##

## Helper function to help format googlesheet (0.456 -> 45%)
format_prediction_to_percent <- function(sheet, pos){
  
  ## Get the Google Sheet
  x <- gs4_get(sheet)
  
  ## Determine the targeted sheet
  range_spec <- googlesheets4:::as_range_spec(
    pos,
    sheet = 2,
    sheets_df = x$sheets, nr_df = x$named_ranges
  )
  
  ## Form request
  range_req <- googlesheets4:::as_GridRange(range_spec)
  
  ## Specify the cell formatting
  
  
  ## Generate and send the API request
  
  
  
  ## Execute the request and send the response
  resp_raw <- request_make(req)
  gargle::response_process(resp_raw)
}

##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 10: Write an automation script for prediction
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Automated script for prediction
##---------------------------------------------------------------------------##

## Capture the data of new patients in sheet 1



## If there is a new patient, then make prediction and 
## append to sheet 2

if(nrow(new_patients) > 0) {
  
  ## Apply model on new patient data
  pred_data <-
    augment(readmit_model, new_patients) |>
    select(all_of(colnames(new_patients)),
           "Prediction" = ".pred_class",
           "Prob No Readmit" = ".pred_No",
           "Prob Readmit" = ".pred_Yes")
  
  ## Instead of overwriting the data, we need to append new data
  
  ## Before appending, let's confirm that the column names are still the same.
  ## They should be, but it best practice to confirm.
  
  if(!all(colnames(patient_data_with_pred) == colnames(pred_data))) {
    stop("Patient data and prediction database don't match")
  }
  
  ## Append new patient and prediction to sheet 2
  
  
  ## Format the text col (i.e 0.564 -> 56%)
  
  
}


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 12: Write automation script to send emails
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Automation script for emails
##---------------------------------------------------------------------------##

if(nrow(new_patients) > 0){
  ## Prepare email message
  smtp <- emayili::server(
    host = "smtp.gmail.com",
    port = 465,
    username = Sys.getenv("GMAIL_USERNAME"),
    password = Sys.getenv("GMAIL_PASSWORD")
  )
  
  send_to <- c("")
  
  ## Create and send email message
  
  
  ## Show as the email sends
  smtp(emayili, verbose = TRUE)
}


