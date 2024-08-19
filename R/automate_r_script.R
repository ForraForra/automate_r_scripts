
## Automate R scripts with GitHub Actions


## Hämtar paket och modell
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

readmit_model <- read_rds("inst/final_readmission_model.rds")


## Dekryptera authorization till Google Drive and Google Sheet

#googledrive setup
googledrive::drive_auth(
  path = gargle::secret_decrypt_json("inst/googledrive_encrypt.json", 
                                     "GOOGLEDRIVE_KEY")
)
#googlesheet setup
googlesheets4::gs4_auth(
  path = gargle::secret_decrypt_json("inst/googledrive_encrypt.json", 
                                     "GOOGLEDRIVE_KEY")
)


## Hämta data från sheets 1 och skapa prediktioner för alla raderna 


# Ansluter google sheeten
sheet_id <- "https://docs.google.com/spreadsheets/d/1mDT8Pe-PtE__SxXs2dCaLttRMB1XFYk2PoUSwZuPAfU/edit?usp=sharing"

patient_data <- read_sheet(sheet_id, sheet = 1)
patient_data_with_pred <- read_sheet(sheet_id, sheet = 2)

#Prediktionerna tsm med tidigare kolumner pullas till sheet 2
pred_data <- 
  augment(readmit_model, patient_data) |> 
  select(all_of(colnames(patient_data)), 
         "Prediction" = ".pred_class", 
         "Prob No Readmit" = ".pred_No", 
         "Prob Readmit" = ".pred_Yes")
sheet_write(ss = sheet_id, data = pred_data, sheet = "Patient Data with Prediction")

# Rad 44 till 50 bör läggas "#" för raderna, då det annars kmr komma med i automatiseringen senare 


## Formatterar prediktionsvärdena till procent (0.456 -> 45%)
format_prediction_to_percent <- function(sheet, pos){
  x <- gs4_get(sheet)

  range_spec <- googlesheets4:::as_range_spec(
    pos,
    sheet = 2,
    sheets_df = x$sheets, nr_df = x$named_ranges
  )
  
  range_req <- googlesheets4:::as_GridRange(range_spec)
  
  cell_req <- list(
    userEnteredFormat = list(
      numberFormat = list(
        type = "NUMBER", 
        pattern = "0%"
      )
    )
  )
  field_req <- "userEnteredFormat.numberFormat"
  
  #request_generated() genererar en google sheets API
  req <- request_generated(
    "sheets.spreadsheets.batchUpdate", 
    params = list(
      spreadsheetId = as_sheets_id(sheet), 
      requests = list(
        repeatCell = list(
          range = range_req, 
          cell = cell_reg, 
          fields = field_req
        )
      )
    )
  )
  resp_raw <- request_make(req)
  gargle::response_process(resp_raw)
}


## Automation script för prediktion

# Prediktionerna skapas med readmission modellen
# Skrpitet ska automatiskt utföra en prediktion (till sheets) när en ny patient-rad läggs in. 

#Kod från rad 37 behövs köras innan nedan (givet rad 44 körts en gång på de "gamla" raderna):
new_patient <- patient_data |> 
  anti_join(patient_data_with_pred |> 
              select("patient_id"))

if(nrow(new_patients) > 0) {
  pred_data <-
    augment(readmit_model, new_patients) |>
    select(all_of(colnames(new_patients)),
           "Prediction" = ".pred_class",
           "Prob No Readmit" = ".pred_No",
           "Prob Readmit" = ".pred_Yes")

  
  if(!all(colnames(patient_data_with_pred) == colnames(pred_data))) {
    stop("Patient data and prediction database don't match")
  }
  
  
  sheet_append(ss = sheet_id, 
               data = pred_data, 
               sheet = "Patient Data with Prediction")
  
  format_prediction_to_percent(sheet_id, pos = "N")
  format_prediction_to_percent(sheet_id, pos = "O")
  
}


## Skriptet som automatiskt ska skicka emails

if(nrow(new_patients) > 0){
  smtp <- emayili::server(
    host = "smtp.gmail.com",
    port = 465,
    username = Sys.getenv("GMAIL_USERNAME"),
    password = Sys.getenv("GMAIL_PASSWORD")
  )
  send_to <- c("exempelmail@gmail.com")
  
  #SKapar emailet
  emayili <- envelope() %>%
    from("ludvigforsmark@gmail.com") %>%
    to(send_to) %>%
    subject("New Readmission Prediction") %>%
    emayili::render(
      input = "R/automate_r_email_content.Rmd", 
      params = list(
        patient_id = pred_data$patient_id, 
        pred = pred_data$Prediction, 
        no_readmit = pred_data$`Prob No Readmit`, 
        yes_readmit = pred_data$`Prob Readmit`, 
      ), 
      squish = F, 
      include_css = "bootstrap"
    )
  
  smtp(emayili, verbose = TRUE)
}

