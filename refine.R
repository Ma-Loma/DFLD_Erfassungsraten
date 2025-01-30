library(tidyverse)
library(data.table)
rm(list = ls())
full_data_path <- "data/details/first_refine"#Daten mit von Herrn Weise präferierten Fangradien

# Liste aller CSV-Dateien im Datenordner
csv_files <- function(path) {
  list.files(path = path,
             pattern = "*.csv",
             full.names = TRUE)
}

#csv_files(full_data_path)



# Funktion zum Einlesen einer einzelnen CSV-Datei
read_csv_file <- function(file) {
  read_delim(
    file,
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    na = c("", "NA", "---"),
    col_types = cols(.default = col_character()),
    locale = locale(encoding = 'ISO8859-1')#nötig, da sonst die Umlaute nicht gelesen werden können.
  ) %>%
    select(!c(Icao24, MaxIx, Zähler, `ts = Nenner`)) %>%
    select(!starts_with("Hip")) %>%
    select(!"L99day v Peak-Analyse":"L1night")
}

# Einlesen der CSV-Dateien mit präferierten Fangradien und zusammenführen in einen Dataframe
meas_data <- function(path) {
  csv_files(path) %>%
    lapply(., read_csv_file) %>%
    bind_rows()
}

data<-
  meas_data(full_data_path)

fwrite(data, file = "data/full_data.csv",sep = ";")

