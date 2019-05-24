library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# OK-CLEAN ----------------------------------------------------------------

KeuntunganOKCLEAN_raw <- read_csv("data-raw/KeuntunganOKCLEAN.csv")
KeuntunganOKCLEAN_raw %>% 
  clean_names() %>% 
  write_csv("data-raw/001_ok-clean_keuntungan.csv")

KomplainOKCLEAN_raw <- read_csv("data-raw/KomplainOKCLEAN.csv")
KomplainOKCLEAN_raw %>% 
  clean_names() %>% 
  write_csv("data-raw/001_ok-clean_komplain.csv")

PenggunaOKCLEAN_raw <- read_csv("data-raw/PenggunaOKCLEAN.csv")
PenggunaOKCLEAN_raw %>% 
  clean_names() %>% 
  write_csv("data-raw/001_ok-clean_pengguna.csv")

# Twitter Bot -------------------------------------------------------------

twitter_bot_raw <- read_excel("data-raw/Dataset_bot.xlsx")
twitter_bot_raw %>% 
  clean_names() %>% 
  write_csv("data-raw/002_twitter-bot.csv")


# Lamudi ------------------------------------------------------------------

lamudi_raw <- read_csv("data-raw/Klasifikasi_Lamudi.csv")
lamudi_raw %>%
  clean_names() %>%
  mutate(harga_rumah = parse_number(harga_rumah, locale = locale(
    decimal_mark = ",", grouping_mark = "."
  ))) %>% 
  write_csv("data-raw/003_lamudi.csv")

# HDI ---------------------------------------------------------------------

hdi_raw <- read_csv("data-raw/DATASET HDI.csv")
hdi_raw %>% 
  clean_names() %>% 
  write_csv("data-raw/004_hdi.csv")

