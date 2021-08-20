rm(list=ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(here)
library(reldist)
library(latex2exp)
library(scales)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
source(here("R/setup_country_classification.R"))

download_bond_data <- F
download_eurostat_debt <- F
macro_data <- fread(here("OGNonsense_Paneldata.csv"))

macro_data$epsilon <- (macro_data$CAB2020 - macro_data$fiscalbalance2020)/macro_data$outputgap2020 *(-1)
macro_data$controlCAB <- macro_data$fiscalbalance2020 - (macro_data$outputgap2020*macro_data$epsilon)
macro_data$outputgapnorev2020 <- (macro_data$realGDP2020-macro_data$potentialoutput2019)/macro_data$potentialoutput2019*100

macro_data$CABnorev2020 <- macro_data$fiscalbalance2020 - (macro_data$outputgapnorev2020*macro_data$epsilon)
macro_data$structuralbalancenorev2020 <- macro_data$CABnorev2020 - macro_data$oneoffeffects2020

macro_data$epsilon2019 <- (macro_data$CAB2019 - macro_data$fiscalbalance2019)/macro_data$outputgap2019 *(-1)

macro_data_restr <- subset(macro_data, year %in% c('2021'))
comparison_table <- select(macro_data_restr, iso3c, structuralbalance2020, structuralbalancenorev2020)
comparison_table$Country <- countrycode(comparison_table$iso3c, 'iso3c', 'country.name')
comparison_table_new <- select(comparison_table, Country, structuralbalance2020, structuralbalancenorev2020)

write.csv(comparison_table_new, "strucbalance-comparison.csv")