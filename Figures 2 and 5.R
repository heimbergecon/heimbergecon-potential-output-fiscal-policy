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
library(lmtest)
if (!require("icaeDesign")){
  devtools::install_github("graebnerc/icaeDesign")
}
source(here("R/setup_country_classification.R"))

download_bond_data <- F
download_eurostat_debt <- F
macro_data <- fread(here("OGNonsense_Paneldata.csv"))

macro_data$losspotentialoutput <- (macro_data$potentialoutput2019-macro_data$potentialoutput2020)/macro_data$potentialoutput2019*100
macro_data$outputgapcalc <- (macro_data$potentialoutput2020-macro_data$realGDP2020)/macro_data$potentialoutput2019*100
macro_data$lossrealGDP <- (macro_data$potentialoutput2019-macro_data$realGDP2020)/macro_data$potentialoutput2019*100
macro_data$lossrealGDPcheckconsistenty <- macro_data$losspotentialoutput + macro_data$outputgapcalc

revision_data <- subset(macro_data, year %in% c('2021'))
revision_data <- select(revision_data, iso3c, lossrealGDP, losspotentialoutput)

reg <- lm(losspotentialoutput~lossrealGDP, data=revision_data)
summary(reg)
coeftest(reg, vcovHAC)

#Figure 5 in the Intereconomics paper
plot_bivariate<-ggplot(revision_data, aes(x=lossrealGDP, y=losspotentialoutput)) +
  geom_smooth(method=lm, se=F) +
  geom_point(shape=1) +
  xlab("Predicted loss in actual output, year 2021 (in %)") +
  ylab("Predicted loss in potential output, year 2021 (in %)") +
  ggtitle("Correlation of actual and potential output losses in the context of COVID-19: \n Spring 2020 forecast vs. Autumn 2019 forecast ") +
  theme(axis.text=element_text(size=10))+
  theme(axis.title=element_text(size=10)) +
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  annotate("text", x = 3.5, y = 5, size=3.5, label = "y = 0.58x - 0.501
           T-value (HAC-robust) = 2.70 (**)
           R_sq = 0.31") +
  geom_text_repel(data=revision_data, aes(x=lossrealGDP, y=losspotentialoutput, label=iso3c))
plot_bivariate

ggsave(plot = plot_bivariate, 
       filename = here("correlation_loss_actual_potential.pdf"),
       width = 10, height = 4)

#Actual vs. potential output losses (Autumn 2019 vs. Autumn 2007)

data_past <- fread(here("POlossessince2007.csv"))
data_past$ccode <- countrycode(data_past$V1, 'country.name', 'iso3c') #convert OECD name codes to three letter country codes (iso3n)

reg_past <- lm(potentialoutputloss~GDPloss, data=data_past)
summary(reg_past)
coeftest(reg_past, vcovHAC)

#Figure 2 in the Intereconomics paper
plot_bivariate_past<-ggplot(data_past, aes(x=GDPloss, y=potentialoutputloss)) +
  geom_smooth(method=lm, se=F) +
  geom_point(shape=1) +
  xlab("Loss in actual output, year 2019 (in %)") +
  ylab("Loss in potential output, year 2019 (in %)") +
  ggtitle("Correlation of actual and potential output losses, year 2019") +
  theme(axis.text=element_text(size=10))+
  theme(axis.title=element_text(size=10)) +
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  annotate("text", x = 7, y = 40, size=3.5, label = "y = 0.97x + 1.29
           T-value (HAC-robust) = 30.68 (***)
           R_sq = 0.99") +
  geom_text_repel(data=data_past, aes(x=GDPloss, y=potentialoutputloss, label=ccode))
plot_bivariate_past

ggsave(plot = plot_bivariate_past, 
       filename = here("past_correlation_loss_actual_potential.pdf"),
       width = 10, height = 4)