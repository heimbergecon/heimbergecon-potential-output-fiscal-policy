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
macro_data <- fread(here("Italy PO revisions 2007equals100.csv"))

plots_title_size <- 12
plots_axis_title_size <- 11
plots_axis_ticks_size <- 10

macro_data_melt <- melt(macro_data, id.var="forecast")

x_axis_breaks <- c(1995, 2000, 2005, 2007, 2010, 2015, 2020)

#Figure 3 in the Intereconomics paper
plot_Italy <- ggplot(data=macro_data_melt,
                       aes(x=forecast, y=value, colour=variable)) +
  geom_line() + geom_point() +
  xlab("Year") +
  ylab("2007 = 100") +
  ggtitle("Italy: downward revisions in potential output over time (2007=100)") +
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(
    breaks=x_axis_breaks, 
    expand = expand_scale(
      mult = c(0, 0), add = c(0, 0.5)
    )
  ) +
  theme_icae() +
  theme(axis.title = element_text(color="black", size=plots_axis_title_size),
        plot.title = element_text(color="black", size=plots_title_size),
        axis.text = element_text(color="black", size=plots_axis_ticks_size))
plot_Italy

ggsave(plot = plot_Italy, 
       filename = here("Italy-PO-revisions.pdf"),
       width = 10, height = 4)
