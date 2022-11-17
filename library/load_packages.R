
# might contain packages that are not really needed for the app

Sys.setenv(LANG = "en") # run to force error messages to be in English

library(biomaRt)
library(shiny)
library(gtools)
library(shinyjs)
library(DT)
library(devEMF)
library(broom)
library(ggrepel)
library(withr)
library(tidyverse)
library(scales)
library(plotly)
library(readxl)
library(scales)
library(shinydashboard)
library(shinybrowser)
library(pathfindR)
library(knitr)
# theme_set(theme_bw())
theme_set(theme_bw(base_size = 14))

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")