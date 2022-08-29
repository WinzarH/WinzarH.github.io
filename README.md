# WinzarH.github.io
Easy access to some of my work for colleagues 

## Purpose

Recent conference papers and journal articles reference some innovative techniques, data analyses, and data visualisations. 
I created this repository to share the code for these projects.

## Contents

### LikertMaker

Most simulations of Likert-scale data are created to test analysis methods before finalising a questionnaire. Here, however, we want to reproduce published results where only the first two moments (mean and variance) are reported. Many publications report only means and standard deviations of rating scales, often only the means. The boundaries of a scale (1-5, 1-7, etc.) will often produce skewed data, so such simple statistics can be misleading. We demonstrate a novel method to reproduce Likert-scale data using only mean and standard deviation so that they can be properly visualised and interpreted. The algorithm is surprisingly accurate. 

LikertMaker is a Shiny app embedded inside a single Quarto document for easy download and use.

##### Requirements
You will need to have installed: 
  1. the R statistical programming language 
  1. a recent version of RStudio GUI program
  1. install the following libraries
      * library(dplyr)      (data manipulation)
      * library(ggplot2)    (data visualisation)
      * library(shinybusy)  (calculation/ progress indicator)
      * library(DEoptim)    (non-linear optimisation)
      
      
