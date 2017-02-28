README 

This folder contains all the scripts and data needed to replicate the paper 

'The Lieberson Assessment for Qualitative Comparative Analysis'

To replicate:

1) Run the simulations in "final_data_set.R." This will use multiple cores to write a series of trials to disk. This file uses the R code found in "sim.ltQCA.R."

2) Load the files to disk, run the regression models, and plot the results in "QCA_plots.R." 


To run  your own Lieberson assessment, use 

source("ltQCA.R")
ltQCA(yourQCAmodel)


