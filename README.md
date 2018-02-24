# Predict the Critical Temperature of a Superconductor

The first thing you need to do is to make sure you have installed R.  Go to https://cloud.r-project.org/ to install the latest version of R.

Next, download the file auxiliary.RData.  Make sure you know where in your computer you downloaded this file.  This file contains all the data and auxiliary functions to create the prediction model.

You need to two R packages.  Open up the R gui and run the following command.  If a pop up window opens up with title "Secure CRAN mirror", just pick "0-Cloud [https]" and hit OK.  
```r
install.packages(c("ranger","CHNOSZ"))
```
You only need to run the above command only once.  
