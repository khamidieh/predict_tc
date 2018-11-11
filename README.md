# Predict the Critical Temperature of a Superconductor

For the most up to date information and for citation please see [here](https://www.sciencedirect.com/science/article/pii/S0927025618304877).  An arXiv version is [here](https://arxiv.org/abs/1803.10260).  You can also see the slides presented at the March 2018 American Physical Society: [KamH_APS_March_2018.pdf](https://github.com/khamidieh/predict_tc/blob/master/KamH_APS_March_2018.pdf).

All the code I used is now available in the file script main_script_production_9.R.  Be careful blindly copying and running this code; some chunks may take days to run.


This readme file shows you how to set up and make predictions.  Step (6) shows you how to download the data.  If you just want to  download the data, just run steps (1), (4), and (6).

------------------------------------------------------------------------------------------------------------------------------------

**Step (1)** 

Go to https://cloud.r-project.org/ to install the latest version of R.


**Step (2)** 

Once R has been installed, you need two R packages.  Open up the R gui and run the following command.  If a pop up window opens up with the title "Secure CRAN mirror", just pick "0-Cloud [https]" and hit OK.  
```r
install.packages(c("xgboost","CHNOSZ"))
```
You only need to run the above command once unless you download a new R version.


**Step (3)** 

You need to make the R packages available to your current R session.  You need to do this *EVERY* time you start a new R session.  At the R prompt run:
```r
library(xgboost)
library(CHNOSZ)
```
One more time: You need to do this *EVERY* time you start a new R session.


**Step (4)** 

Next, download the file `tc.RData` which is posted here in github; just go to https://github.com/khamidieh/predict_tc/blob/master/tc.RData and hit the `Download` button.  Make sure you know where in your computer you downloaded this file.  This file contains all the data and auxiliary functions to do the predictions.

*Important: Make sure your R session's working directory is set to where you downloaded `tc.RData`.*

You can do this in two ways: (1) Go to the R Console, then go to menu path File, and then to "Change dir...", or (2) use ```setwd() ``` command.  (I do not use RStudio.)

Once your directory is set properly run:
```r
load("tc.RData")
```

**Step (5)** 

Start predicting.  Try these:
```r
predict_tc("Ba0.2La1.8Cu1O4", verbose = TRUE)
predict_tc("MgB2")
predict_tc("Hg")
predict_tc("Ca0.5Sr0.5C6", verbose = TRUE)
predict_tc("NaSn2As2", verbose = TRUE)
predict_tc("H2S", verbose = TRUE)
predict_tc("FCl", verbose = TRUE)
predict_tc("mgB2", verbose = TRUE)
```
Setting ```verbose = TRUE``` will find materials similar to the one you entered.  The default is false.


**Step (6)**

Run the following to get the train data.  This train data was used to create the XGBoost model.
```r
write.csv(train, "train.csv", row.names = F)
```

You can also run this to get a data file that has the chemical formulas broken up.
```r
write.csv(unique_m, "unique_m.csv", row.names = F)
```

