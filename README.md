# Predict the Critical Temperature of a Superconductor

The first thing you need to do is to make sure you have installed R.  Go to https://cloud.r-project.org/ to install the latest version of R.

You need two R packages.  Open up the R gui and run the following command.  If a pop up window opens up with title "Secure CRAN mirror", just pick "0-Cloud [https]" and hit OK.  
```r
install.packages(c("ranger","CHNOSZ"))
```
You only need to run the above command once; you'll never to install these packages unless you download a new R version.

Now you need to make these packages available to your current R session.  You need to do this EVERY time you want to use these packages:
```r
library(ranger)
library(CHNOSZ)
```

Next, download the file auxiliary.RData.  Make sure you know where in your computer you downloaded this file.  This file contains all the data and auxiliary functions to create the prediction model.

Please read this next line carefully: **MAKE SURE YOUR R SESSION'S WORKING DIRECTORY IS SET TO WHERE YOU DOWNLOADED THIS FILE**.  

You can do this in two ways: (1) Go to R Console, then go to menu path File, and then to "Change dir...", or (2) use ```setwd ``` command.  (I do not use RStudio.)

Run:
```r
load("auxiliary.RData")
```

Now, you can create the random forest model.  If you set the random number generator to the seed below, you'll get the same results that I do.  The following command will take 2-3 minutes to run.
```r
set.seed(10203040)
final_rf_model = ranger(critical_temp ~ ., data = train, mtry = 10, min.node.size = 1, num.trees = 1000)
```

Start predicting.  Try these two:
```r
predict_tc("Ba0.2La1.8Cu1O4", verbose = TRUE)
predict_tc("MgB2")
```
Setting ```verbose = TRUE``` will find material similar to the one you entered.

