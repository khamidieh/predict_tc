#################################################################################
###
### Make sure I start with a clean slate.
### Set the directory to the latest working directory.
###

###
### Do not run the rm command often.  
###

rm(list=ls(all=TRUE))

setwd("C:/Users/proud/Documents/RESEARCH/PROJECT - Superconductivity/Productions/Production_9")


#################################################################################
###
### This part describes the clean up process for the data.
### The superconducting material list was obtained from:
###  http://supercon.nims.go.jp/index_en.html
###  on July 24th, 2017, 11:15 AM.
###
### Once logged in, I clicked on "OXIDE & METALLIC".
### The site took me to http://supercon.nims.go.jp/supercon/material_menu.
### There, I clicked on the "search" to get *all* the data.
### I took a screen shot of the site.  The screen shot file is in "menu.pdf".
###
### 31639 rows of data are indicated.
###
### I clicked on "Data Download" to download the data.
### The file is "material.csv".
###
### I made a copy of "material.csv" into "material_copy.csv".  
### "material_copy.csv" is the file used for all the clean up.
###
###
### The following are done manually:
### (1) Removed columns ma1 to mj2.
### (2) Sorted the data by Tc from highest to lowest.
### (3) The critical temp for the following "num" variables were shifted by ONE column to the
###     right.  I fixed these manually: 31020, 31021, 31022, 31023, 31024, 31025, 
###     153150, 153149, 42170, 42171, 30716, 30717, 30718, 30719, 
###     150001, 150002, 150003, 150004, 150005, 150006, 150007, 
###     30712, 30713, 30714, 30715.
### (4) The following are removed since the cricial temp seem wrong:
###     num = 111620,     La0.23Th0.77Pb3
###     num = 9632,	    Pb2C1Ag2O6
###     num = 140,	    Er1Ba2Cu3O7-X
### (5) All rows with Tc = 0 or missing are removed. 
### (6) Columns nums, mo1, mo2, oz, str3, tcn, tcfig, refno are removed.
### (7) The following changes are made manually:
###     Y2C2Br0.5!1.5 is removed.
###     Y1Ba2Cu3O6050 is removed.  I suspect 6050 is a mistake.
###     Nd185Ce0.15Cu1O4 is removed.  There was a Nd1.85Ce0.15Cu1O4 already in the data.
###     Hg1234O10 is removed. 1234 for Hg???? This doesnot seem correct.
###     Bi1.6Pb0.4Sr2Cu3Ca2O1013 is changed to Bi1.6Pb0.4Sr2Cu3Ca2O10.13
###     Y1Ba2Cu285Ni0.15O7 is changed to Y1Ba2Cu2.85Ni0.15O7
###
### (9) The following are removed because they have a zero as a coefficient:
###     "Bi1.7Pb0.3Sr2Ca1Cu2O0"  (Zero of Oxygen?)
###     "La1.85Nd0Ca1.15Cu2O5.99"
###     "Bi0Mo0.33Cu2.67Sr2Y1O7.41"
###     "Y0.5Yb0.5Ba2Sr0Cu3O7"
### (10) These are removed because they don't make sense:
###      Yo975Yb0.025Ba2Cu3O
###      Yo975Yb0.025Ba2Cu3O
###      Yo975Yb0.025Ba2Cu3O
### (10) Change all oxygen designations such as O8-z to O8, O5+X TO O5, etc.
###      Important: I had to do this in the correct sequence; for example find and replace
###      Oz+8 before Oz, etc.
###      I did this manually!  Took a lot of time.  I initially approached it by
###      string manipulations but trying to take care of the exceptions was taking too much time.
### (11) The column heading changed to "material" and critical_temp
###

###
### This is the fantastic R package that reads and breaks up formulas:
###

install.packages("CHNOSZ")
library(CHNOSZ)


###
### Now upload the data and make sure you get no error messages.
###

dat = read.csv("material_copy.csv", stringsAsFactors = FALSE, header = TRUE)

###
### The following takes about 9 seconds:
###
### Run to see if you see any problems: errors, warning, etc.
### Also gives you an idea about how long looping over 25000 rows
###  and using the "makeup" function take.
###

system.time({
for (i in 1:nrow(dat))
{
  makeup(dat$material[i])
}
})


###
### I got no error or warning messages.
###

###
### Just look at a snippet of the data:
###

head(dat)
summary(dat)
dim(dat)

###
### No missing values for critical temperatures.
### There are 24861 rows.
###

###
### Let's see how many rows were left out from the original data:
###

tmp = read.csv("material.csv", na.strings = "", 
               stringsAsFactors = FALSE,
               header = TRUE)
dim(tmp)

###
### 31639 - 24861 = 6778 are left out.
###

###
### Clean up:
###

ls()
rm(tmp)
rm(i)

###
### Only the following elements will be considered.
### All the atomic numbers are less than 86.
###

elements = c("H", "He", "Li", "Be", "B", "C",  "N", "O", "F", "Ne", "Na", "Mg", 
             "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", 
             "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", 
             "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In",
             "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", 
             "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", 
             "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn")

###
### In this next section, I'll only pick up the materials with atomic number <= 86.
### 

###
### The reject object holds the index value of the materials that are rejected.
###
### The following chunk takes about 9 seconds:
###

reject = rep(0, nrow(dat))

system.time({
for (i in 1:nrow(dat))
{
  tmp = makeup(dat$material[i])
  if (!all(names(tmp) %in% elements)) reject[i] = 1
}
})

###
### How many are rejected?
###

length(which(reject == 1))


###
### 973 are rejected.
###

###
### Pick up the remaining and save data into 
###  an object called kept_dat.  
###

kept_dat = dat[ which(reject == 0), ]
dim(kept_dat)
dim(dat)

###
### Now I have 23888 = 24861 - 973 rows.
###

###
### Save the rejected material for later analysis.
###

rejected_dat = dat[ which(reject == 1), ]


###
### Again, some clean up.
###

ls()
rm(i)
rm(tmp)

###
### The following chunk will take about 265 seconds or about 5 minutes.
###
### Matrix (and later converted to data frame) m will be used later to 
###  gather summary statistics from the data.
### It could also be used for feature extraction.
###

row_names_kept_dat = row.names(kept_dat)

m = matrix(0, nrow(kept_dat), length(elements) )
colnames(m) = elements

system.time({
for ( i in 1:nrow(kept_dat) )
{
  tmp_values                      = makeup(kept_dat$material[i])
  tmp_location_of_matches         = match(  names(tmp_values),  colnames(m) )
  m[i, tmp_location_of_matches  ] = tmp_values
}
})



###
### Add the temperature back:
###

m = as.data.frame(m)
m$critical_temp = kept_dat$critical_temp

###
### Add the row names back:
###

row.names(m) = row_names_kept_dat
 

###
### Get only unique rows.
###

dim(m)

unique_m = unique(m)

dim(unique_m)

###
### 23888 - 21263 = 2625 rows omitted.
###

###
### Now add the chemical formulas back to m:
###

m$material = kept_dat$material

###
### Now add the chemical formula's back to unique_m
###
 
unique_m$material = m$material[row.names(m) %in% row.names(unique_m)]

dim(m)
dim(unique_m)

###
### m:        You have 23888 by 2
### unique_m: You have 21263 by 2
###
### 23888 - 21263 = 2625 duplicates
###


###
### I did a final check here:
###
### Here v just records the largest number of atoms 
###  for an element in a material.
### The intent is to catch any thing suspecious.
###

v = numeric(nrow(unique_m))
 
for (i in 1:nrow(unique_m))
{
  v[i] = max(makeup(unique_m$material[i]))
}

unique_m$material[which(v > 90)]

###
### Most of these look legit but some don't look right. For example, "B105" doesn't look legit.
### They are mostly alloys. 
### I will leave these as is.
###

dim(unique_m)

###
### I am down to 21263 material.
### I lost 31639 - 21263 = 10376 rows.
### I have picked up 67% of the data.
###

###
### Remove junk:
###

ls()
rm(i)
rm("row_names_kept_dat")
rm("tmp_location_of_matches")
rm("tmp_values")
rm(v)
ls()
gc()


###
### Next, I upload the data from elements.
### The subfolder has "Production_1" because this data never changed.
###

element_data = read.csv("C:/Users/proud/Documents/RESEARCH/PROJECT - Superconductivity/Productions/Production_1/data_csv.csv",header = T)

###
### Assign row names:
###

rownames(element_data) = element_data$Name


###
### Remove some useless stuff or stuff that can be highly 
### correlated with other material.
### This was a judgement call and comes from initial experimentation
###  with the element superconductivity.
###

element_data$Name              = NULL
element_data$Color             = NULL
element_data$ElectricalType    = NULL
element_data$QuantumNumbers    = NULL
element_data$RefractiveIndex   = NULL
element_data$VanDerWaalsRadius = NULL
element_data$VickersHardness   = NULL
element_data$BulkModulus       = NULL

element_data$NeutronCrossSection   = NULL
element_data$NeutronMassAbsorption = NULL

###
### This is what I have at this point:
###
### (1) dat:           Data file that has the material, and the critical temperature.
###                    This is the cleaned data but has *duplicates* and materials with
###                    elements which could have atomic number over 86.
### (2) rejected_dat : This is the data which had elements with atomic number over 86.
### (8) reject:        Index of rejected material in "dat" object.
### (3) kept_dat:      This is the dat but with reject_data removed.  It has duplicates. 
### (2) element_data:  This is the original file that has all the elements 
###                    and their properties. This does have a lot of missing values.
### (3) elements:      This is just a list of chemical symbols of elements used for the 
###                    analysis. 
### (6) m:             This is a large data table that in addition to the material names
###                    and critical temp, it also has 86 columns corresponding to 86 possible elements 
###                    and whether the element listed in the column is in material or not.
###                    It has duplicates.
### (7) unique_m:      This is m with the duplicates removed.  It has no duplicates.
###
###
### Save the data for future analysis:
###

save.image("processed_data_production_9.RData")

###
### The file size is around 1.215 MB.
###

###
### This is used for checking. These should be deleted from 
###  the local directory.
###

write.csv(dat,       "dat.csv")
write.csv(m,         "m.csv")
write.csv(unique_m,  "unique_m.csv")


######################################################################################
######################################################################################
######################################################################################
###
### In this part, I will just visually inspect the data and make some summaries.
###

###
### First let's look at the data I kept.
###

###
### Set up first:
###

library(CHNOSZ) 

setwd("C:/Users/proud/Documents/RESEARCH/PROJECT - Superconductivity/Productions/Production_9")
load("processed_data_production_9.RData")

###
### Let's count the number of elements:
###

###
### This function just counts:
###

count_it = function(x)
{
  sum(x > 0)
}

###
### This is the table that has the proportions.
### Note we just count columns 1:86.  87 & 88 have
###  critical temp and formula name.
###

p_table          = apply(unique_m[,1:86], 2, count_it)/nrow(unique_m)
names(p_table)   = colnames(unique_m[,1:86])
non_zero_p_table = p_table[which(p_table > 0)]
non_zero_p_table = sort(non_zero_p_table, decreasing = TRUE) 

###
### Now plot it:
###


off_set = 0.015

default_mar = c(5, 4, 4, 2) + 0.1  # c(bottom, left, top, right)

### mar c(bottom, left, top, right) = default is c(5, 4, 4, 2) + 0.1

par(mar = c(2, 5, 2, 1) + 0.1)

x = 1:length(non_zero_p_table)
plot( x = x, y = non_zero_p_table, xaxt = "n", xlab = "",
      ylab = "Element Proportion", pch = 19,
      cex.lab = 1.25, ylim = c(0,max(non_zero_p_table) + off_set * 1.05), cex = 0.65)
abline(h = seq(from = 0, to = 0.6, by = 0.05), lwd = 0.5, col = "grey")
text(x, non_zero_p_table+off_set, names(non_zero_p_table), cex = 0.65)

###
### The plot above is saved in "element_proportions.pdf".
### I did have to resize the plot.
###

###
### I'll also create a top 20 proportion too:
###

non_zero_p_table_top_20 = non_zero_p_table[1:20]

x = 1:length(non_zero_p_table_top_20)
plot( x = x, y = non_zero_p_table_top_20, xaxt = "n", xlab = "",
      ylab = "Element Proportion", pch = 19,
      cex.lab = 1.25, ylim = c(0,max(non_zero_p_table_top_20) + off_set * 1.05), cex = 0.65)
abline(h = seq(from = 0, to = 0.6, by = 0.05), lwd = 0.5, col = "grey")
text(x, non_zero_p_table_top_20 + off_set* 1.1, names(non_zero_p_table_top_20))

###
### The plot above is saved in "element_proportions_top_20.pdf".
### I did have to resize the plot.
###

###
### Let's save a csv as well
###

write.csv(non_zero_p_table, "non_zero_p_table.csv")

###
### Now collect summary data per element.
### ncol(m) - 2 : This is the number of elements
### 1:86        : Leave out 87 which the formula and 88 which is the temp
### 7           : 7 parameters used.
###

summary_matrix = matrix(NA, ncol(unique_m) - 2, 7)
rownames(summary_matrix) = colnames(unique_m)[1:86]
colnames(summary_matrix) = c("Min", "Q1", "Med", "Q3", "Max", "Mean", "SD")

for ( j in 1:nrow(summary_matrix) )
{
  tmp = unique_m[ which(unique_m[,j] > 0), "critical_temp"]
  summary_matrix[j,1] = min(tmp)
  summary_matrix[j,2] = quantile(tmp, prob = 0.25, names = F)
  summary_matrix[j,3] = median(tmp)
  summary_matrix[j,4] = quantile(tmp, prob = 0.75, names = F)
  summary_matrix[j,5] = max(tmp)
  summary_matrix[j,6] = mean(tmp)
  summary_matrix[j,7] = sd(tmp)
}

###
### Order by mean & Now plot the means:
###

summary_matrix = summary_matrix[ order(summary_matrix[,6], decreasing = TRUE) , ]
summary_matrix = as.data.frame(summary_matrix)

###
### The last 9 lines give non-sense results.
### These are mostly noble gases.  Remove them:
###

summary_matrix = na.omit(summary_matrix)

###
### Now plot them:
###

mean_crit_temp_per_element = summary_matrix[,"Mean"]

off_set = 2

### mar c(bottom, left, top, right) = default is c(5, 4, 4, 2) + 0.1
par(mar = c(2, 5, 2, 1) + 0.1)

x = 1:length(mean_crit_temp_per_element)
plot( x = x, y = mean_crit_temp_per_element, xaxt = "n", xlab = "", ylab = "Mean Critical Temperature (K)", 
     , pch = 19, cex.lab = 1.25, ylim = c(-5, max(mean_crit_temp_per_element) + off_set * 1.1) + 5, cex = 0.65)
abline(h = seq(from = 0, to = 80, by = 10), lwd = 0.1, col = "grey")
text(x, mean_crit_temp_per_element + off_set, rownames(summary_matrix), cex = 0.65)

###
### The file above is saved in "mean_crit_temp_per_element.pdf".
###

###
### Look at the top 20 as well.
###

### mar c(bottom, left, top, right) = default is c(5, 4, 4, 2) + 0.1

windows(width = 10, height = 7)
par(mar = c(2, 5, 2, 1) + 0.1)

x = 1:length(mean_crit_temp_per_element[1:20])
plot( x = x, y = mean_crit_temp_per_element[1:20], xaxt = "n", xlab = "", ylab = "Mean Critical Temperature (K)", 
     , pch = 19, cex.lab = 1.25, ylim = c(35, 85), cex = 0.65)
abline(h = seq(from = 35, to = 85, by = 5), lwd = 0.1, col = "grey")
text(x, mean_crit_temp_per_element[1:20] + 2, rownames(summary_matrix)[1:20])

###
### The file above is saved in "mean_crit_temp_per_element_top_20.pdf".
###

###
### Now repeat but sort by standard deviation:
###

summary_matrix = summary_matrix[ order(summary_matrix[,7], decreasing = TRUE) , ]
summary_matrix = as.data.frame(summary_matrix)

###
### Now plot it:
###

sd_crit_temp_per_element = summary_matrix[,"SD"]

off_set = 2

### mar c(bottom, left, top, right) = default is c(5, 4, 4, 2) + 0.1
par(mar = c(2, 5, 2, 1) + 0.1)

x = 1:length(sd_crit_temp_per_element)
plot( x = x, y = sd_crit_temp_per_element, xaxt = "n", xlab = "", ylab = "SD Critical Temperature (K)", 
     , pch = 19, cex.lab = 1.25, ylim = c(-5, max(sd_crit_temp_per_element) + off_set * 1.1) + 5, cex = 0.65)
abline(h = seq(from = 0, to = 80, by = 5), lwd = 0.1, col = "grey")
text(x, sd_crit_temp_per_element + off_set, rownames(summary_matrix), cex = 0.65)

###
### The plot is saved in "sd_crit_temp_per_element.pdf".
###

###
### Look at the top 20 as well.
###

### mar c(bottom, left, top, right) = default is c(5, 4, 4, 2) + 0.1

windows(width = 10, height = 7)
par(mar = c(2, 5, 2, 1) + 0.1)

x = 1:length(sd_crit_temp_per_element[1:20])
plot( x = x, y = sd_crit_temp_per_element[1:20], xaxt = "n", xlab = "", ylab = "SD Critical Temperature (K)", 
     , pch = 19, cex.lab = 1.25, ylim = c(30,45), cex = 0.65)
abline(h = seq(from = 30, to = 45, by = 5), lwd = 0.1, col = "grey")
text(x, sd_crit_temp_per_element[1:20] + 0.75, rownames(summary_matrix)[1:20])

###
### The plot is saved in "sd_crit_temp_per_element_top_20.pdf".
###

###
### Now plot the mean and sd 
###


windows(width = 9, height = 5)
par(mfrow=c(1,2), mar = c(5,5,3,1) + 0.1)
plot(SD ~ Mean ,      data = summary_matrix,  
     xlab = "Mean Critical Temperature (K)", ylab = "SD Critical Temperature (K)", pch = 19, cex.lab = 1)
plot(SD ~ log(Mean) , data = summary_matrix,  
     xlab = "log(Mean Critical Temperature) (K)", ylab = "SD Critical Temperature (K)", pch = 19, cex.lab = 1)
par(mfrow=c(1,1))

###
### The picture is saved in "sd_vs_mean_crit_temp.pdf".
###

###
### Let's look at the histogram of the critical temp for the data selected
###

par(mfrow = c(1,3))
hist(unique_m$critical_temp, col = "grey", freq = FALSE, xlab = "Critical Temperature (K)", main = "")
boxplot(unique_m$critical_temp, col = "grey", xlab = "Critical Temperature (K)", horizontal = TRUE, pch = 19)
plot(rev(sort(unique_m$critical_temp)),  cex = 0.65, pch = 19, ylab = "Critical Temperature (K)", ylim = c(0,200))
abline(h = 0 , lwd = 0.1, lty = 2)
par(mfrow = c(1,1))

###
### The above is saved in "hist_boxplot_crit_temp.pdf".
###

hist(unique_m$critical_temp, col = "grey", freq = FALSE, 
    xlab = "Critical Temperature (K)", main = "")

###
### The above is saved in "hist_crit_temp.pdf".
###


###
### Let's look at the histogram of critical temp of the rejected 
###  material.
###

rejected_temps = dat[which(reject == 1), "critical_temp"]
hist(rejected_temps, col = "grey", freq = FALSE, xlab = "Critical Temperature (K)", main = "")

###
### The above is saved in "hist_rejected_crit_temp.pdf".
###

###
### Let's compare the quantiles of the two distributions:
###

quantiles_material_kept     = quantile(unique_m$critical_temp, prob = seq(from = 0.01, to = 0.99, by = 0.01), names = F)
quantiles_material_rejected = quantile(rejected_dat$critical_temp, prob = seq(from = 0.01, to = 0.99, by = 0.01), names = F)

par(mar = c(5,5,3,1) + 0.1)
plot(quantiles_material_kept  ~ quantiles_material_rejected, pch = 19, 
     xlab = "Quantiles of Rejected", ylab = "Quantiles of Kept")
abline(a = 0, b = 1, col = "red", lty = 2)

###
### The above is saved in "quantiles_of_kept_vs_rejected.pdf".
###

###
### This does not make it look like they are the same; Critical temps are higher for
###  what I have kept!
###

###
### Compare summary measures:
###

round(summary(unique_m$critical_temp),2)
round(summary(rejected_dat$critical_temp),2)

### > round(summary(unique_m$critical_temp),2)
###    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###    0.00    5.37   20.00   34.42   63.00  185.00 
### > round(summary(rejected_dat$critical_temp),2)
###    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###    0.09    1.36    3.43   12.58   12.50  135.00 

###
### Actually, it looks like I am leaving some low temp ones out!
###

###
### Let's look at some values at random:
###

set.seed(8811)
rejected_dat[ sample( 1:nrow(rejected_dat), size = 5, replace = F), ]

###
### I could see some mistakes: D0.018Nb0.982 = what is D?
### Pu1Co1Ga5 has Plutonium, etc.
###

###
### Let's do some clean up:
###

rm(j)
rm(tmp)
rm(x)

###
### Save results up to summary analysis:
###

save.image("visual_and_summary_analysis.RData")


###################################################################################
###################################################################################
###################################################################################
###
### In this next part, I will create the training data.
###

###
### This function is the new definition of standard deviation.
### It is the population sd.
###
### I had some problems with R's sd function.
###

std = function(x)
{
  out = sqrt(mean((x - mean(x))^2))
  out
}

#############################################################################
###
### This function gets the basic summaries based
###  on one variable.
### Note that I will be ignoring the missing values.
###
### y = some property of material such as the atomic mass
### p = proportion or coefficient in the chemical formula
###

get_features = function(y, p)
{

  ###
  ### out = index to remove the missing values.
  ###

  out = -which(is.na(y))

  if ( length(out) > 0 ) {
    new_y = y[out]
    new_p = p[out]
  } else {
    new_y = y
    new_p = p
  }

  ###
  ### Now get the features after NA's have been removed.
  ###

  ###
  ### I also have added absolute value for some measurements because
  ### some values may be negative.
  ###

  mean_y        = mean(new_y)
  wtd_mean_y    = sum(new_p * new_y)

  gmean_y       = exp(mean(log(abs(new_y))))
  wtd_gmean_y   = exp(sum(new_p * log(abs(new_y))))

  tmp           = abs(new_y)/sum(abs(new_y))
  entropy_y     = -1 * sum(tmp * log(tmp))

  tmp           = (new_p * abs(new_y))/sum(new_p * abs(new_y))
  wtd_entropy_y = -1 * sum(tmp * log(tmp))

  range_y       = max(new_y) - min(new_y)
  wtd_range_y   = max(new_p * new_y) - min(new_p * new_y)

  lrange_y      = max(log(abs(new_y)))   - min(log(abs(new_y)))
  wtd_lrange_y  = max(new_p * log(abs(new_y))) - min(new_p * log(abs(new_y)))

  std_y         = std(new_y)
  wtd_std_y     = sqrt(sum((new_y - wtd_mean_y)^2*new_p))  

  out = c(mean_y, wtd_mean_y, gmean_y, wtd_gmean_y, entropy_y,
          wtd_entropy_y, range_y, wtd_range_y, std_y, wtd_std_y)

  out
}

#############################################################################
###
### This is the function that will extract information
###
### x  = formula of the material
### ed = element data file.  "ed" **MUST** have a column name "Element".
###

extract = function(x, ed)
{

  ###
  ### Note that names(ratios) gives the 
  ###  listing of the elements.
  ###
  ### ratios here, tells us how many of each element is in the 
  ###  material formula.  These are basically the coefficients
  ###  in front of the elements in the chemical formula.
  ###

  ratios      = makeup(x)
  prob_ratios = data.frame( prob_ratios = ratios/sum(ratios), Element = names(ratios))

  ###
  ### Here's the subset table I create:
  ###
                   
  subset_table = subset(ed, Element %in% names(ratios),  
                        c(Element, AtomicMass, FirstIonizationEnergy, AtomicRadius, Density, ElectronAffinity,
                          FusionHeat, ThermalConductivity,   Valence) )

  ###
  ### Merge with the proportons.  Very important: the column "Element"
  ###  must be present in both subset_table and prob_ratios!
  ###

  subset_table = merge(subset_table, prob_ratios)

  ###
  ### Atomic Mass:
  ###

  AtomicMass_features = get_features(subset_table[,"AtomicMass"], subset_table[,"prob_ratios"])

  ###
  ### First Ionization Energy = FIE
  ###
  ### Note FIE is highly correlated with Electronegativity
  ###

  FirstIonizationEnergy_features = get_features(subset_table[,"FirstIonizationEnergy"], 
                                                subset_table[,"prob_ratios"])
 

  ###
  ### AtomicRadius
  ### Note: AtomicRadius & CovalentRadius are highly correlated.
  ###

  AtomicRadius_features = get_features(subset_table[,"AtomicRadius"], 
                                       subset_table[,"prob_ratios"])


  ###
  ### Density
  ###

  Density_features = get_features(subset_table[,"Density"], 
                                       subset_table[,"prob_ratios"])


  ###
  ### ElectronAffinity
  ### Note: A small constant 1.5 should have been added to prevent log(0) = -Inf
  ###

  ElectronAffinity_features = get_features(subset_table[,"ElectronAffinity"], 
                                       subset_table[,"prob_ratios"])


  ###
  ### FusionHeat
  ###

  FusionHeat_features = get_features(subset_table[,"FusionHeat"], 
                                       subset_table[,"prob_ratios"])


  ###
  ### ThermalConductivity
  ###

  ThermalConductivity_features = get_features(subset_table[,"ThermalConductivity"], 
                                       subset_table[,"prob_ratios"])


  ###
  ### Valence
  ###

  Valence_features = get_features(subset_table[,"Valence"], 
                                       subset_table[,"prob_ratios"])

  ###
  ### Output
  ###

  number_of_elements = length(ratios)

  out = c(number_of_elements, AtomicMass_features, FirstIonizationEnergy_features,
          AtomicRadius_features, Density_features, ElectronAffinity_features,
          FusionHeat_features, ThermalConductivity_features, Valence_features)

  return(out)
}

#############################################################################
###
### There will be two train data; This first one has the iron and cuprate
### 81 is the initial number of features extracted.
### 82 will be whether iron is in there.
### 83 will be whether it is cuprate.
### 84 will be the response.
###
### train: This is the main file I will use to create the models.
###

train = matrix(0, nrow(unique_m), 84)

colnames(train) = c(

  "number_of_elements",  
  
  "mean_atomic_mass",        
  "wtd_mean_atomic_mass",  
  "gmean_atomic_mass",       
  "wtd_gmean_atomic_mass",  
  "entropy_atomic_mass",     
  "wtd_entropy_atomic_mass", 
  "range_atomic_mass",       
  "wtd_range_atomic_mass",   
  "std_atomic_mass",
  "wtd_std_atomic_mass",

  "mean_fie",        
  "wtd_mean_fie",    
  "gmean_fie",       
  "wtd_gmean_fie",   
  "entropy_fie",    
  "wtd_entropy_fie", 
  "range_fie",       
  "wtd_range_fie",   
  "std_fie",
  "wtd_std_fie",

  "mean_atomic_radius",        
  "wtd_mean_atomic_radius",    
  "gmean_atomic_radius",       
  "wtd_gmean_atomic_radius",   
  "entropy_atomic_radius",    
  "wtd_entropy_atomic_radius", 
  "range_atomic_radius",       
  "wtd_range_atomic_radius",   
  "std_atomic_radius",
  "wtd_std_atomic_radius",

  "mean_Density",        
  "wtd_mean_Density",    
  "gmean_Density",       
  "wtd_gmean_Density",   
  "entropy_Density",    
  "wtd_entropy_Density", 
  "range_Density",       
  "wtd_range_Density",   
  "std_Density",
  "wtd_std_Density",

  "mean_ElectronAffinity",        
  "wtd_mean_ElectronAffinity",    
  "gmean_ElectronAffinity",       
  "wtd_gmean_ElectronAffinity",   
  "entropy_ElectronAffinity",    
  "wtd_entropy_ElectronAffinity", 
  "range_ElectronAffinity",       
  "wtd_range_ElectronAffinity",   
  "std_ElectronAffinity",
  "wtd_std_ElectronAffinity",

  "mean_FusionHeat",        
  "wtd_mean_FusionHeat",    
  "gmean_FusionHeat",       
  "wtd_gmean_FusionHeat",   
  "entropy_FusionHeat",    
  "wtd_entropy_FusionHeat", 
  "range_FusionHeat",       
  "wtd_range_FusionHeat",   
  "std_FusionHeat",
  "wtd_std_FusionHeat",

  "mean_ThermalConductivity",        
  "wtd_mean_ThermalConductivity",    
  "gmean_ThermalConductivity",       
  "wtd_gmean_ThermalConductivity",   
  "entropy_ThermalConductivity",    
  "wtd_entropy_ThermalConductivity", 
  "range_ThermalConductivity",       
  "wtd_range_ThermalConductivity",   
  "std_ThermalConductivity",
  "wtd_std_ThermalConductivity",

  "mean_Valence",        
  "wtd_mean_Valence",    
  "gmean_Valence",       
  "wtd_gmean_Valence",   
  "entropy_Valence",    
  "wtd_entropy_Valence", 
  "range_Valence",       
  "wtd_range_Valence",    
  "std_Valence",
  "wtd_std_Valence",

  "iron",        
  "cuprate",    

  "critical_temp"
)

###
### Let's not alter element_data file but work with a subset of it.
###

subset_element_data = element_data[,c("Element", "AtomicMass", "FirstIonizationEnergy", "AtomicRadius", "Density", "ElectronAffinity",
                          "FusionHeat", "ThermalConductivity",   "Valence")]

###
### Replacing some of the missing values from: https://www.webelements.com
###

subset_element_data[ which( subset_element_data$Element == "La"),"AtomicRadius"] = 195
subset_element_data[ which( subset_element_data$Element == "Ce"),"AtomicRadius"] = 185

###
### For ElectronAffinity add small constant 1.5 to prevent log(0) = -Inf
###

subset_element_data$ElectronAffinity = subset_element_data$ElectronAffinity + 1.5

###
### Here's the main loop.
### In case > library(CHNOSZ) has not been run, make sure you run it.
###
###
### This takes 60 seconds or 1 minute.
###

system.time({

for (i in 1:nrow(unique_m))
{
  if ( unique_m[i, "Fe"] > 0 ) tmp_iron = 1 else tmp_iron = 0
  if ( (unique_m[i, "O"] > 0) & (unique_m[i, "Cu"] > 0) ) tmp_cuprate = 1 else tmp_cuprate = 0
  train[i,]  = c(as.numeric(extract( x = unique_m[i,"material"], ed = subset_element_data)) , 
                    tmp_iron, tmp_cuprate, unique_m$critical_temp[i])
}

})

###
### Take a quick look:
###

summary(train)

###
### Change train into a data.frame:
###

train = as.data.frame(train)

###
### Looks okay.
###

summary(train)

###
### Change iron and cuperate to categorical
###

train$iron    = as.factor(train$iron)
train$cuprate = as.factor(train$cuprate)

###
### Take a quick look again:
###

summary(train)

###
### Remove some junk:
###

ls()
rm(x)
rm(u)
rm(tmp_iron)
rm(tmp_cuprate)
rm(tmp_material)
rm(tmp)
rm(i)
rm(a)

###
### Now here is where I will separate the data into two groups:
###

train_with_indicators = train
train$iron    = NULL
train$cuprate = NULL


###
### Save for later analysis:
###

save.image("all_data_production_9.RData")

###
### The above is about 1.2 megabytes.
###

###
### Check one line to make sure it is working!
###


######################################################################################
######################################################################################
######################################################################################
###
### In this part the predictors are examined.
###

library(CHNOSZ)
load("all_data_production_9.RData")

###
### Take a look at the plot of each variable versus the critical temp:
###

pdf("univariate_plots.pdf")

for ( i in 1:(ncol(train_with_indicators) - 1) )
{
  plot(train_with_indicators$critical_temp ~ train_with_indicators[,i], xlab = colnames(train_with_indicators)[i],
       ylab = "Critical Temperature (K)")
  abline(a = 0, b = 1, col = "red")
}
dev.off()

###
### I think the plots of iron and cuprates are interesting.
###

par(mar = c(5,5,3,1) + 0.1)
boxplot(train_with_indicators$critical_temp ~ train_with_indicators$iron, pch = 19, 
     xlab = "Iron", names = c("No","Yes"), col = "grey",
     ylab = "Critical Temperature (K)", cex.axis = 1.25, cex.lab = 1.25)

###
### This is saved in file "iron_vs_temp.pdf".
###

###
### Let's look at the mean and sd
###

mean(train_with_indicators[ train_with_indicators$iron == "1", "critical_temp"])
sd(train_with_indicators[ train_with_indicators$iron == "1", "critical_temp"])

mean(train_with_indicators[ train_with_indicators$iron == "0", "critical_temp"])
sd(train_with_indicators[ train_with_indicators$iron == "0", "critical_temp"])

median(train_with_indicators[ train_with_indicators$iron == "1", "critical_temp"])
median(train_with_indicators[ train_with_indicators$iron == "0", "critical_temp"])

length(train_with_indicators[ train_with_indicators$iron == "1", "critical_temp"])

summary(train_with_indicators[ train_with_indicators$iron == "1", "critical_temp"])
summary(train_with_indicators[ train_with_indicators$iron == "0", "critical_temp"])

###
### 2339 iron material.
### For the iron = 1: median = 21.7, mean = 26.89978, sd = 21.4333
### For the iron = 0: median = 19.6, mean = 35.35087, sd = 35.40876
###
### For the iron = 1:
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###  0.018  11.250  21.700  26.900  35.500 130.000 
### 
### For the iron = 0:
###     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
###  0.00021   4.80000  19.60000  35.35087  68.00000 185.00000 
###

t.test(train_with_indicators$critical_temp ~ train_with_indicators$iron)

###
### 95% confidence interval: 7.446312 9.455865
###

###
### Now do the plot for the cuprates:
###

par(mar = c(5,5,3,1) + 0.1)
boxplot(train_with_indicators$critical_temp ~ train_with_indicators$cuprate, pch = 19, 
     xlab = "Cuprate",  names = c("No","Yes"), col = "grey",
     ylab = "Critical Temperature (K)", cex.axis = 1.25, cex.lab = 1.25)

###
### This is saved in file "cuprate_vs_temp.pdf".
###

###
### Let's look at the mean and sd
###

mean(train_with_indicators[ train_with_indicators$cuprate == "1", "critical_temp"])
sd(train_with_indicators[ train_with_indicators$cuprate == "1", "critical_temp"])

mean(train_with_indicators[ train_with_indicators$cuprate == "0", "critical_temp"])
sd(train_with_indicators[ train_with_indicators$cuprate == "0", "critical_temp"])

median(train_with_indicators[ train_with_indicators$cuprate == "1", "critical_temp"])
median(train_with_indicators[ train_with_indicators$cuprate == "0", "critical_temp"])

length(train_with_indicators[ train_with_indicators$cuprate == "1", "critical_temp"])

summary(train_with_indicators[ train_with_indicators$cuprate == "1", "critical_temp"])
summary(train_with_indicators[ train_with_indicators$cuprate == "0", "critical_temp"])

###
### 10532 cuprate material.
### For the cuprate = 1: median = 63.1, mean = 59.85045, sd = 31.16545
### For the cuprate = 0: median = 5.7,  mean = 9.463563, sd = 10.6838
###
###> summary(train[ train$cuprate == "1", "critical_temp"])
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###  0.001  31.000  63.100  59.850  86.000 143.000 
###
###> summary(train[ train$cuprate == "0", "critical_temp"])
###     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
###  0.00021   2.52000   5.70000   9.46356  12.20000 185.00000 
###

t.test(train_with_indicators$critical_temp ~ train_with_indicators$cuprate)

###
### 95% confidence interval: -51.01553 -49.75823
###


###
### Let's look at one example:
###

par(mar = c(5,5,3,1) + 0.1)
plot(train_with_indicators$critical_temp ~ train_with_indicators$mean_ThermalConductivity, pch = 19, 
     xlab = "Mean Thermal Conductivity (W/(m K))", 
     ylab = "Critical Temperature (K)", cex.axis = 1.25, cex.lab = 1.25)

###
### This is saved as "thermal_cond_vs_critic_temp.pdf"
###

###
### Nothing looks linear.  Potential methods are non-parametric.
###

###
### Let's look at the correlation plot of the data:
###
### I have to exclude the two indicator variables.
### Columns 82 & 83 are iron and cuprate,
### so I am back to train.
###

install.packages("corrplot")
library(corrplot)

corrplot(cor(train), method = "square", type = "lower", tl.cex = 0.35)

###
### This is save as "correlation_plots.pdf"
###

###
### Let's get an idea of the size of the correlation matrix
###

cor_mat = cor(train)

mean(abs(cor_mat[lower.tri(cor_mat, diag = FALSE)]) )

###
### The mean absolute value of correlations is 0.3456327.
### There is a good amount of correlation among the 
###  variables on average.
###

###
### Let's look at eigen values of the PCA of the features only.
###
### Exclude the last column since it is the response.
###

pca_object = princomp(x = train[,-82], cor = TRUE)

plot(pca_object)

cum_var = cumsum(pca_object$sdev^2)/sum(pca_object$sdev^2)
plot(cum_var, type = "b", pch = 19, xlab = "Number of Components", 
     ylab = "Cummulative Variance")
abline(h = c(0.9,0.99), lty = 2)

###
### This is saved in "pca_var_cum.pdf"
###

###
### Looks like 40-50 components can do a good job of explaining the variation in the data.
### In fact with 31 components, you can explain 99% of the variation.
###

###
### Next I will create a linear model as a baseline model.
### I had tried a model with the log of the response with previous 
### attempts but it was not much better.  I will scale the numeric data
### so the coefficients are easier to interpret.
###

###
### Here's a function that scales the data.  I do *NOT*
### like the R's scale function.
###

scale_it = function(x)
{
  out = (x - mean(x))/sd(x)
  out
}

###
### Loop through the columns and scale them.
### The response is not scaled.
###
### Note that this does not include iron and cuprate
###

scaled_train = train

for (i in 1:81)
{
  scaled_train[,i] = scale_it(scaled_train[,i])
}

###
### Here's the model:
###

linear_mod = lm(critical_temp ~ ., data = scaled_train)
summary(linear_mod)

###
### Almost all the predictor show up statistical significant
###
###
### Residual standard error: 17.59
### Multiple R-squared:  0.7373,    Adjusted R-squared:  0.7363  
###

###
### Let's look at the size of the coefficients:
###

sort(abs(coef(linear_mod)), decreasing = T)

###
### All coefficients are positive.
###

###
### save the coeffcient sizes
###

tmp = sort(abs(coef(linear_mod)), decreasing = T)
write.csv(tmp, "linear_model_coef_size.csv")
rm(tmp)



###

###
### Let's look a predicted versus observed:
###

windows(width = 7, height = 5)
plot(linear_mod$fitted ~ scaled_train$critical_temp, pch = 19,
     ylab = "Predicted Critical Temperature (K)", xlab = "Observed Critical Temperature (K)")
abline(a = 0, b = 1, col = "red")

###
### This is saved in "linear_model_predicted_tc_vs_observed_tc.pdf".
###

###
### Let's look at the diagnostics.  
###

windows(width = 11, height = 4)
par(mfrow = c(1,3))
plot(linear_mod$residuals ~ scaled_train$critical_temp, pch = 19,
     ylab = "Residcuals", xlab = "Observed Critical Temperature (K)")
abline( h = 0, lty = 2)
qqnorm(linear_mod$residuals, main = "")
qqline(linear_mod$residuals)
hist(linear_mod$residuals, col = "grey", freq = F, xlab = "Residuals", main = "")
par(mfrow = c(1,1))

###
### The picture is saved in "linear_model_residuls_plot.pdf".
###

###
### Do a cross validation to see the performance of the linear model:
###

set.seed(100000)

n = 25

tmp_mse = numeric(n)
tmp_r2  = numeric(n)

system.time({
for (i in 1:n)
{
  ind = sample(x = c(1,2,3), size = nrow(scaled_train), replace = TRUE)
  tmp_test   = scaled_train[  which(ind == 1),]
  tmp_train  = scaled_train[ -which(ind == 1),]

  tmp_model  = lm(critical_temp ~ ., data = tmp_train)

  tmp_y      = predict(tmp_model, tmp_test)

  tmp_mse[i] = mean((tmp_y - tmp_test$critical_temp)^2)
  tmp_r2[i]  = (cor(tmp_y , tmp_test$critical_temp))^2

}
})

###
### The above took 6 seconds
###

summary(sqrt(tmp_mse))
summary(tmp_r2)

###
### I can report the means:
###
###
###> summary(sqrt(tmp_mse))
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###  17.24   17.53   17.66   17.63   17.74   18.03 
###> summary(tmp_r2)
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 0.7274  0.7305  0.7343  0.7350  0.7389  0.7488  
###

###
### Look at some plots:
###

par(mfrow = c(1,2))
plot(sort(sqrt(tmp_mse)))
plot(sort(tmp_r2))
par(mfrow = c(1,1))

###
### The plots did not reveal much.
###

###
### The results are similar but slightly improved relative to what I had before.
### I won't do pcr as I expect nothing to change there.
###

###
### Some clean up:
###

rm(n)
rm(i)
rm(tmp_model)
rm(tmp_mse)
rm(tmp_r2)
rm(tmp_test)
rm(tmp_train)
rm(tmp_y)

###
### Save for later:
###

save.image("linear_model_analysis.RData")


##################################################################
###
### In this section, random forests are fitted.
###

install.packages("ranger")
library(ranger)

###
### Try a timing with but change num.trees to 2500, and default of mtry = 9
###
###
### It took about 175 seconds.
###

date()
system.time({
tmp_rf_model = ranger(critical_temp ~ ., data = train, num.trees = 2500)
})
date()



###
### Try another timing with but change num.trees to 2500, and default of mtry = 50
###
### This took 752 seconds or 13 minutes!
###

date()
system.time({
tmp_rf_model = ranger(critical_temp ~ ., data = train, mtry = 50, num.trees = 2500)
})
date()


###
### This is the tuning:
###

rf_grid = expand.grid(mtry          = seq(from = 1, to = 40, by = 1), 
                      num.trees     = c(1000, 2500),
                      min.node.size = c(1, 5, 25))

rf_grid

n = dim(rf_grid)[1]
n

###
### The grid size 240
###


###
### This could take up tp 240 * 13 min = 3120 min or 2.2 days!
###

###
### This will be done in my dell machine so my time estimate may be off.
###

###
### This tmp.RData will be loaded to Dell Machine
###

save.image("data_for_dell_tuning.RData")


################################################################
###
### This chunk is done in Dell
###

library(ranger)
load("data_for_dell_tuning.RData")

set.seed(10^7)

mse_vals = numeric(n)

start_time = date()
system.time({
for (i in 1:n)
{
  tmp_model = ranger(critical_temp ~ ., data          = train, 
                                        mtry          = rf_grid$mtry[i],
                                        num.trees     = rf_grid$num.trees[i],
                                        min.node.size = rf_grid$min.node.size[i])
                         
  mse_vals[i]   = tmp_model$prediction.error   

  cat(i,"\n")   
   
}
})
end_time = date()
save.image("dell_tuning_results.RData")

###
### End of chunk in Dell
###
### Start Time = "Tue Feb 13 10:33:21 2018"
### End Time = "Thu Feb 15 09:06:45 2018"
###
###       user     system    elapsed 
### 1178981.68    3822.94  167602.53 
###
### Going with the elapsed time, this is 2 days and 47 hours!
###
################################################################

###
### Reload from dell results. About 7 seconds.
###

system.time({
load("dell_tuning_results.RData")
})

###
### Now analyze the results:
###

results = cbind(rf_grid, sqrt(mse_vals))
colnames(results)[ncol(results)] = "rmse"

results = as.data.frame(results)

###
### Get the best results:
###

results[which.min(results$rmse),]

###  
###  The best:
###
###> results[which.min(results$rmse),]
###   mtry num.trees min.node.size     rmse
###99   19      1000             5 8.958336
###

plot(sort(results$rmse))
summary(results$rmse)

###
###> summary(results$rmse)
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###  8.958   8.974   8.988   9.099   9.209  10.389 
###
### There does not seem to be too much variability.
###

plot(rmse ~ as.factor(min.node.size), data = results)

###
### Not much a difference between min.node.size of 1 and 5
###  but a bigger differene for min.node.size of 25.
###

plot(rmse ~ mtry, data = results)

###
### The top band is for min.node.size 25.
###

plot(rmse ~ as.factor(mtry), data = results)

###
### However, it is obvious that low mtry is not good.
###

###
### What does this look like without min size of 25
###

y_limits = c(min(results$rmse)*0.99, max(results$rmse)*1.01)

par(mfrow = c(1,3))
plot(rmse ~ as.factor(mtry), ylim = y_limits,
    data = results[results$min.node.size == 1, ])
plot(rmse ~ as.factor(mtry), ylim = y_limits, 
    data = results[results$min.node.size == 5, ])
plot(rmse ~ as.factor(mtry), ylim = y_limits,
   data = results[results$min.node.size == 25, ])
par(mfrow = c(1,1))

###
### I like the min.node.size = 1 the most.  Seems less sensitive to mtry.
###

###
### Let's see what the result was for min node size of 1
###

tmp = results[results$min.node.size == 5, ]
tmp[which.min(tmp$rmse),]

tmp = results[results$min.node.size == 1, ]
tmp[which.min(tmp$rmse),]

###
###> tmp = results[results$min.node.size == 5, ]
###> tmp[which.min(tmp$rmse),]
###   mtry num.trees min.node.size     rmse
###99   19      1000             5 8.958336
###> 
###> tmp = results[results$min.node.size == 1, ]
###> tmp[which.min(tmp$rmse),]
###   mtry num.trees min.node.size     rmse
###61   21      2500             1 8.959162
###
###
### Do the number of trees make a difference?
###

plot(rmse ~ as.factor(num.trees), data = results)

###
### Not really.
###

###
### Let's look at this:
###


lower_limit = min(results$rmse)
upper_minit = max(results$rmse)

par(mfrow = c(1,2))

for (j in c(1000,2500))
{
  tmp = results[which(results$min.node.size == 1),]
  tmp$min.node.size = NULL
  tmp = tmp[which(tmp$num.trees == j),]
  plot(rmse ~ mtry, data = tmp, pch = 19, type = "b", 
      ylim = c(lower_limit, upper_minit), main = j)
}
par(mfrow = c(1,1))

###
### It looks like that mtry may not make much of difference unless it is
### too low or min.node.size is set too high.

###
### I'll go with result becase the differences are small:
###   mtry num.trees min.node.size     rmse
###10   10      1000             1 8.993745
### 

###
### Let's time it and see how long this takes:
###

system.time({
  tmp = ranger(critical_temp ~ ., data = train, mtry = 21, min.node.size = 1,
                        num.trees = 2500)
})

###
### This took 230 seconds.
### If I do this 25 times: 25*230= 5750 seconds 
###


###
### Save this and run it on dell:
###

save.image("dell_final_cv.RData")



###
###
### This part for final cross validation.
### This is run in the dell machine.
###

load("dell_final_cv.RData")

set.seed(100000)

n = 25

tmp_mse = numeric(n)
tmp_r2  = numeric(n)

system.time({
for (i in 1:n)
{
  ind = sample(x = c(1,2,3), size = nrow(train), replace = TRUE)
  tmp_test  = train[  which(ind == 1),]
  tmp_train = train[ -which(ind == 1),]

  tmp_model = ranger(critical_temp ~ ., data = tmp_train, mtry = 10, min.node.size = 1,
                        num.trees = 1000)

  tmp_y      = predict(tmp_model, data = tmp_test)

  tmp_mse[i] = mean((tmp_y$prediction - tmp_test$critical_temp)^2)
  tmp_r2[i]  = (cor(tmp_y$prediction , tmp_test$critical_temp))^2

}
})

save.image("dell_final_cv_results.RData")

###
### The above took  3938 seconds 
###

###
### Now Analyze
###

par(mfrow = c(1,2))
plot(sort(sqrt(tmp_mse)))
plot(sort(tmp_r2))
par(mfrow = c(1,1))

summary(sqrt(tmp_mse))
summary(tmp_r2)

### 
###> summary(sqrt(tmp_mse))
###> summary(sqrt(tmp_mse))
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###  9.140   9.322   9.484   9.452   9.556   9.763 
###> summary(tmp_r2)
###   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 0.9194  0.9225  0.9242  0.9242  0.9260  0.9290  
### 

sqrt(mean(tmp_mse))
mean(tmp_r2)

###
### I think a safe rmse is 9.453426 or about 9.5
### A safe r2 is about 0.9241977.
###

###
### Now create the final model:
###


set.seed(10203040)
system.time({
final_rf_model = ranger(critical_temp ~ ., data = train, mtry = 10, 
                        min.node.size = 1,
                        num.trees = 1000, importance = "permutation" )
})

###
### This takes about 488 seconds or about 8.13 minutes.
###
###
### What are the in sample values?
###

final_rf_model

###
### Training rmse = sqrt(80.67905) = 8.982152
### Training r2   = 0.931241
###
###

###
### Let's look at the variable importance:
###

rf_importance = sort(sqrt(final_rf_model$variable.importance))

dotchart(x = rf_importance, labels = names(rf_importance), pch = 19, cex = 0.4, pt.cex = 0.75)

###
### This is saved in "random_forest_variable_importance.pdf"
###

###
### Let's look at the predicted vs fitted but note that these are the out of sample
### predicted values.
###

plot(final_rf_model$predictions ~ train$critical_temp, pch = 19,
     ylab = "Predicted Critical Temperature (K)", xlab = "Observed Critical Temperature (K)")
abline(a = 0, b = 1, col = "red")

###
### File, saved in "random_forest_out_of_sample_predicted_tc_vs_observed_tc.pdf".
###

###
### I think in sample predictions are unreliable.
### Look at the out of sample residuals.
###

###
### Let's look at the residuals:
###

rf_residuals = train$critical_temp - final_rf_model$predictions 


par(mfrow = c(1,3))
plot(rf_residuals ~ train$critical_temp, pch = 19,
     ylab = "Residuals (Obs - Pred)", xlab = "Observed Critical Temperature (K)")
abline( h = 0, lty = 2)
qqnorm(rf_residuals, main = "")
qqline(rf_residuals)
hist(rf_residuals, col = "grey", freq = F, xlab = "Residuals", main = "")
par(mfrow = c(1,1))


plot(rf_residuals ~ train$critical_temp, pch = 19,
     ylab = "Residuals (Observed - Predicted)", xlab = "Observed Critical Temperature (K)")
abline( h = 0, lty = 2)



###
### What was the sd of residuals:
###

sd(rf_residuals)

###
### 8.98234
###

###
### Let's add the 1 sd:
###

sd_limit = mean(sqrt(tmp_mse))

plot(rf_residuals ~ train$critical_temp, pch = 19,
     ylab = "Residuals (Observed - Predicted)", xlab = "Observed Critical Temperature (K)")
abline( h = 0, lty = 1)
abline( h = -sd_limit, lty = 2, col = "red")
abline( h = +sd_limit, lty = 2, col = "red")

###
### This is saved in file "random_forest_residual_vs_observed_tc.pdf"
###

###
### what percentage were above and below
###

above = sum(rf_residuals > sd_limit)
below = sum(rf_residuals < -sd_limit)
above
below

(above + below)/length(rf_residuals)

###
### above = 1642
### below = 1709
###
### (above + below)/length(rf_residuals) = 0.1575977
### 1 - 0.1575977 = 0.8424023
###

###
### Now save for later use:
###

rm(tmp_model)
rm(tmp_mse)
rm(tmp_r2)
rm(tmp_test)
rm(tmp_train)
rm(tmp_y)
rm(i)

save.image("results_after_final_model.RData")


######################################################################################
###
### In this part, I will perform the recursive feature elimination.
### Reference paper: Correlation and variable importance in random forests, 
###   stat computing, 2017
###  Baptiste Gregorutti1, Bertrand Michel, Philippe Saint-Pierre
###
### The steps are:
### (1) train a random forest
### (2) compute the permutation importance
### (3) Eliminate the less relevant variable
### (4) Repeat steps 1 to 3 until no further variables remian
### 

###
###  Here's the implementation.
###  The code is not efficient
###
###  This was run on the dell machine and not on my surface laptop.
###
###  Start of Dell run
###


library(ranger)

set.seed(564738)

variables_removed   = c()
removed_perm_error  = c()

train_for_imp = train

system.time({

for (i in 1:(ncol(train) - 1))  
{
  tmp = ranger(critical_temp ~ ., data = train_for_imp,  num.trees = 1000,
               importance = "permutation", min.node.size = 1 )
  
  one_var_removed    = names(which.min(tmp$variable.importance))
  variables_removed  = c( variables_removed, one_var_removed)
  removed_perm_error = c( removed_perm_error, min(tmp$variable.importance) )
  train_for_imp      = train_for_imp[ , - which( one_var_removed == colnames(train_for_imp) ) ]
}

})

###
### Took 21628 seconds or 6 hours!
###

save.image("results_after_rfe.RData")


###
###  End of Dell run:
###
######################################################################################


###
### Get the variables.
###
### Note that this is in the order of least important to most
###

var_imp_from_rfe = variables_removed

###
### Now combine the results, sorted from most important to least.
###

rfe_results = data.frame( predictor = var_imp_from_rfe, error = removed_perm_error ) 

###
### Plot the final results:
###

dotchart(x = rfe_results$error, labels = rfe_results$predictor, pch = 19, cex = 0.4, pt.cex = 0.75)

###
### This is saved in "rfe_variable_importance.pdf"
###

###
### Get the top 20:
###

dotchart(x = rfe_results$error[62:81], labels = rfe_results$predictor[62:81], pch = 19)

###
### This is saved in "rfe_variable_importance_top_20.pdf"
###

###
### I am not happy with rfe.  I will implement feature importance
###  by leaving out a group of variables.
###

###
### Final model mse:
###

final_rf_model_mse = final_rf_model$prediction.error

###
### 80.67905, sqrt(80.67905) = 8.98
###
############################################################################
###
### Function to get a prediction
###

library(CHNOSZ)

predict_tc = function(your_material, verbose = F)
{

  tmp = makeup(your_material)
  if ( any( names(tmp) == "Fe") ) tmp_iron = 1 else tmp_iron = 0
  if ( any( names(tmp) == "O") & any( names(tmp) == "Cu") ) tmp_cuprate = 1 else tmp_cuprate = 0

  mat = as.data.frame( t(c(as.numeric(extract( x = your_material, ed = subset_element_data)) , 
                    tmp_iron, tmp_cuprate))  )

  colnames(mat) = colnames(train)[-ncol(train)]
  prediction = predict(final_rf_model, mat)$prediction


  if (verbose) 
  {
    checker = numeric(ncol(unique_m[1:86]))
    names(checker) = colnames(unique_m)[1:86]
    checker[match(names(tmp) , colnames(unique_m))] = tmp

    matched_location = c()
    for (i in 1:nrow(unique_m))
    {
      if ( all(checker == unique_m[i,1:86]) ) matched_location = c(matched_location,i)
    }

   if (length(matched_location) > 0)
   {
     info = unique_m[matched_location,c("critical_temp", "material")]
   } else
     {
       info = "No match(es) found."
     }  
  }


  if (verbose) 
  {
    out = list(prediction = prediction, info = info)
  } else {
    out = prediction
  }

  return(out)
}

###
### Save the final results!
###

save.image("results_to_end_of_rf.RData")

######################################################################

###
### In this next part, I will do gbm tuning
###


library(gbm)

###
### Split the data into 2/3 train and 1/3 test:
###

n = floor(nrow(train)/3)
n

set.seed(10^4)
index_for_test_data = sample(1:nrow(train), size = n, replace = FALSE)

tmp_test  = train[ index_for_test_data, ]
tmp_train = train[-index_for_test_data, ]

###
### Now create the following models:
###

system.time({

model_10_05 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 10, shrinkage = 0.05, n.trees = 5000)
model_12_05 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 12, shrinkage = 0.05, n.trees = 5000)
model_14_05 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 14, shrinkage = 0.05, n.trees = 5000)
model_16_05 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 16, shrinkage = 0.05, n.trees = 5000)
model_18_05 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 18, shrinkage = 0.05, n.trees = 5000)
})

###
### Above took 27216 seconds or 7.5 hours
###

system.time({
model_10_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 10, shrinkage = 0.10, n.trees = 2500)
model_12_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 12, shrinkage = 0.10, n.trees = 2500)
model_14_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 14, shrinkage = 0.10, n.trees = 2500)
model_16_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 16, shrinkage = 0.10, n.trees = 2500)
model_18_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 18, shrinkage = 0.10, n.trees = 2500)
})

###
### The above took 13795 seconds or 3 hours 50 minutes
###

save.image("results_to_end_of_gbm_tuning.RData")


###
### Extract test rmse for the larger model:
###

rows_to_check_05 = seq(from = 1, to = 5000, by = 50)

model_lambda_05           = as.data.frame(matrix(NA, length(rows_to_check_05), 5))
colnames(model_lambda_05) = c("depth10","depth12","depth14", "depth16", "depth18")

tmp_observed = tmp_test$critical_temp

system.time({
for (i in 1:length(rows_to_check_05))
{
  model_lambda_05[i,"depth10"] = sqrt(mean(( tmp_observed - predict(model_10_05, tmp_test, n.trees = i))^2))
  model_lambda_05[i,"depth12"] = sqrt(mean(( tmp_observed - predict(model_12_05, tmp_test, n.trees = i))^2))
  model_lambda_05[i,"depth14"] = sqrt(mean(( tmp_observed - predict(model_14_05, tmp_test, n.trees = i))^2))
  model_lambda_05[i,"depth16"] = sqrt(mean(( tmp_observed - predict(model_16_05, tmp_test, n.trees = i))^2))
  model_lambda_05[i,"depth18"] = sqrt(mean(( tmp_observed - predict(model_18_05, tmp_test, n.trees = i))^2))

  cat(i,"\n")   
   
}
})


###
### Extract test rmse for the smaller model:
###

rows_to_check_10 = seq(from = 1, to = 2500, by = 25)

model_lambda_10           = as.data.frame(matrix(NA, length(rows_to_check_10), 5))
colnames(model_lambda_10) = c("depth10","depth12","depth14", "depth16", "depth18")

system.time({
for (i in 1:length(rows_to_check_10))
{
  model_lambda_10[i,"depth10"] = sqrt(mean(( tmp_observed - predict(model_10_10, tmp_test, n.trees = i))^2))
  model_lambda_10[i,"depth12"] = sqrt(mean(( tmp_observed - predict(model_12_10, tmp_test, n.trees = i))^2))
  model_lambda_10[i,"depth14"] = sqrt(mean(( tmp_observed - predict(model_14_10, tmp_test, n.trees = i))^2))
  model_lambda_10[i,"depth16"] = sqrt(mean(( tmp_observed - predict(model_16_10, tmp_test, n.trees = i))^2))
  model_lambda_10[i,"depth18"] = sqrt(mean(( tmp_observed - predict(model_18_10, tmp_test, n.trees = i))^2))

  cat(i,"\n")   
   
}
})

###
### Save:
###

save.image("results_to_end_of_gbm_tuning_and_scoring.RData")

###
### Look at the results:
###

apply(model_lambda_05, 2, min)

apply(model_lambda_10, 2, min)

###
### > apply(model_lambda_05, 2, min)
###  depth10  depth12  depth14  depth16  depth18 
### 12.52792 12.25158 12.00899 11.86604 11.69278 
### > apply(model_lambda_10, 2, min)
###  depth10  depth12  depth14  depth16  depth18 
### 11.74746 11.55576 11.31623 11.22463 11.05579 
###

###
### Shrinkage of 0.10 shows the most promise but still not great.
###

matplot(model_lambda_10)

######################################################################

###
### In this next part, I will do another tuning of gbm 
###

###
### Split the data into 2/3 train and 1/3 test:
###

n = floor(nrow(train)/3)
n

set.seed(10^4)
index_for_test_data = sample(1:nrow(train), size = n, replace = FALSE)

tmp_test  = train[ index_for_test_data, ]
tmp_train = train[-index_for_test_data, ]

###
### Use higher interaction terms and larger number of trees
###

system.time({
model_18_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 18, shrinkage = 0.10, n.trees = 10000)
model_20_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 20, shrinkage = 0.10, n.trees = 10000)
model_22_10 = gbm(critical_temp ~ ., data = tmp_train, distribution = "gaussian",
                   interaction.depth = 22, shrinkage = 0.10, n.trees = 10000)
})

###
### 45493 seconds or 12 hours and 40 min
###

###
### Check the rows:
###

##
### Extract test rmse for the smaller model:
###

rows_to_check_10 = seq(from = 1, to = 10000, by = 100)

model_lambda_10           = as.data.frame(matrix(NA, length(rows_to_check_10), 3))
colnames(model_lambda_10) = c("depth18","depth20","depth22")

system.time({
for (i in 1:length(rows_to_check_10))
{
  model_lambda_10[i,"depth18"] = sqrt(mean(( tmp_observed - predict(model_18_10, tmp_test, n.trees = i))^2))
  model_lambda_10[i,"depth20"] = sqrt(mean(( tmp_observed - predict(model_20_10, tmp_test, n.trees = i))^2))
  model_lambda_10[i,"depth22"] = sqrt(mean(( tmp_observed - predict(model_22_10, tmp_test, n.trees = i))^2))

  cat(i,"\n")   
   
}
})

apply(model_lambda_10, 2, min)

###
### depth18  depth20  depth22 
###10.93251 10.94463 10.71546 
###

### Ok, this seems hopeless.  GBM probably won't work.

#############################################################################
#############################################################################
#############################################################################

###
### First save the predict function into an old function:
###

predict_tc_old = predict_tc


###
### Now modify it to make it better:
###

library(CHNOSZ)
library(ranger)

predict_tc = function(your_material, verbose = F, match_level = 0.999999)
{

  tmp = makeup(your_material)

  mat = as.data.frame( t(c(as.numeric(extract( x = your_material, ed = subset_element_data))))  )

  colnames(mat) = colnames(train)[-ncol(train)]
  prediction = predict(final_rf_model, mat)$prediction


  if (verbose) 
  {
    checker        = numeric(ncol(unique_m[1:86]))
    names(checker) = colnames(unique_m)[1:86]
    checker[match(names(tmp) , colnames(unique_m))] = tmp

    normalize = function(x) { sqrt(sum(x^2))}

    checker   =  checker  / sqrt(sum(checker^2))
    checker   =  matrix(checker, 86,1)

    tmp_denom =  apply(unique_m[,1:86], 1, normalize)
    tmp_m     =  matrix( tmp_denom, nrow(unique_m[,1:86]), ncol(unique_m[,1:86])) 
    norm_m    =  as.matrix(unique_m[,1:86] / tmp_m)

    dot_res   = norm_m %*% checker
    matched_location = which(dot_res >= match_level)

   if (length(matched_location) > 0)
   {
     info = unique_m[matched_location,c("critical_temp", "material")]
   } else
     {
       info = "Not able to find match(es)."
     }  

   }

   if (verbose) 
   {
     out = list(prediction = prediction, info = info)
   } else {
     out = prediction
   }

  return(out)
}

###
### Finally, save
###

system.time({
save.image("FINAL.RData")
})


