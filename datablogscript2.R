library.path <- .libPaths()
install.packages("pak")
library(pak, lib.loc=library.path)
pak::pak("r-lib/devtools")
pak::pak("janish-parikh/ZTest@main")
library(HypothesisTesting)
install.packages("survey") 
library(survey) 
library(dplyr)
install.packages("tidyr")
library(tidyr,lib.loc=library.path)
library(ggplot2)
columns <- read.csv('llcp2022variablelayout.csv')
columns$File_Width = sapply(1:nrow(columns), function(y) ifelse(y < nrow(columns), columns$Starting_Column[y + 1] - columns$Starting_Column[y], 1))
columns = columns[columns$File_Width > 0,]
responses = read.fwf("LLCP2022.ASC ", widths = columns$File_Width, col.names = columns$Variable_Name)
brfsscodebook <- read.csv('llcp2022codebook - Sheet1 (2).csv')
njresponses <- responses[responses$X_STATE == 34,]
njresponses <- njresponses %>%
  group_by(IDAY, IMONTH, IYEAR) %>%
  mutate(IDATE = as.Date(paste(IYEAR, IMONTH, IDAY,sep="-")))
brfsscodebook <- brfsscodebook[!grepl("-",  brfsscodebook$Value),]
write.csv(responses, "responses.csv", row.names=FALSE)
brfssmap <- brfsscodebook[,c('Variable_Name','Value','Value_Label')]
brfssmap[brfssmap == ''] <- NA
brfssmap <- brfssmap %>%
  group_by(Value_Label, Value) %>%
   pivot_wider(names_from='Variable_Name', values_from='Value_Label')
write.csv(brfssmap, "brfssmap.csv", row.names=FALSE)
for (var in intersect(names(njresponses),unique(brfssmap$Variable_Name))) {
  map <- brfssmap[brfssmap$Variable_Name == var, ]
  matching_indices <- match(njresponses[[var]], map$Value)
  
  # Only update njresponses if there's a matching index in map$Value
  valid_indices <- !is.na(matching_indices)
  njresponses[[var]][valid_indices] <- map$Value_Label[matching_indices[valid_indices]]
}
write.csv(njresponses, "njresponses.csv", row.names=FALSE)
njresponsesog <- responses[responses$X_STATE == 34,]
na_counts <- colSums(is.na(njresponses))/8209
njresponses <- njresponses[, na_counts <= 0.5]
missing_counts <- sapply(njresponses, function(col) sum(grepl('missing', col, ignore.case = TRUE)))/8209
njresponses <- njresponses[, missing_counts <= 0.5]
none_counts <- sapply(njresponses, function(col) sum(grepl('none', col, ignore.case = TRUE)))/8209
njresponses <- njresponses[, none_counts <= 0.5]
name_list = c("X_STATE", "X_URBSTAT", "X_AGEG5YR", "DISPCODE", "NUMADULT", "SEXVAR", "GENHLTH",
              "PHYSHLTH", "MENTHLTH", "POORHLTH", "PRIMINSR", "EXERANY2", "SLEPTIM1", "CVDINFR4",
              "CVDCRHD4", "CVDSTRK3", "ASTHMA3", "CNCRTYP2", "CHCCOPD3",
              "HAVARTH4", "ADDEPEV3", "CHCKDNY2", "DIABETE4", "LASTDEN4",
              "RMVTETH4", "MARITAL", "EDUCA", "RENTHOM1", "VETERAN3", "EMPLOY1", "CHILDREN",
              "INCOME3", "PREGNANT", "WEIGHT2", "HTIN4", "DEAF", "BLIND", "SMOKDAY2",
              "STOPSMK2", "AVEDRNK3", "DRNK3GE5", "HADMAM", "PSATEST1",
              "COLNSIGM", "HIVTST7", "HIVRISK5", "X_LLCPWT","X_TOTINDA", "IMONTH", "IDAY", 
              "IYEAR", "IDATE","CHECKUP1", "X_BMI5CAT","X_RACE1","COVIDPOS","COVIDVA1","COVIDNU1",
              "COVIDFS1","COVIDSE1","LSATISFY","EMTSUPRT","SDHISOLT", "ECIGNOW2","SDHSTRE1")
njresponses = njresponses[, name_list]
graphcolors <- c('#ADD8E6','#90EE90','#FFB6C1','#E6E6FA','#FFD700','#D3D3D3')
par(bg = "#FFFFF0")
par(mar=c(5,6,4,1)+.1)
mosaic_data <- table(njresponses[njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$GENHLTH != 'Donâ€™t know/Not Sure' & njresponses$LSATISFY != 'Refused' & njresponses$GENHLTH != 'Refused',]$LSATISFY,njresponses[njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$GENHLTH != 'Donâ€™t know/Not Sure' & njresponses$LSATISFY != 'Refused' & njresponses$GENHLTH != 'Refused',]$GENHLTH)
mosaic_data <- mosaic_data[rev(c('Very dissatisfied', 'Dissatisfied', 'Satisfied', 'Very satisfied')), c('Excellent', 'Very good', 'Good', 'Fair', 'Poor')]
mosaicplot(mosaic_data, xlab = 'In general, how satisfied are you with your life?', ylab ='Would you say that in general your health is:', main = "Mosaic of level of satisfaction in life vs level of General Health",col = graphcolors, border ='linen',las=2)

box_data <- njresponses[njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused' ,c("LSATISFY","POORHLTH")] 
box_data$LSATISFY <- factor(box_data$LSATISFY , levels=c("Very satisfied", "Satisfied", "Dissatisfied", "Very dissatisfied"))
box_data$POORHLTH <- as.numeric(box_data$POORHLTH)
boxplot(data = box_data, as.numeric(POORHLTH)~LSATISFY, xlab = 'In general, how satisfied are you with your life?', ylab = 'During the past 30 days, for about how many days did poor physical\n or mental health keep you from doing your usual activities?', main = 'Boxplot of reported level of satisfaction in life vs\n number of days experiencing poor health in the past 30 days', col = graphcolors, border = "black")

ggplot(responses[responses$MENTHLTH < 77 & responses$PHYSHLTH < 77,], aes(as.numeric(MENTHLTH), as.numeric(PHYSHLTH))) + stat_density2d(geom="tile", aes(fill = ..count..), contour = FALSE) + scale_fill_continuous(type = "viridis") + xlab('Now thinking about your mental health, which includes stress, depression, and problems with emotions,\n for how many days during the past 30 days was your mental health not good?') + ylab('Now thinking about your physical health, which includes physical illness and injury,\n for how many days during the past 30 days was your physical health not good?') +ggtitle('Density plot of number of days when mental health was not vs\n number of days when physical health was not good')

#heatmap_data <-tapply(as.numeric(njresponses[njresponses$MENTHLTH != 'Donâ€™t know/Not sure' & njresponses$MENTHLTH != 'Refused' & njresponses$MENTHLTH != 'None',]$MENTHLTH, na.rm=TRUE), njresponses[njresponses$MENTHLTH != 'Donâ€™t know/Not sure' & njresponses$MENTHLTH != 'Refused' & njresponses$MENTHLTH != 'None',]$IDATE, mean)
#heatmap_data <- as.matrix(heatmap_data)
#heatmap(heatmap_data, Colv = NA, Rowv = NA, scale="column")
box_data_new <- njresponses[njresponses$PHYSHLTH != 'Refused' & njresponses$PHYSHLTH != 'Donâ€™t know/Not Sure' & njresponses$SDHSTRE1 != 'Refused' & njresponses$SDHSTRE1 != 'Donâ€™t know/Not Sure',c("PHYSHLTH","SDHSTRE1")]
box_data_new$SDHSTRE1 <- factor(box_data_new$SDHSTRE1 , levels=c("Always", "Usually", "Sometimes", "Rarely", "Never"))
boxplot(data = box_data_new, as.numeric(PHYSHLTH)~SDHSTRE1, xlab = 'Stress means a situation in which a person feels tense, restless, nervous, or anxious, or is unable to sleep at night\nbecause his/her mind is troubled all the time. Within the last 30 days, how often have you felt this kind of stress?', ylab = 'Now thinking about your physical health, which includes physical illness and injury,\n for how many days during the past 30 days was your physical health not good?', main = 'Boxplot of reported frequency of stress in life in past 30 days vs\n number of days experiencing poor physical health in the past 30 days', col = graphcolors, border = "black")

table_for_graph <- table(njresponses[njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused',]$LSATISFY)
table_for_graph <- table_for_graph[rev(c('Very dissatisfied', 'Dissatisfied', 'Satisfied', 'Very satisfied'))]
barplot(table_for_graph, xlab = 'In general, how satisfied are you with your life?', ylab = 'Frequency', main = 'Distribution of people and their perceived satisfaction in life', col = graphcolors, border = 'black')

table_for_graph <- table(njresponses[njresponses$SDHSTRE1 != 'Donâ€™t know/Not Sure' & njresponses$SDHSTRE1 != 'Refused',]$SDHSTRE1)
table_for_graph <- table_for_graph[c('Always','Usually','Sometimes','Rarely','Never')]
barplot(table_for_graph, xlab = 'Stress means a situation in which a person feels tense, restless, nervous, or anxious, or is unable to sleep at night\n because his/her mind is troubled all the time. Within the last 30 days, how often have you felt this kind of stress?', ylab = 'Frequency', main = 'Distribution of people and the prevalence of stress in their lives', col = graphcolors, border = 'black')

table_for_graph <- table(njresponses[njresponses$SDHISOLT != 'Donâ€™t know/Not sure' & njresponses$SDHISOLT != 'Refused',]$SDHISOLT)
table_for_graph <- table_for_graph[c('Always','Usually','Sometimes','Rarely','Never')]
barplot(table_for_graph, xlab = 'How often do you feel socially isolated from others?  Is it…', ylab = 'Frequency', main = 'Distribution of people and how often they\n feel socially isolated from others', col = graphcolors, border = 'black')

mosaic_data <- table(njresponses[njresponses$SDHISOLT != 'Donâ€™t know/Not sure' & njresponses$SDHISOLT != 'Refused' & njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused',]$LSATISFY, njresponses[njresponses$SDHISOLT != 'Donâ€™t know/Not sure' & njresponses$SDHISOLT != 'Refused' & njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused',]$SDHISOLT)
mosaic_data <- mosaic_data[rev(c('Very dissatisfied', 'Dissatisfied', 'Satisfied', 'Very satisfied')), c('Always','Usually','Sometimes','Rarely','Never')]
mosaicplot(mosaic_data, xlab = 'In general, how satisfied are you with your life?', ylab ='How often do you feel socially\n isolated from others?  Is it…', main = "Mosaic of level of satisfaction in life \nvs how often one feels socially isolated",col = graphcolors, border ='linen',las=2)

chi_permutation_test(njresponses[njresponses$SDHISOLT != 'Donâ€™t know/Not sure' & njresponses$SDHISOLT != 'Refused' & njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused',], 'LSATISFY', 'SDHISOLT', 1000)
test <- chisq.test(mosaic_data)
z_test_from_data(box_data, 'LSATISFY', 'POORHLTH', 'Very satisfied', 'Satisfied')
z_test_from_data(box_data, 'LSATISFY', 'POORHLTH', 'Very satisfied', 'Dissatisfied')
z_test_from_data(box_data, 'LSATISFY', 'POORHLTH', 'Very satisfied', 'Very dissatisfied')
permutation_test(box_data, 'LSATISFY', 'POORHLTH', 1000, 'Very satisfied', 'Very dissatisfied')

box_data <- njresponses[njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused' & njresponses$SLEPTIM1 != 'Donâ€™t know/Not Sure' & njresponses$SLEPTIM1 != 'Refused',c("LSATISFY","SLEPTIM1")] 
box_data$SLEPTIM1 <- as.numeric(box_data$SLEPTIM1)
box_data$LSATISFY <- factor(box_data$LSATISFY , levels=c("Very satisfied", "Satisfied", "Dissatisfied", "Very dissatisfied"))
boxplot(data = box_data, as.numeric(SLEPTIM1)~LSATISFY, xlab = 'In general, how satisfied are you with your life?', ylab = 'On average, how many hours of sleep do you get in a 24-hour period?', main = 'Boxplot of reported level of satisfaction in life\n vs average # hours of sleep in 24 hours', col = graphcolors, border = "black")

mosaic_data <- table(njresponses[njresponses$EXERANY2 != 'Donâ€™t know/Not Sure' & njresponses$EXERANY2 != 'Refused' & njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused',]$LSATISFY, njresponses[njresponses$EXERANY2 != 'Donâ€™t know/Not Sure' & njresponses$EXERANY2 != 'Refused' & njresponses$LSATISFY != 'Donâ€™t know/Not sure' & njresponses$LSATISFY != 'Refused',]$EXERANY2)
mosaic_data <- mosaic_data[rev(c('Very dissatisfied', 'Dissatisfied', 'Satisfied', 'Very satisfied')), c('Yes','No')]
mosaicplot(mosaic_data, xlab = 'In general, how satisfied are you with your life?', ylab ='During the past month, other than your regular job, did you participate in any\n physical activities or exercises such as running, calisthenics, golf, gardening, or walking for exercise?', main = "Mosaic of level of satisfaction in life \nvs participating in physical activity in past 30 days",col = graphcolors, border ='linen',las=2)

z_test_from_data(box_data, 'LSATISFY', 'SLEPTIM1', 'Very satisfied', 'Satisfied')
z_test_from_data(box_data, 'LSATISFY', 'SLEPTIM1', 'Very satisfied', 'Dissatisfied')
z_test_from_data(box_data, 'LSATISFY', 'SLEPTIM1', 'Very satisfied', 'Very dissatisfied')
test <- chisq.test(mosaic_data)
