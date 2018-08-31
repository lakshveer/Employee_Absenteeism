getwd()

#Libraries imported
library(xlsx)
library(ggplot2)
library(gridExtra)
library(DMwR)
library(corrgram)
library(forecast)
library(outliers)
library(plyr)
library(tseries) #for stationarity
library(lmtest) #For testing autocorrelation

#importing employee absenteeism data set
complete_data = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1, header = T)

"As it's 3 years of data from July 2007 to July 2010(37 months) and we don't have a 'Year' variable in the dataset, 
including that variable will help in our seasonal analysis"

complete_data[1:113, 'Year'] = 2007
complete_data[114:358, 'Year'] = 2008
complete_data[359:570, 'Year'] = 2009
complete_data[571:740, 'Year'] = 2010

#Changing the position of the Year variable
complete_data = complete_data[,c(1,2,3,22,4:21)]

#Checking data type and structure
head(complete_data, 10); str(complete_data);
names(complete_data); summary(complete_data)

"
Distribution of month vs season:
1. Mid Dec - Mid March = Season 2
2. Mid March - Mid June = Season 3
3. Mid June - Mid Sep = Season 1
4. Mid Sep - Mid Dec = Season 4


We have 3 data points which are at bottom where monthofabsence is 0(which doesn't mean anything) we will remove these rows. The rows have reason of 
abscene 0 and absenteeism time 0 as well and uneven season pattern.

"
complete_data = complete_data[1:737,]

"We have a month of 1 in 2007 which is not possible as the data is given from July 2007, seems to be a typo error as
it is between month 10, missplaced as 1. We will replace it to 10
"

complete_data[67, 'Month.of.absence'] = 10

"
As we can see our data has all numeric variables, but we can see that few variables are category time,
so converting those variables into factors for our analysis
"

factor_variables = c('ID',"Reason.for.absence", "Month.of.absence","Day.of.the.week", "Seasons", "Disciplinary.failure",
                     "Education", "Son", "Social.drinker", "Social.smoker", "Pet")
for(i in factor_variables)
{
  complete_data[,i] = as.factor(complete_data[,i])
}

#Confirming the changes type
str(complete_data)






#*****************Completeness of data******************************

#Missing value Analysis
#Creating a data frame with missing value count
missing_val = data.frame(apply(complete_data,2,function(x){sum(is.na(x))}))
#Adding a columns with features and traget variable name
missing_val$Columns = row.names(missing_val)
#Renaming the missing value columns with 'missing_percentage' 
names(missing_val)[1] =  "missing_percentage"
#Calculating the percentage
missing_val$missing_percentage = (missing_val$missing_percentage/nrow(complete_data)) * 100
#Changing the position of 'missing_percentage'
missing_val = missing_val[order(-missing_val$missing_percentage),]
#Removing the first redundant column
row.names(missing_val) = NULL
#Changing the position of columns
missing_val = missing_val[,c(2,1)]
#Saving the missing percentages in a csv
write.csv(missing_val, "Miising_perc.csv", row.names = F)


ggplot(data = missing_val[1:10,], aes(x=reorder(Columns, -missing_percentage),y = missing_percentage))+
   geom_bar(stat = "identity",fill = "grey",size=25)+ xlab("Parameter")+
   ggtitle("Missing data percentage") + theme_bw()



"As we can see we don't have much missing values per column, the highest we have is 4.18% for Body.mass.index feature
    *So we will be imputing the missing values with the appropriate technique
    *Try trial and error method, to check when technique from Mean, median and Knn imputation works fine in our dataset
    *First imputing NA's for the values we already know and we will se which technique works best.

Imputing: row no BMI for row no 139 which is 23 with NA
          row no 194 and 707 for Absenteeism time which is 3 and 8 with NA
Mean Method:194:6.981844 , 707:6.981844, 139: 26.68, 139:25

Median Method: 194:3 , 707:3 ,139:25
kNN Imputation
194:1.58, 707: 7.274897
139:23.71572
"
"We are going with Knn imputation as the results of the manully imputated variables to NA seems to be closer to 
the actual values.
Mean gives 6.98 for all the values of Absenteeism.time.in.hours, which can increase our overall mean of the target variable
And median decreases.
The most appropriate seems to be Knn imputation in this case so freezing this.
"

#kNN Imputation
complete_data = knnImputation(complete_data, k = 3)


#Confirming there are no na's left
sum(is.na(complete_data))

#Structure of data after imputing
str(complete_data)

#Summary of data
summary(complete_data)

"Just having a glance at the data it looks like there are many outliers in the data set.
  Reasons: The abseenteeism.time.in.hour cannot be greater than 24 hours as it's a daily data, in some cases it is more
than 24 even 120 which is not possible.
  Reasons: Service.Time: Service time is the daily service hours of the persons, which cannot be more than 12 hours(keeping it as 24) but
I can see data points with 29 hours which is not possible.
  
"

#So lets perform outlier analysis first as visualizing the data will be better after this





#********************Outlier Analysis**********************

numeric_index = sapply(complete_data,is.numeric) #selecting only numeric

numeric_data = complete_data[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(complete_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of Absenteeism.time.in.hours for",cnames[i])))
}

# ## Plotting together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)
gridExtra::grid.arrange(gn11,ncol=2)


#As our data is less we will impute the outliers treating them as missing value with the help of KnnImputation
for(i in cnames)
{
  cnames
  val = complete_data[,i][complete_data[,i] %in% boxplot.stats(complete_data[,i])$out]
  print(length(val))
  complete_data[,i][complete_data[,i] %in% val] = NA
}

"
ID , Transportation.expense, Distance.from.Residence.to.Work
Service.time, Age, Work.load.Average.day., Hit.target, Weight, Height,Body.mass.index, Absenteeism.time.in.hours
[1] 3
[1] 0
[1] 5
[1] 8
[1] 31
[1] 19
[1] 0
[1] 119
[1] 0
[1] 44 
"


#As knnimputation takes only numeric variables into consideration, converting all the 
complete_data = knnImputation(complete_data, k = 3)

#Confirming that there is no missing value left
sum(is.na(complete_data))

#Checking the structure again
str(complete_data)

write.csv(complete_data, "CleanedData.csv", row.names = T)

##*********************Correlation Plot************************

corrgram(complete_data, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#Checking the correlation with churn using anova to finding the corrleation between the target continous and categorical features
#From the correlated variable

avo1 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Day.of.the.week)
avo2 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Seasons)
avo3 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Disciplinary.failure)
avo4 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Education)
avo5 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Son)
avo6 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Social.drinker)
avo7 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Social.smoker)
avo8 = aov(complete_data$Absenteeism.time.in.hours ~ complete_data$Pet)

#***************Exploring some important variables***********************
  
#Visualizing data
#Univariate analysis of important continuous variable
#Making the density plot
g1 = ggplot(complete_data, aes(x = Transportation.expense)) + geom_density()
g2 = ggplot(complete_data, aes(x = Distance.from.Residence.to.Work)) + geom_density()
g3 = ggplot(complete_data, aes(x = Age)) + geom_density()
g4 = ggplot(complete_data, aes(x = Service.time)) + geom_density()
g5 = ggplot(complete_data, aes(x = Work.load.Average.day.)) + geom_density()
g6 = ggplot(complete_data, aes(x = Hit.target)) + geom_density()
g7 = ggplot(complete_data, aes(x = Weight)) + geom_density()
g8 = ggplot(complete_data, aes(x = Height)) + geom_density()
g9 = ggplot(complete_data, aes(x = Body.mass.index)) + geom_density()
g10 = ggplot(complete_data, aes(x = Absenteeism.time.in.hours)) + geom_density()

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol=2)




#Exploring top 5 important features:
#Disciplinary failure, Reason of Absence, Social Drinker, Social Smoker and Son. 

aggre.absent.desciplinary_failure =  ddply(complete_data, c( "Disciplinary.failure"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

#Count for Reason of absence
g11 = ggplot(complete_data, aes(x = Reason.for.absence, fill = Reason.for.absence)) + geom_bar(stat = 'count')  +
  geom_label(stat='count',aes(label=..count..), size=5) + labs(ggtitle("Count for Reason of Absence")) +
  theme_grey(base_size = 18)
g11
#Reasons for absence according to day or week, seasons and month of absence
g12 = ggplot(complete_data, aes(x = Reason.for.absence, fill = Day.of.the.week)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Reason of absence based day of week"))+
theme_grey()
g12

g13 = ggplot(complete_data, aes(x = Reason.for.absence, fill = Seasons)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Reason of absence based on seasons of all the years"))+
  facet_grid(Year~.)
  theme_grey()
g13

g14 = ggplot(complete_data, aes(x = Reason.for.absence, fill = Month.of.absence)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..))+ labs(ggtitle("Count of reason for absence per year")) +
  facet_grid(Year~.) +
  theme_grey()
g14




#Aggregating the absentense hours based on ID
aggre.absent.ID =  ddply(complete_data, c( "ID"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))
#Aggregating the absendtense hours based on Reason of Absence and ID 
aggre.absent.id.reasons=  ddply(complete_data, c("ID", "Reason.for.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))



#Aggregating based on sons, smoker, drinker
aggre.absent.son=  ddply(complete_data, c("Son"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

aggre.absent.drinker=  ddply(complete_data, c("Social.drinker"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

aggre.absent.smoker =  ddply(complete_data, c("Social.smoker"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))



