## loading the necessary libraries
library(ggplot2)
library(lubridate) # ymd_hms 
library(dplyr)
library(tidyr)
library(cowplot)
library(caTools)
library(MASS)
library(car)
library(e1071)
library(caret)
library(Information)
library(reshape2)
library(scales)
library(e1071)
library(ROCR)

##################################################################################################################################
#                                 Business Understanding
##################################################################################################################################

# Basically in large companies, employee count will affect the business drastically .
# If a new employee enters the organisation,there needs to be a initial understanding 
# about the business and internal logistics.
# Lot of KT and several trainings has to be given to the employees inorder to make them productive.
# If an employee leave the company, they need to be replaced with the talent pool available 
# in the job market and also the projects and their timelines will get delayed which leads 
# to a reputation loss among consumers and partners as well as the money.
# So, we are developing a model which will predict the probability of employee attrition, 
# which helps company to be proactive

##################################################################################################################################
#                                 Data Understanding
##################################################################################################################################


# Age gives  age of the employee	
# Attrition explains us Whether the employee left in the previous year or not	
# BusinessTravel explains	How frequently the employees travelled for business purposes in the last year	
# Department explains	Department in company	
# DistanceFromHome	gives info about Distance from home in kms	
# Education	contains four Levels	1 'Below College'2 'College'3 'Bachelor'4 'Master'5 'Doctor'
# EducationField	Field of education	
# EmployeeCount	 gives no info as it is single Employee data	
# EmployeeNumber unique id in the dataset	acts as a primary key	
# EnvironmentSatisfaction	explains Work Environment Satisfaction of employee contains four Levels	1 'Low'2 'Medium'3 'High'4 'Very High'
# Gender tells us the	Gender of employee	
# JobInvolvement explains about the	Job Involvement of the employee contains four Levels	1 'Low'2 'Medium'3 'High'4 'Very High'
# JobLevel gives the employee	Job level at company on a scale of 1 to 5	
# JobRole	gives the employee Name of job role in company	
# JobSatisfaction	explains the employee Job Satisfaction with four Levels	1 'Low'2 'Medium'3 'High'4 'Very High'
# MaritalStatus	gives Marital status of the employee	
# MonthlyIncome gives the employee	Monthly income in rupees per month	
# NumCompaniesWorked explains	Total number of companies the employee has worked for	
# Over18 gives no info as all the employees will be over 18	
# PercentSalaryHike gives the	Percent salary hike for last year	
# PerformanceRating gives the employee	Performance rating for last year with four levels	1 'Low'2 'Good'3 'Excellent'4 'Outstanding'
# RelationshipSatisfaction gives the employee	Relationship satisfaction contains four levels	1 'Low'2 'Medium'3 'High'4 'Very High'
# StandardHours gives	Standard hours of work for the employee	
# StockOptionLevel gives Stock option level of the employee	
# TotalWorkingYears gives	Total number of years the employee has worked so far	
# TrainingTimesLastYear explains	Number of times training was conducted for this employee last year	
# WorkLifeBalance explains	Work life balance of the employee with four levels	1 'Bad'2 'Good'3 'Better'4 'Best'
# YearsAtCompany gives	Total number of years spent at the company by the employee	
# YearsSinceLastPromotion	gives Number of years since last promotion	
# YearsWithCurrManager	gives Number of years under current manager
# In and out time dataset give the employee working hours and their leaves taken 



##################################################################################################################################
#                                Data Preparation
##############################################################################################################################
#Clearing the environment
rm(list = ls())

# load the datasets
in.time <- read.csv('in_time.csv',header = T,stringsAsFactors = F)
out.time <- read.csv('out_time.csv',header = T,stringsAsFactors = F)
emp.survey <- read.csv('employee_survey_data.csv',header = T,stringsAsFactors = F)
mgr.survey <- read.csv('manager_survey_data.csv', header = T,stringsAsFactors = F)
general <- read.csv('general_data.csv',header = T,stringsAsFactors = F)


# first we will check for the unique ids of the datasets
length(unique(in.time$X)) # 4410
length(unique(out.time$X)) # 4410
length(unique(emp.survey$EmployeeID)) # 4410
length(unique(mgr.survey$EmployeeID)) # 4410
length(unique(general$EmployeeID)) # 4410,
#EmployeeID is the primary key in all the tables 

## Checking if all employee IDs present in the three files match 

setdiff(general$EmployeeID,emp.survey$EmployeeID) # Identical employee ID across these datasets
setdiff(general$EmployeeID,mgr.survey$EmployeeID) 




##################################################################################################################################
#                                Deriving New Metrics from intime and outtime datasets
##################################################################################################################################
#we will be using in time and out time for forming new derived matrix 

# Assumption: 
# 1. we will consider clock-in as presence (0) and no-clock-in as absence (1)
# 2. difference between in time & out time is considered as hours worked in a day
# 3. public holiday: day where no employees had in-time data in in.time dataset
#leave pattern 

leave <- as.data.frame(ifelse(is.na(in.time[,-1]),1,0))

#removing the public holidays time entries

leave <- leave[,which(colSums(leave)!=4410)]

emp.leaves <- data.frame(Employee_ID = in.time$X , total_leaves = (rowSums(leave)))


#work hrs calculation 

identical(colnames(in.time$X), colnames(out.time$X)) # clock in & out datasets are similar

# exclude public holidays
in.time <- in.time[, which(sapply(in.time, function(x) sum(is.na(x))) !=4410)]
out.time <- out.time[, which(sapply(out.time, function(x) sum(is.na(x))) !=4410)]

in.time[,-1] <- sapply(in.time[,-1], ymd_hms) # character to datetime
out.time[,-1] <- sapply(out.time[,-1], ymd_hms) # character to datetime
work.hrs <- (out.time[,-1] - in.time[,-1])/3600 # total working hours calculation

# emp.hrs df to store employee working hours
# average working hours calculations

emp.hrs <- data.frame(EmployeeID=in.time$X, 
                      Avg.hrs = round(rowMeans(work.hrs, na.rm = T),2))

str(emp.hrs)
str(emp.leaves)
colnames(emp.leaves)[1]='EmployeeID'
merged_wh_leaves=merge(emp.hrs,emp.leaves,by='EmployeeID')


## Merging all the files to form a single final file
merge1 <- merge(x=emp.survey,y= general,by = "EmployeeID")
merge2 <- merge(x= merge1,y= mgr.survey,by = "EmployeeID")
final_df <- merge(x= merge2,y=merged_wh_leaves, by="EmployeeID")

##################################################################################################################################
#                                Data Cleaning
##################################################################################################################################

## Removing Unnecessary columns

employee_final <- final_df[,-c(12,19,21)]
str(employee_final)

## As for these three columns entire data is having a single value which doesn't give any information

##checking for NA values
sum(is.na(employee_final))
# 111 NA values
# checking for blank values
sapply(employee_final,function(x) length(which(x==" "))) 
# No blank values

# checking for missing values in dataset
sapply(employee_final,function(x) length(which(is.na(x))))
# 111 NA values are in 5 columns :EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,NumCompaniesWorked,TotalWorkingYears

#Total percentage of NA values present in each column
## 1. EnvironmentSatisfaction #25
sum(is.na(employee_final$EnvironmentSatisfaction))
sum(is.na(employee_final$EnvironmentSatisfaction))/nrow(employee_final)
# 0.56% missing values

## 2. JobSatisfaction
sum(is.na(employee_final$JobSatisfaction)) #20
sum(is.na(employee_final$JobSatisfaction))/nrow(employee_final)
# 0.45% missing values

## 3. WorkLifeBalance
sum(is.na(employee_final$WorkLifeBalance)) #38
sum(is.na(employee_final$WorkLifeBalance))/nrow(employee_final)
# 0.86% missing values

## 4. NumCompaniesWorked
sum(is.na(employee_final$NumCompaniesWorked)) #19
sum(is.na(employee_final$NumCompaniesWorked))/nrow(employee_final)
# 0.43% missing values

## 5. TotalWorkingYears
sum(is.na(employee_final$TotalWorkingYears)) #9
sum(is.na(employee_final$TotalWorkingYears))/nrow(employee_final)
# 0.20% missing values

summary(employee_final)
## Removing all the NA values as they are very less in number and dont affect anything
employee_final=employee_final[complete.cases(employee_final),]
sum(is.na(employee_final))
str(employee_final)

#-----------------------------------------------------------------------------------------------------------------
# making a copy of the combined data set from numeric to categorical which will help for dummy variable creation
str(employee_final)
employee_final$EnvironmentSatisfaction=if_else(employee_final$EnvironmentSatisfaction==1,'Low'
                       ,if_else(employee_final$EnvironmentSatisfaction==2,'Medium'
                                ,if_else(employee_final$EnvironmentSatisfaction==3,'High','Very High')))

employee_final$JobSatisfaction=if_else(employee_final$JobSatisfaction==1,'Low'
                                               ,if_else(employee_final$JobSatisfaction==2,'Medium'
                                                        ,if_else(employee_final$JobSatisfaction==3,'High','Very High')))
employee_final$WorkLifeBalance=if_else(employee_final$WorkLifeBalance==1,'Bad'
                                       ,if_else(employee_final$WorkLifeBalance==2,'Good'
                                                ,if_else(employee_final$WorkLifeBalance==3,'Better','Best')))

employee_final$Attrition =ifelse(employee_final$Attrition == "No",0,1)


employee_final$Education=if_else(employee_final$Education==1,'Below College'
                                       ,if_else(employee_final$Education==2,'College'
                                                ,if_else(employee_final$Education==3,'Bachelor'
                                                         ,if_else(employee_final$Education==4,'Master','Docter'))))

employee_final$JobLevel=if_else(employee_final$JobLevel==1,'very low'
                                 ,if_else(employee_final$JobLevel==2,'low'
                                          ,if_else(employee_final$JobLevel==3,'medium'
                                                   ,if_else(employee_final$JobLevel==4,'high','very high'))))

employee_final$StockOptionLevel=if_else(employee_final$StockOptionLevel==0,'Zero'
                                 ,if_else(employee_final$Education==1,'One'
                                          ,if_else(employee_final$Education==2,'Two','Three')))

employee_final$PerformanceRating=if_else(employee_final$PerformanceRating==3,'Three','Four')
employee_final$JobInvolvement=if_else(employee_final$JobInvolvement==1,'low'
                                      ,if_else(employee_final$JobInvolvement==2,'Medium'
                                               ,if_else(employee_final$JobInvolvement==3,'High','Very High')))

employee_final$TrainingTimesLastYear=if_else(employee_final$TrainingTimesLastYear==0,'Zero'
                                          ,if_else(employee_final$TrainingTimesLastYear==1,'One'
                                                   ,if_else(employee_final$TrainingTimesLastYear==2,'Two'
                                                            ,if_else(employee_final$TrainingTimesLastYear==3,'Three'
                                                                     ,if_else(employee_final$TrainingTimesLastYear==4,'Four',
                                                                              if_else(employee_final$TrainingTimesLastYear==5,'Five','Six'))))))


employee_final$TotalWorkingYears=if_else(employee_final$NumCompaniesWorked ==0
                                         ,employee_final$YearsAtCompany,
                                         employee_final$TotalWorkingYears)

##As it was 1 year higher for zero companies worked
str(employee_final)

####################################################################################
##   Exploratory Data Analysis - Barcharts for categorical features    ##
####################################################################################

# Plot showing the attrition percentage in the entire dataset
ggplot(employee_final,aes(factor(Attrition),fill = factor(Attrition)))+geom_bar()+
  geom_text(stat="count",aes(label = scales::percent((..count..)/sum(..count..))),vjust=-0.2)

#--------------------------------------UNIVARIATE PLOTS-----------------------------------------------
# Creating a generalised function for  Univariate plots
univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
    ) 
}

##lets analyse the distribution for each categorical variable

univariate_categorical(employee_final,employee_final$EnvironmentSatisfaction,"ENvironment Satisfaction Distribution")
univariate_categorical(employee_final,employee_final$JobSatisfaction,"Job satisfaction Distribution")
univariate_categorical(employee_final,employee_final$WorkLifeBalance,"Work life Balance Distribution")
univariate_categorical(employee_final,employee_final$Attrition,"Attrnatiom Distribution")
univariate_categorical(employee_final,employee_final$BusinessTravel,"Buisness Tarvel Distribution")
univariate_categorical(employee_final,employee_final$Department,"Department Distribution")
univariate_categorical(employee_final,employee_final$Education,"Education Distribution")
univariate_categorical(employee_final,employee_final$EducationField,"Education Field Distribution")
univariate_categorical(employee_final,employee_final$Gender,"Geneder Distribution")
univariate_categorical(employee_final,employee_final$JobLevel,"Job Level Distribution")
univariate_categorical(employee_final,employee_final$JobRole,"Job Role Distribution")
univariate_categorical(employee_final,employee_final$MaritalStatus,"Marital Distribution")
univariate_categorical(employee_final,employee_final$NumCompaniesWorked,"Number of companies work Distribution")
univariate_categorical(employee_final,employee_final$TrainingTimesLastYear,"Training time last year")
univariate_categorical(employee_final,employee_final$JobInvolvement,"Job Invovlvment distribution")
univariate_categorical(employee_final,employee_final$PerformanceRating,"Performance rating Distribution")

#--------------------------------------BIVARIATE PLOTS-----------------------------------------------
# Creating a generalised function for  Bivariate plots (Categorical plots)
bivariate_categorical_plot = function(var1, var_name)
{
  ggplot(employee_final,aes(x=factor(var1),fill=factor(Attrition)))+geom_bar(position = "fill")  + ggtitle(var_name)
  
  
}

bivariate_categorical_plot(employee_final$EnvironmentSatisfaction, "EnvironmentSatisfaction vs Attrition")
#we can see for low environment satisfaction, attrition is higher compared to others

bivariate_categorical_plot(employee_final$JobSatisfaction, "Job Satisfaction vs Attrition")
#We can see for low job satisfaction attrition is higher compared to high which hass less attrition

bivariate_categorical_plot(employee_final$WorkLifeBalance, "Work Life Balance vs Attrition")
#for bad work life balance, attrition is higher

bivariate_categorical_plot(employee_final$BusinessTravel, "Buisness Travel vs Attrition")
#those who tarvel frequenctly has more chance of attrition

bivariate_categorical_plot(employee_final$Department, "Department vs Attrition")
#human resouses has more attrition

bivariate_categorical_plot(employee_final$Education, "Education vs Attrition")
#almost all level of education except college has same attrition
bivariate_categorical_plot(employee_final$EducationField, "Educational Field vs Attrition")
##education fiedl 'Human resourse has highest attrition
bivariate_categorical_plot(employee_final$Gender, "Gender vs Attrition")
##same level of attrition
bivariate_categorical_plot(employee_final$JobLevel, "Job Level vs Attrition")
##almost all job level has same lavel of attrition
bivariate_categorical_plot(employee_final$JobRole, "Job Role vs Attrition")
##research scientist and research director has more attrition
bivariate_categorical_plot(employee_final$MaritalStatus, "Marital Status vs Attrition")
##single status has more attrition
bivariate_categorical_plot(employee_final$NumCompaniesWorked, "Number of Companies worked vs Attrition")
## the person who has work for 5,6,7,9 has more chance to leave the company while who has worked for 1 company also compared to others
bivariate_categorical_plot(employee_final$StockOptionLevel, "Stock Option Level vs Attrition")
##same for both levels
bivariate_categorical_plot(employee_final$TrainingTimesLastYear, "TrainingTimesLastYear vs Attrition")
##those who have be trained 6 times are very less likely to attrinate
bivariate_categorical_plot(employee_final$JobInvolvement,"Job Involvement vs Attrition")
#3low job involvemnet has high attrition compared to others
bivariate_categorical_plot(employee_final$PerformanceRating, "Performance Rating vs Attrition")
##Performance has not any sigificat pattern with attrition


###Now lets focus on numeric variables
# Creating a generalised function for  Bivariate plots(numeric variables)

bivariate_numeric_categorical_plot = function(var1, var_name)
{
  ggplot(employee_final,aes(x=var1,color=factor(Attrition)))+geom_histogram(binwidth = 5) + ggtitle(var_name)
  
}
bivariate_numeric_categorical_plot(employee_final$Age, "Age Vs Attrition")

bivariate_numeric_categorical_plot(employee_final$DistanceFromHome, "Distance From Home vs Attrition")
bivariate_numeric_categorical_plot(employee_final$PercentSalaryHike, "Percent Salary Hike Vs Attrition")
bivariate_numeric_categorical_plot(employee_final$TotalWorkingYears, "Total Working Years Vs Attrition")

bivariate_numeric_categorical_plot(employee_final$YearsAtCompany, "YearsAtCompany Vs Attrition")

bivariate_numeric_categorical_plot(employee_final$total_leaves, "Total Leaves Vs Attrition")

bivariate_categorical_plot(employee_final$PercentSalaryHike, "PercentSalaryHike Vs Attrition")

bivariate_numeric_categorical_plot(employee_final$YearsSinceLastPromotion, "YearsSinceLastPromotion Vs Attrition")


## Creating a generalised function for Multi-Variate Plots
bivariate_numeric_numeric_categorical_plot = function(var1,var2, var_name)
{
  ggplot(employee_final,aes(x=var1,y=var2,color=factor(Attrition)))+geom_point() + ggtitle(var_name)
  
}
bivariate_numeric_numeric_categorical_plot(employee_final$Age,employee_final$MonthlyIncome, "Monthly Income vs Age")

##we can see that less age and less income are more likely to leave compared to less age and high income
##high age and high income less likely to leave
##bivariate_numeric_numeric_categorical_plot(employee_final$YearsSinceLastPromotion,employee_final$YearsWithCurrManager)

########################################################################################################################################################################
#---------------------------------------------Correlation Matrix-------------------------------------------------------------------------------------------
########################################################################################################################################################################
numeric_employee_df=employee_final[sapply(employee_final,is.numeric)]

cormat <- round(cor(numeric_employee_df),2)
head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="HR Analytics\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

########################################################################################################################################################################
#---------------------------------------------Outlier Treatment------------------------------------------------------------
########################################################################################################################################################################
sapply(employee_final[,c(5,16,9,20,22:24,27)],function(x) boxplot.stats(x)$out)

# there are outliers in monthly income & Avg.hrs.

## Outlier Treatment ##

iv <- create_infotables(numeric_employee_df, y="Attrition")
head(iv$Summary)

# there are outliers in monthly income & Avg.hrs
#woe transformation for Avg.hrs

employee_final$Avg.hrs <- ifelse(employee_final$Avg.hrs >=5.95 & employee_final$Avg.hrs <=6.19, iv$Tables[['Avg.hrs']]['WOE'][[1]][1], 
                     ifelse(employee_final$Avg.hrs >=6.2 & employee_final$Avg.hrs<=6.5, iv$Tables[['Avg.hrs']]['WOE'][[1]][2], 
                            ifelse(employee_final$Avg.hrs >=6.51 & employee_final$Avg.hrs<=6.79, iv$Tables[['Avg.hrs']]['WOE'][[1]][3],
                                   ifelse(employee_final$Avg.hrs>=6.8 & employee_final$Avg.hrs<=7.1, iv$Tables[['Avg.hrs']]['WOE'][[1]][4],
                                          ifelse(employee_final$Avg.hrs >=7.11 & employee_final$Avg.hrs<=7.4, iv$Tables[['Avg.hrs']]['WOE'][[1]][5],
                                                 ifelse(employee_final$Avg.hrs >=7.41 & employee_final$Avg.hrs<=7.69, iv$Tables[['Avg.hrs']]['WOE'][[1]][6],
                                                        ifelse(employee_final$Avg.hrs >=7.7 & employee_final$Avg.hrs<=7.99, iv$Tables[['Avg.hrs']]['WOE'][[1]][7],
                                                               ifelse(employee_final$Avg.hrs >=8 & employee_final$Avg.hrs<=8.87, iv$Tables[['Avg.hrs']]['WOE'][[1]][8],
                                                                      ifelse(employee_final$Avg.hrs >=8.88 & employee_final$Avg.hrs<=9.99, iv$Tables[['Avg.hrs']]['WOE'][[1]][9],
                                                                             ifelse(employee_final$Avg.hrs >=10 & employee_final$Avg.hrs<=11.03, iv$Tables[['Avg.hrs']]['WOE'][[1]][10], NA))))))))))


## the range of values in the numeric variables is small except for monthly income & Avg hours,so no point of treating the outliers as it 
# wont affect the model

##########################################################################################################################################
#                                           Standardising/Scaling For model Making
##########################################################################################################################################


employee_final[,-c(2:4,6,7:8,10:15,19,21,25,26)] <- as.data.frame(scale(employee_final[,-c(2:4,6,7:8,10:15,19,21,25,26)]))

# creating a dataframe of categorical features
employee_final_char<- employee_final[,c(2:4,6,7:8,10:15,19,21,25,26)]
##########################################################################################################################################
#                                   Dummy Variable creation 
##########################################################################################################################################
# converting categorical attributes to factor
employee_dummy <- data.frame(sapply(employee_final_char, function(x) factor(x)))
str(employee_dummy)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_dummy, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_dummy))[,-1]))
# for gender male is 1 and female is 0
employee_final$Gender <- ifelse(employee_final$Gender=='Male', 1, 0)
# Final dataset
emp_model<- cbind(employee_final[,-c(1,2:4,6,7:8,10:15,19,21,25,26)],dummies) # removed the ID field: EmployeeID
View(emp_model) # 4300 obs. of  63 variables
str(emp_model)
##########################################################################################################################################
#-------------------------------------------------------  MODEL BUILDING  ------------------------------------------------------------
##########################################################################################################################################

# Dividing the dataset to train and test dataset
set.seed(100)

indices = sample.split(emp_model$Attrition, SplitRatio = 0.7)

train = emp_model[indices,]

test = emp_model[!(indices),]


####################################################################################################################################################################
#                                              Logistic Regression model
##########################################################################################################################################################

#Initial model
model_1 = glm(Attrition~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

sort(vif(model_2), decreasing  = T)


##removing Jobinvolbvment xmedium based on pvalue
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + Education.xDocter + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel + 
                 TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                 TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + 
                 JobInvolvement.xlow + JobInvolvement.xVery.High,family = "binomial", data = train)
 
summary(model_3)

# Removing multicollinearity through VIF check
sort(vif(model_3), decreasing  = T)

#removing jobinvolvment.xlow based on p value
model_4 <-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xCollege + Education.xDocter + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
                family = "binomial", data = train)
summary(model_4)
sort(vif(model_4), decreasing  = T)

#MaritalStatus.xMarried removed based on high vif and p value 

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + Education.xDocter + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                 TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                 TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + 
                 JobInvolvement.xMedium + JobInvolvement.xVery.High ,
               family = "binomial", data = train)
summary(model_5)
sort(vif(model_5), decreasing  = T)


#removing EducationField.xmedical based on P-value

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + Education.xDocter + EducationField.xMarketing + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                 TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                 TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + 
                 JobInvolvement.xMedium + JobInvolvement.xVery.High ,
               family = "binomial", data = train)

sort(vif(model_6), decreasing  = T)
summary(model_6)

##removing job involvment x mediumbased on p value

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + Education.xDocter + EducationField.xMarketing + 
                 EducationField.xOther+EducationField.xTechnical.Degree + 
                 JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                 TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                 TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
               family = "binomial", data = train)

sort(vif(model_7), decreasing  = T)
summary(model_7)


##removing job role x researchscientist
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
      YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
      EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
      JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
      WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
      BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
      Department.xSales + Education.xCollege + Education.xDocter + EducationField.xMarketing + 
      EducationField.xOther+EducationField.xTechnical.Degree + 
      JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
      JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
       JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
      TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
      TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
    family = "binomial", data = train)

sort(vif(model_8), decreasing  = T)
summary(model_8)



#Removing education field x marketing as it is insignificant

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xCollege + Education.xDocter +
                 EducationField.xOther+EducationField.xTechnical.Degree + 
                 JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                 TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                 TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
               family = "binomial", data = train)

sort(vif(model_9), decreasing  = T)
summary(model_9)




# Removing EducationField xOther based on p value

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xCollege + Education.xDocter +
                  EducationField.xTechnical.Degree + JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
                family = "binomial", data = train)

sort(vif(model_10), decreasing  = T)
summary(model_10)




#Removing education field xtechnical degree P value

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xCollege + Education.xDocter +
                  JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
                family = "binomial", data = train)
sort(vif(model_11), decreasing  = T)
summary(model_11)



#Revoming Educationx college based on p value

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xDocter +
                  JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero + JobInvolvement.xVery.High ,
                family = "binomial", data = train)


sort(vif(model_12), decreasing  = T)
summary(model_12)




#Removing Job involvment x very high based on p value
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xDocter +
                  JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources + 
                  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)
sort(vif(model_13), decreasing  = T)
summary(model_13)





#Removing job role xmanager based on P value
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xDocter +
                  JobLevel.xlow + JobLevel.xvery.high + JobRole.xHuman.Resources  
                  + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)
sort(vif(model_14), decreasing  = T)
summary(model_14)



#JobRole.xHuman resources based on P value
model_15 <-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.xDocter +
                 JobLevel.xlow + JobLevel.xvery.high 
               + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                 TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                 TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
               family = "binomial", data = train)

sort(vif(model_15), decreasing  = T)
summary(model_15)



#Removing job level x very high based on p value
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xDocter +
                  JobLevel.xlow + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + StockOptionLevel + 
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)

sort(vif(model_16), decreasing  = T)
summary(model_16)



#Removing Stock option level based on p[ value]
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + Education.xDocter +
                  JobLevel.xlow + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle +  
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)

sort(vif(model_17), decreasing  = T)
summary(model_17)



#Removing buisness travel rarely based on vif and p value(single*)
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                  + Department.xResearch...Development + Department.xSales + Education.xDocter +
                  JobLevel.xlow + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle +  
                  TrainingTimesLastYear.xSix + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)

sort(vif(model_18), decreasing  = T)
summary(model_18)



# Train time last year six removed based on single * p value
model_19 <-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                   EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                   JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                   WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + Department.xSales + Education.xDocter +
                   JobLevel.xlow + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xSales.Executive  + MaritalStatus.xSingle + TrainingTimesLastYear.xThree + 
                   TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                 family = "binomial", data = train)


sort(vif(model_19), decreasing  = T)
summary(model_19)

#Education.xdocter may be removed based on p value  
model_20 <-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                   EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                   JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                   WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Department.xSales +
                   JobLevel.xlow + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xSales.Executive  + MaritalStatus.xSingle + TrainingTimesLastYear.xThree + 
                   TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                 family = "binomial", data = train)

sort(vif(model_20), decreasing  = T)
summary(model_20)

#Environment satisfaction high  removed based on p value
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + Department.xResearch...Development +
                  Department.xSales +JobLevel.xlow + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle + TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)

sort(vif(model_21), decreasing  = T)
summary(model_21)



#JobLevel Low can be removed based on P value
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow +  JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Department.xSales + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle +  TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo + TrainingTimesLastYear.xZero,
                family = "binomial", data = train)

sort(vif(model_22), decreasing  = T)
summary(model_22)



#training time last year zero can be removed based on P value(2*)
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + JobSatisfaction.xLow +
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Department.xSales +
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive  + MaritalStatus.xSingle +  
                 TrainingTimesLastYear.xThree + 
                  TrainingTimesLastYear.xTwo ,
                family = "binomial", data = train)

sort(vif(model_23), decreasing  = T)
summary(model_23)



#JobRole.xSales.Executive can be removed based on single * P value
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                               YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                                JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                               WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                                Department.xResearch...Development + Department.xSales +
                                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                               MaritalStatus.xSingle + TrainingTimesLastYear.xThree +TrainingTimesLastYear.xTwo ,
                             family = "binomial", data = train)

sort(vif(model_24), decreasing  = T)
summary(model_24)


#JobRole.xResearch.Director based on single * p value
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                              YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                               JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                              WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                              Department.xSales + JobRole.xManufacturing.Director + 
                             MaritalStatus.xSingle +TrainingTimesLastYear.xThree +TrainingTimesLastYear.xTwo ,
                            family = "binomial", data = train)

sort(vif(model_25), decreasing  = T)
summary(model_25)



#Training time last year 2 has been removed based on Double *
model_26 <-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                    JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                   WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                    Department.xResearch...Development + Department.xSales + JobRole.xManufacturing.Director + 
                    MaritalStatus.xSingle +TrainingTimesLastYear.xThree ,
                 family = "binomial", data = train)



sort(vif(model_26), decreasing  = T)
summary(model_26)



#TrainingTimesLastYear.xThree  has been removed on P value
model_27 <-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                   JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                   WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                   Department.xResearch...Development + Department.xSales +
                   JobRole.xManufacturing.Director + 
                   MaritalStatus.xSingle,
                 family = "binomial", data = train)

sort(vif(model_27), decreasing  = T)
summary(model_27)



#Department.xSales can checked if The step AiC increases so much

model_28 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development +
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle,
                family = "binomial", data = train)

sort(vif(model_28), decreasing  = T)
summary(model_28)


#Department.xResearch...Development can be removed based on p value


model_29 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle,family = "binomial", data = train)

sort(vif(model_29), decreasing  = T)
summary(model_29)

#JobRole.xManufacturing.Director can be tested if AiC increases significantly

model_30 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg.hrs + EnvironmentSatisfaction.xLow + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood + BusinessTravel.xTravel_Frequently  + MaritalStatus.xSingle,
                  family = "binomial", data = train)

sort(vif(model_30), decreasing  = T)
summary(model_30)

##########################################################################################################################################
###                                                         Model Evaluation
##########################################################################################################################################
### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(model_30, type = "response", 
                    newdata = test[,-21])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

#######################################################################
# Lets use the probability cutoff of 40%
test_pred_attrit <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
table(test_pred_attrit, test_actual_attrition)

# Going for Optimal cut off value as 50% n 40% cut offs are not predicting properly
####################################################################################################################
##                           Optimal cut off value                                                              ##
####################################################################################################################

# Let's find out the optimal probalility cutoff 
# helper function to seperate model performance data from confusion matrix

populateCMdata <- function(cutoff)  {
  predicted.attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted.attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) #transpose the matrix
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.0004435 to 0.8711025 for plotting and initiallizing a matrix 
# of 100 X 3.
# Summary of test probability
summary(test_pred)

cutoff.data = seq(.01,.80,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = populateCMdata(cutoff.data[i])
} 
(cutoff <- cutoff.data[which(abs(cmdata[,1]-cmdata[,2]) < 0.01)])
plot(cutoff.data, cmdata[,1], xlab="Cutoff", ylab="Value", cex.lab=1,
     cex.axis=1, ylim=c(0,1), type="l", lwd=2, axes=FALSE, col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff.data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff.data,cmdata[,3],col=4,lwd=2)
box()
legend(0.10,0.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))
text(0.24, 0.3, labels=sprintf("cutoff value: %0.7f", cutoff))

# Let's choose a cutoff value of 0.1775758 for final model
test_cutoff_attrition <- factor(ifelse(test_pred >= 0.1775758, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc 
sens 
spec 
View(test)

########################################
### KS -statistic - Test Data ######
########################################
library(ROCR)

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, measure = "tpr", x.measure = "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

################################################################################
### Plot Receiver Operating Characteristics (ROC) Curve: AUC calculation ######
################################################################################
plot(performance_measures_test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred_object_test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value))


########################################################################################################################
##                                                Lift & Gain Chart                                                   ##
#######################################################################################################################
# helper function to calculate gain & lift
calcLift <- function(labels , predicted_prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
attrition_decile  <-  calcLift(test_actual_attrition, test_pred, groups = 10)

######## plot the lift chart #######

plot(attrition_decile$Cumlift, type="l", lwd=2, col="red",# lty=4,
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

######### Plot Gain Chart #########

attrition_decile  <-  calcLift(test_actual_attrition, test_pred, groups = 10)

gain <- c(0,attrition_decile$Gain)
Deciles <- c(0,attrition_decile$bucket)
plot(y=gain,x=Deciles,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

Random_Gain <- seq(from=0,to=100,by=10)
lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="red")

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="darkgreen")


#####################################################################################################
#                                              CONCLUSIONS
#####################################################################################################
#Our Logistic Regression model is able to predict with 77% accuracy.
#Sensitivity is more important to us.
#Sensitivity  of our model is 77% i.e. it correctly identifies 77% of employees which are likely to leave the company.
#Specificity of our model is 77% i.e. it correctly identifies 77% of employees which are not likely to leave the company.
#KS Statistics for the model is 54% which indicates our model is fairly good and accurate.

#   Recommendations are as follows:
#1. Company should takes some actions to improve work life balance of employees.
#2. Working environment should be improved. Employees should get regular breaks. Quizes and indoor games should be promoted.
#3. Managers should not be changed very frequently.
#4. For Frequently traveling employees, company should take some steps to keep them in our company.

