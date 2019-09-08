#Understanding factors in R - There are two types of variables - continuous variables that have infinite values and categorical variables that have finite values
#Categorical variables are of two types - 1. unordered (nominal) categorical variables and 2. ordered (ordinal) categorical variables 
#Factors are used to represent these categorical variables in R - unordered and ordered

#eg. Unordered categorical variables
gender=c('Male','Female','Male','Male','Female')
names(gender)=c('Data Analyst 1','Data Analyst 2','Data Analyst 3','Data Analyst 4','Data Analyst 5')
gender #Genders of 5 data analyst - named character vector
#However unordered categorical variables or infact categorical variables should be converted to factors.
factor_gender=factor(gender) #factor(unordered categorical variable) will give the same result but along with the levels which it will assign integer values from 1 depending on the name. 
factor_gender #Unordered Factor with 2 levels 'Female 'Male : 2 1 2 2 1 (Here it gives 'Female' value 1 and 'Male' value 2 since 'F' comes before 'M')
levels(factor_gender) #Gives the levels of the factor - Note : once the factor is made, levels cannot be changed drastically like it can be changed from levels=c('f','m) to c('female','male')
nlevels(factor_gender) #Gives the no. of levels of the factor

#eg. Ordered categorical variables
speed=c('slow','fast','medium','fast','medium')
names(speed)=c('Data Analyst 1','Data Analyst 2','Data Analyst 3','Data Analyst 4','Data Analyst 5')
speed #Speed of 5 data analysts - named character vector
factor_speed=factor(speed,order=TRUE,levels=c('slow','medium','fast')) #Factor of ordered categorical variables is factor(ordered categorical variable,order=TRUE,levels=c('slow','medium','fast')) - according to ascending order
factor_speed #Ordered factor with 3 levels 'slow'<'medium'<'fast' : 1 3 2 3 2
#Summary function
summary(factor_gender) #summary(factors) - gives how many levels have occurred how many times
summary(factor_speed)

x=c(1,2,3,4,5,6,7,8) #numeric vector
summary(x) #summary(numeric scalars or vectors or matrices) give min,max,quartile 1,quartile 2 (median or 50th percentile),quartile 3 and mean.

#Comparison of the elements of unordered factors in R is not possible 
factor_gender[1]>factor_gender[3]

#However comparison of elements of ordered factors in R is possible
result_data_analyst_speed=factor_speed[1]>factor_speed[3] #Checking if speed of data analyst 1 is better/greater than the speed of data analyst 3 
result_data_analyst_speed #Seeing the result
