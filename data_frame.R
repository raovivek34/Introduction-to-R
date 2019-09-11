#Understanding data frames in R - R programming language heavily involves the use of datasets (some of them are already included in the packages in R and some of them should be put externally)
#These datasets need to be cleaned, analyzed and visualized well to turn relevant data into information useful to make better business decisions.
#And these datasets sometimes are data frames with x observations on y variables (vectors) put columnwise.
#So initally we have say different vectors eg. Student Name, Marks scored out of 20, Age, Gender
student_name=c('Vivek Rao','Pratheep Kumar','Siddharth Singh','Raj Sanghavi','Rhea Dsouza','Kevin Fernandes','Riddhima Chaudhary')
marks_scored=c(20,16,14,14,16,16,16)
age=c(22,21,23,22,25,25,24)
gender=c('Male','Male','Male','Male','Female','Male','Female') #unordered categorical variable
factor_gender=factor(gender) #Unordered factor in R
factor_gender
student_info.df=data.frame(student_name,marks_scored,age,factor_gender,stringsAsFactors = FALSE) #Data frames are quite different from matrices in the sense that matrices have only data type in the result but data frames have more than one data type but every column of data frame which are vectors has one datatype.
student_info.df
names(student_info.df)=c('Student Name','Marks scored','Age','Gender') #Naming data frames
student_info.df
str(student_info.df) #Structure of the dataset - i.e dataframe with 7 observations on 4 variables and gives the variables i.e different vectors and their data types
student_info.df
student_info.df[c(1,3,5,7),c('Student Name','Marks scored')] #Retrieving elements from dataframe is same as retrieving elements from matrices - dealing with rows and columns
student_info.df$`Student Name` #Gives you the 'Student Nmae' variable along with values
#Subsetting a dataframe below --
subset(student_info.df,subset=gender=='Male') #subset() is used for subsetting data frame where there is a condition eg. subset=gender == 'Male' .Note : It is used for filtering out the dataframe based on some variable condition which is included in subset. Also for logical variables say answer is a logical variable even if you write subset=answer...it will filter and give you only TRUE values and filter out the rest.
age
#order() below --
order(age) #order() takes vector as argument and returns the position of every element in the vector
age[order(age)] #Arranges in ascending order
age[order(-age)] #Arranges in descending order
#Sorting out student_info.df in terms of ascending order of age -- 
order(student_info.df$Age) #Gives the position of every element in Age variable in the student_info,df
student_info.df[order(student_info.df$Age),]
#Playing with datasets 
mtcars #A dataset already present in R
head(mtcars,n=6) #Giving the head part of the data set i.e top part of the dataset with first 6 observations
tail(mtcars,n=6) #Giving the tail part of the data set i.e bottom part of the dataset with last 6 observations
str(mtcars) #Structure of the dataset - i.e dataframe with x observations on y variables and gives the variables i.e different vectors and their data types

