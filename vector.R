#Introduction to R - R Basics that will cover Scalars and Vectors
a=2 #Scalars
b=2.12
c=1+3i
d=TRUE
e='Hey wassup'
a
b
c
d
e
class(a)
class(b)
class(c)
class(d)
class(e) #Covering R dataypes here by finding the datatype of the variable with class() - numeric,complex,logical,character datatypes
f=c(1,'rad',1+3i,TRUE) #Vectors - Vectors can combine any datatypes using c()-i.e combine function but finally return a single datatype vector eg. either character vector or complex vector or numeric vector or logical vector
f #character vector
class(f)
g=c(1,1+3i,TRUE) #complex vector
g
class(g)
h=c(1,TRUE) #numeric vector
h
class(h)
i=c(TRUE,FALSE,TRUE) #logical vector
i
class(i)
j=c(1,2,3,4,5) #numeric vector
j
names(j)=c('val1','val2','val3','val4','val5') #Assigning names or identity to each data in vector
j #Named numeric vector
j[1] #Retrieving first element from named numeric vector
j['val1'] #Another way of retrieving first element from named numeric vector
j[1:4]
j[c(1,4:5)] #Retrieving first,fourth and fifth elements from vector
j[j>3] #Will give me only elements that satisfy condition i.e j>3 and will reject rest of the elements