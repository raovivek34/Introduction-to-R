#Dealing with matrices in R - Matrices unlike vectors are two dimensional and deal with rows and columns
x=matrix(c(1,2,5,6,1,9,7,4,3),byrow=TRUE,nrow=3,ncol=3) #Just a normal matrix - matrix can be considered to be just vector data filled in rows and columns either rowwise or columnwise
x #Matrix
y=matrix(c(1,2,3,4,5,4,3,2,1,2,3,4,5,6,7,8),byrow=TRUE,nrow=4,ncol=4) #Another matrix example
y
new_hope=c(460.998,314.4) #Star Wars - A New Hope box office collection in US and Non US (vector)
empire_strikes=c(290.475,247.900) #Star Wars - Empire Strikes Back collection in US and Non US (vector)
return_jedi=c(309.306,165.8) #Star Wars - Return of the Jedi collection in US and Non US (vector)
star_wars_original=matrix(c(new_hope,empire_strikes,return_jedi),byrow=TRUE,nrow=3,ncol=2) #Matrix star_wars_original realised as a combination of row vectors (vector datas) filled rowwise
star_wars_original
rownames(star_wars_original)=c('Star Wars - A New Hope','Star Wars - Empire Strikes Back','Star Wars - Return of the Jedi') #Giving names to the rows
colnames(star_wars_original)=c('US Box Office','Non US Box Office') #Giving names to the columns
star_wars_original #Named matrix 
worldwide_boxoffice=rowSums(star_wars_original) #Adds elements of rows and returns vector - gives worldwide box office of the three movies here.
worldwide_boxoffice #Worldwide Box Office of the three movies
trilogy_boxoffice=colSums(star_wars_original) #Adds elements of columns and returns vector
trilogy_boxoffice #Gives us combined US Box office of all three movies and combined Non Us box office
cbind(star_wars_original,worldwide_boxoffice) #Add columns to the matrix defined but does not change the matrix
rbind(star_wars_original,trilogy_boxoffice) #Add rows to the matrix defined but does not change the matrix
star_wars_original #Original Star Wars Trilogy along with it's US and non US box office collection
phantom_menace=c(474.5,552.5)
attack_clones=c(310.7,338.7)
revenge_sith=c(380.3,468.5)
star_wars_remake=matrix(c(phantom_menace,attack_clones,revenge_sith),byrow=TRUE,nrow=3,ncol=2)
star_wars_remake
rownames(star_wars_remake)=c('Star Wars - The Phantom Menace','Star Wars - Attack of the Clones','Star Wars - Revenge of the Sith')
colnames(star_wars_remake)=c('US Box Office','Non US Box Office')
star_wars_remake #Star Wars Remake Trilogy along with it's US and non US box office collection
all_star_wars=rbind(star_wars_original,star_wars_remake) #Row binding the two matrices
all_star_wars
worldwide_boxoffice=rowSums(all_star_wars)
worldwide_boxoffice
all_star_wars
all_star_wars[1,1:2] #all_star_wars is a matrix and it's elements can be retrieved by eg. all_star_wars[element of the row,element of the col] - if you want all rows and all columns - simply do not mention the elements
ticket_prices=5
all_star_wars_visitors=all_star_wars/ticket_prices
colnames(all_star_wars_visitors)=c('US audience','Non US audience')
all_star_wars
all_star_wars_visitors #visitors who came to watch star wars (in millions)
mean(all_star_wars_visitors[,1]) #Average US audience that watched Star Wars movies
