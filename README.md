# R-Program-for-Data-Pre-processing
#Import file from Dataset into R using the Import function in R

x <- ramen_ratings
#----------------------------------------------------------------------
#View file by renaming it as x for simplicity purposes
View(x)
#-----------------------------------------------------------------------
class(x) #View type of data frame
head(x) #View first 6 rows of data
str(x) #Display structure of data
#---------------------------------------------------------------------------------------------------
x2=subset(x,select = -c(1,4,7)) #Column 1(Review#), Column 4 (Style) and Column 7(Top Ten) had missing values and were removed
View(x2)
str(x2)
summary(x2)
ratings_mean <- tapply(x2$Stars,x2$Country,mean) #Get mean of Stars Rating for each Country

#-----------------------------------------------------------------------------------------------------------------------------
install.packages('DataExplorer')
library(DataExplorer)
library(dplyr)
library(ggplot2)
#-------------------------------------------------------------------------------

my_data <- as_tibble(x2) #Turn database into tibble

a <-prop.table(table(x2$Country))#frequency of country

df2 <- x2[order(x2$Country, decreasing = TRUE),]
View(df2)

test <- df2[-c(1:1469),] #Remove rows above Malaysia
test2 <-test[-c(157:1111),] #Only keep rows for Malaysia
View(test2)

ds <- test2[order(test2$Stars, decreasing = TRUE),] #Arrange column based on descending order for the Stars column
View(ds)

price_mean <- tapply(ds$Stars,ds$Brand, mean) #Get the mean data for each brand in Malaysia

plot_histogram(ds) #Plot histogram for dataset in Malaysia
plot_density(ds) #Plot density for dataset in Malaysia
barplot(table(ds$Stars), main="Stars", col=c("skyblue","red", "lightgreen"))

barplot(height = tapply(ds$Stars,ds$Brand, mean),main = "Average Rating for Malaysia", xlab = ds$Country)

#Now, removing rows from 95 to 114 to show brands with 4 stars and above:
dx <- ds %>% slice(-c(60:156))

#Plot Barplot for the best ramen brands in Malaysia
barplot(height = tapply(dx$Stars,dx$Brand, mean),main = "Brands with Best Ramen Ratings for Malaysia", xlab = ds$Country,col=c("skyblue"))

function(n){
  n=as.integer(readline("Please Enter Brand 1,2,3:"))
  if (n==1){
    n1<-subset(dx,dx$Stars==1)
    print(summary(n1))
  }else if (n==2){
    n2<-subset(dx,dx$Stars==2)
    print(summary(n2))
  }else if (n==3){
    n3<-subset(dx,dx$Stars==3)
    print(summary(n3))
  }else{
    print("Error!")
  }
}

#Compare the Brands by Stars
compare<- function(x,y){
  x=as.integer(readline("Please Enter Brand > x: "))
  y=as.integer(readline("Please enter Stars > y: "))
  Star_Brand <- subset(dx,dx$Brand>x & dx$Stars>y)
  boxplot(Stars~Brand, data=Star_Brand,
          main ="Different Stars vs Brand of Ramen",
          xlab = "Brand", ylab = "Stars", col = "Red")
}
