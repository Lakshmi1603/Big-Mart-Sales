library(car, caret, data.table, ggplot2, corrplot, cowplot, dplyr, xgboost)
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)
library(cowplot)
library(xgboost)
library(data.table)
library(car)

setwd("C:/Users/shree/Downloads/BIG MART SALES_AV")
train <- read.csv("C:/Users/shree/Downloads/BIG MART SALES_AV/Train_UWu5bXk.csv")
>   View(train)
test <- read.csv("C:/Users/shree/Downloads/BIG MART SALES_AV/Test_u94Q5KV.csv")
>   View(test)
#lets combine both the datasets.
#first we add a new column in test data.


? ":="
  is.data.table(test)
test<-data.table(test)  
train<-data.table(train)
is.data.table(test)
is.data.table(train)
test[ , Item_Outlet_Sales := NA]
names(test)
combi<- rbind(test,train)

#Lets plot train data using histogram
p0= ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill="green") + xlab("Item_Outlet_Sales")

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "red") + xlab("Item_Weight")
p2 = ggplot(combi) +geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") + xlab(("Item_Visibility"))
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill="blue") 
plot_grid(p1,p2,p3, nrow = 1)

#Now lets plot categorical variables
ggplot(combi %>% group_by(Item_Fat_Content)%>%summarise(count=n())) + geom_bar(aes(Item_Fat_Content, count), stat = "Identity", fill = "Coral1")
#As we can see, LF, low fat and Low fAt can all be combined into 1 category. same for reg and Regular
combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "Low Fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

#Now lets plot the same again
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(count = n())) + geom_bar(aes(Item_Fat_Content, count), stat = "Identity", fill = "Coral1")

#Lets also check other variables
#Plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(count = n())) + 
      geom_bar(aes(Item_Type, count), stat = "Identity", fill = "Coral1") + xlab("") + 
      geom_label(aes(Item_Type, count, label= count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle("Item_Type")

#Plot for Outlet_Identifier
p5 =  ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(count = n())) +
     geom_bar(aes(Outlet_Identifier, count), stat = "Identity", fill = "Coral1") + xlab("") +
     geom_label(aes(Outlet_Identifier, count, label = count), vjust = 0.5) + theme(axis.title.x = element_text(angle = 45, hjust = 1)) +
     ggtitle("Outlet_Identifier")

#Plot for Outlet_size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarize (count = n())) + 
      geom_bar(aes(Outlet_Size, count), stat = "Identity", fill = "Coral1") + xlab("") + 
        geom_label(aes(Outlet_Size, count,label = count), vjust = 0.5) + theme(axis.title.x = element_text(angle = 45, hjust = 1)) +
         ggtitle("Outlet_Size")
class(combi$Outlet_Size)
#There are 4016 Missing/Nas which can be treated in the Bivariate Analysis.

#For now, lets continue to check all the other variables
 #Plot for Outlet_Establishment_Year
class(combi$Outlet_Establishment_Year)
#Since it is not a factor variable, we will have to specify in aes as a factor.
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(count = n())) +
    geom_bar(aes(factor(Outlet_Establishment_Year), count), stat = "Identity", fill = "Coral1") + xlab("") +
    geom_label(aes(factor(Outlet_Establishment_Year), count, label = count), vjust = 0.5) + theme(axis.title.x = element_text(size = 8.5)) +
    ggtitle("Outlet_Establishment_Year")

#Plot for Outlet_type
class(combi$Outlet_Type)

p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarize(count = n())) +
  geom_bar(aes(Outlet_Type, count), stat = "Identity", fill = "Coral1") + xlab("") +
  geom_label(aes(Outlet_Type, count, label = count), vjust = 0.5) + theme(axis.title.x = element_text(size = 8.5)) + 
  ggtitle("Outlet_Type")

  #to plot both p7 & p8 together
plot_grid(p7,p8, ncol = 1)
 
#Bivariate Analysis: Independent variables vs target variable.
#Extractign train data from combi data.

str(combi)
#Lets explore the numerical variables first vs target variable(Item_Outlet_Sales).

#Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) +
 geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
theme(axis.title = element_text(size = 8))

#Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) +
 geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8))
  

#Item_MRP vs Item_Outlet_Sales
p11 =  ggplot(train) + 
    geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
    theme(axis.title = element_text(size = 8))

  second_row_2 = plot_grid(p10, p11, ncol = 2)    
plot_grid(p9, second_row_2, nrow = 2)

#Now plotting CAtegorical variables vs the target variable

#Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) +
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.title.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8))

#Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) +
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.title.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8))

#Outlet_Identifier vs Item_Outlet_Sales
p14  = ggplot(train) +
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.title.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8))

second_row_3 = plot_grid(p13,p14,ncol =2)
plot_grid(p12, second_row_3, ncol = 1)

#Outlet_location_Type vs Item_Outel_Sales
p15 = ggplot(train) +
  geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
  
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)
