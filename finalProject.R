# Kayelin Santa Elena
# MGSC 410
# Final Project

#--------------------------#
remove(list = ls())

getwd()

# import data
app.og <- read.csv("/Users/kksizzle/Desktop/MGSC 410/final project/App activity (Chapman data extract, Apr 2020).csv")
dict.og <- read.csv("/Users/kksizzle/Desktop/MGSC 410/final project/Data dictionary (Chapman data extract, Apr 2020).csv")
subs.og <- read.csv("/Users/kksizzle/Desktop/MGSC 410/final project/subscriber Information (Chapman data extract, Apr 2020).csv")

# get packages
library(ggplot2) 
library(dplyr)
library(readr)
library(stringr)
library(ggthemes)
library(tidyr)
library(caret)
library(rsq)
library(olsrr)
library(RColorBrewer)
library(corrplot)

#--------------------------#

##############------------------ Data Exploration ------------------###############

# dimensions
dim(app.og) # 809,478 by 4
dim(dict.og) # 31 by 2
dim(subs.og) # 40,102 by 24

# unique variables
unique(app.og$App.Session.Platform)
unique(app.og$App.Activity.Type)

unique(subs.og$Language)
unique(subs.og$subscription.Type)
unique(subs.og$subscription.Event.Type) 
unique(subs.og$Purchase.Store)
unique(subs.og$Currency)
unique(subs.og$Country)

# how many app each user tweeted
summary(app.og$name)
a <- as.data.frame(table(app.og$name))

# check how many missing variables
sum(is.na(app.og))
sum(is.na(dict.og))
sum(is.na(subs.og))

# graphs and plots
ggplot(subs.og, aes(Purchase.Store, Purchase.Amount)) +
  geom_point()



##############------------------ Clustering ------------------###############

library(forcats)
library(zoo)
library(factoextra)
library(cluster)
library(scales)
library(mondate)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tictoc)

# import new data
subs <- read.csv("/Users/kksizzle/Desktop/MGSC 410/final project/Rosetta (1).csv")

# get rid of outliers
# subs <- subs[!(subs$Send.Count > 1000),]
# subs <- subs[!(subs$Open.Count > 1000),]
# subs <- subs[!(subs$Click.Count > 900),]

# check outliers
# ggplot(subs, aes(Send.Count, Dollar_val)) +
#   geom_point()
# ggplot(subs, aes(Open.Count, Dollar_val)) +
#   geom_point()
# ggplot(subs, aes(Click.Count, Dollar_val)) +
#   geom_point()

# subset data when using train and test
# set.seed(310)
# subs_indx <-  sample(1:nrow(subs), 0.5*nrow(subs), replace=FALSE)
# subs_subset <- subs[subs_indx,]

# entire dataset
subs_indx <- subs
subs_subset <- subs

#---------------- Rearange Data ----------------#

# dummy variables
subs_subset$Subscription.Type <- ifelse(subs_subset$Subscription.Type == 'Limited', 1, 0)
subs_subset$Subscription.Event.Type <- ifelse(subs_subset$Subscription.Event.Type == '	INITIAL_PURCHASE', 1, 0)
subs_subset$Purchase.Store <- ifelse(subs_subset$Purchase.Store == 'Web', 1, 0)
subs_subset$Demo.User <- ifelse(subs_subset$Demo.User == 'No', 1, 0)
subs_subset$Free.Trial.User <- ifelse(subs_subset$Free.Trial.User == 'No', 1, 0)
subs_subset$Auto.Renew <- ifelse(subs_subset$Auto.Renew == 'No', 1, 0)
subs_subset$User.Type <- ifelse(subs_subset$User.Type == 'Consumer', 1, 0)
subs_subset$Email.Subscriber <- ifelse(subs_subset$Email.Subscriber == 'No', 1, 0)
subs_subset$Push.Notifications <- ifelse(subs_subset$Push.Notifications == 'No', 1, 0)

# one hot encoding
onehotdf <- subs_subset[ , c("ID","Currency","Country","Lead.Platform")] # new df for categorical vars

subs_subset1 <- select(subs_subset,-c("Language","Currency","Country","Lead.Platform",
                           "Subscription.Start.Date","Subscription.Expiration",
                           "Free.Trial.Start.Date", "Free.Trial.Expiration")) # new df for w/out cat. vars. + sub/trial dates cols 

# one-hot coding
library(caret)
dummies <- dummyVars(" ~ .", onehotdf)
onehotdfdummies <- data.frame(predict(dummies, newdata = onehotdf))


subs_subset2  <- data.frame(subs_subset1,onehotdfdummies) # combine new encoded vars with the df
subs_subset2 <- select(subs_subset2, -ID) # drop ID column, not needed in cluster

#factor one-hot dummies {so that it works with kproto()}
require(plyr)
subs_subset2[,18:27] <- colwise(as.factor)(subs_subset2[,18:27])


#---------------- K prototype clustering ----------------#

#install.packages("clustMixType")
library(clustMixType)

# Check for  optimal number of clusters
tic() #timer
wss<-vector()
for (i in 2:15){ wss[i] <- sum(kproto(subs_subset2, i,na.rm = FALSE)$withinss)}

par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# apply k-prototyps
kpres <- kproto(subs_subset2, 6, na.rm = FALSE)
subs_subset2$cluster = kpres$cluster  # add cluster column to df
toc() #timer

# new df to make graphs and plots

# use this when using train and test
# new_subs <- subs[subs_indx,]
# df <- select(new_subs, -c("ID","Subscription.Start.Date","Subscription.Expiration",
#                              "Free.Trial.Start.Date", "Free.Trial.Expiration"))

df <- select(subs, -c("ID","Subscription.Start.Date","Subscription.Expiration",
                          "Free.Trial.Start.Date", "Free.Trial.Expiration"))

df$cluster = kpres$cluster # add cluster column to df

# save df to csv for tableau
# write.csv(df,"/Users/kksizzle/Desktop/MGSC 410/final project/clusters.csv")

# save df to excel
# install.packages("writexl")
# library(writexl)
# write_xlsx(df,"/Users/kksizzle/Desktop/MGSC 410/final project/clusters.xlsx")

#------Graphs-----#

ggplot(df, aes(cluster, fill = Currency))+
  geom_bar(position = 'fill')

ggplot(df, aes(cluster, fill = User.Type))+
  geom_bar(position = 'fill')

ggplot(df, aes(cluster, fill = Subscription.Type))+
  geom_bar(position = 'fill')

ggplot(df, aes(cluster, fill = User.Type))+
  geom_bar(position = 'fill') +
  facet_wrap(~Subscription.Type, scale = 'free')

ggplot(df, aes(cluster, fill = Subscription.Event.Type))+
  geom_bar(position = 'fill')

ggplot(df, aes(cluster, fill = User.Type))+
  geom_bar(position = 'fill') +
  facet_wrap(~Subscription.Event.Type, scale = 'free')

ggplot(df, aes(Open.Count, Dollar_val, color = as.factor(cluster)))+
  geom_point() +
  coord_cartesian(xlim = c(0,800))

ggplot(df, aes(Send.Count, Dollar_val, color = as.factor(cluster)))+
  geom_point() +
  coord_cartesian(xlim = c(0,800))

ggplot(df, aes(Click.Count, Dollar_val, color = as.factor(cluster)))+
  geom_point() +
  coord_cartesian(xlim = c(0,700))

# ggplot(df, aes(Open.Count, Dollar_val, color = as.factor(cluster)))+
#   geom_point() +
#   coord_cartesian(xlim = c(0,800), ylim = c(0,800))
# 
# ggplot(df, aes(Send.Count, Dollar_val, color = as.factor(cluster)))+
#   geom_point() +
#   coord_cartesian(xlim = c(0,800))

clprofiles(kpres, subs_subset2)

# cluster 1
# mostly euros + GBP
# almost all consumer
# limited subscription
# mostly initial purchase, some renewal
# low purchase amount

# cluster 2
# literally all NULL currency
# almost all consumer
# limited subscription, little bit lifetime
# 45% renewal, 55% initial purchase
# purchase amount is a lot higher 

# cluster 3
# mostly NULL currency + some USD
# 75% other, 25% consumer
# limited subscription, little bit lifetime
# 45% renewal, 55% initial purchase
# medium size purchase amount 

# cluster 4
# mostly USD
# mostly all other, a little bit consumer
# limited subscription, some lifetime
# mostly initial purchase, some renewal
# low purchase amount

# cluster 5
# all USD
# almost all consumer
# limited subscription, some lifetine
# mostly initial purchase, some renewal
# low purchase amount




