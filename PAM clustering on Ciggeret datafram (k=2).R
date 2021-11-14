# The cigarette data
df_cigarette <- read.table("C:\\Users\\ND.COM\\Downloads\\cigaretteConsumption.txt", header = TRUE)
View(df_cigarette)
# For some pre-processing 
colnames(df_cigarette)
summary(df_cigarette)
is.na(df_cigarette)
hist_df = select(df_cigarette, -c(State)) 
View(hist_df)
require(tidyr)
library(psych)
library(plyr)
require(dplyr)
multi.hist(mpg) #error, not numeric
multi.hist(mpg[,sapply(mpg, is.numeric)])
multi.hist(hist_df,nrow = 4, ncol=2,density=TRUE,freq=FALSE,bcol="lightblue",
           dcol= c("red","blue"),dlty=c("solid", "dotted"),
           main=colnames(hist_df))


#PAM CLUSTERING 
library(cluster)
library(factoextra)
library(tidyverse)  # data manipulation
library(NbClust)    #use zip file to install it 
install.packages("fviz_nbclust")
#We will first need to scale the data to prevent abnormalities in the end product. 
scaled_df = scale(hist_df)
View(scaled_df)
fviz_nbclust(scaled_df, pam, method ="silhouette")+theme_minimal()
#Since the graph has it's first decrease at k = 2 clusters we can move on to PAM CLustering 
#Note that the k values for the number of clusters is also found by pam method of clustering. 

pamResult <-pam(scaled_df, k = 2)
pamResult

df_cigarette$cluster = pamResult$cluster
View(df_cigarette)
#To observe the medoids
pamResult$medoids
pamResult$clustering
#Visualizing thw clusters formed. 
fviz_cluster(pamResult, 
             ellipse.type ="euclid",
             repel =TRUE,
             ggtheme =theme_minimal())