setwd("D://Study//R debutant+//Essay")

library(data.table)
library(dplyr)

##Filtering out the redundant variables from the the initial datasets to get datasets of manageable size
df4<-fread(file = 'data_title_basics.tsv',header = TRUE) #reading the file

head(df4,20)

df<-subset(df4,titleType=='movie'&primaryTitle==originalTitle&startYear>=1995&startYear<=2020) # leaving only 1995-202o movies, excluding series and short films 
df<-df%>%filter(grepl('Drama',genres)) #leaving only dramas
head(df)
dim(df)
rm(df4)

df3<-fread(file = 'data_title.tsv',header = TRUE) #reading the file
dim(df3)
df3<-subset(df3,region:'US',types:'imdbDisplay') #leaving only us films and their imdb titles
df3<-df3[,c('titleId','ordering','title')]
dim(df3)
head(df3)

df<-subset(df,tconst %in% df3$titleId)
df<-df[,c('tconst','primaryTitle','isAdult','startYear','runtimeMinutes','genres')]
dim(df)
head(df)
save(df,file="movies_filtered.Rda") # saving the clean data on movie names
rm(df,df3)


df_crew<-fread(file = 'data_principals.tsv',header = TRUE) #reading the file
dim(df_crew)
df_crew<-subset(df_crew,tconst %in% df$tconst)
df_crew<-df_crew[,c('tconst','nconst','category')]
dim(df_crew)
head(df_crew)

df_names<-fread(file = 'data_names.tsv',header = TRUE) #reading the file
dim(df_names)
df_names<-subset(df_names,nconst %in% df_crew$nconst)
dim(df_names)
head(df_names)
unique(df_crew$category)
save(df_crew,file="crew_filtered.Rda")
save(df_names,file="names_filtered.Rda")


#Converting the raw data on movies, crew members and their personal info to a complete dataset with all the data for each movie
library(dplyr)
library(Hmisc)

load("movies_filtered.Rda") #loading the datasets
load("crew_filtered.Rda")
load("names_filtered.Rda")

head(df)
head(df_names)
head(df_crew)
hist(as.integer(df$startYear))
hist(as.integer(df$runtimeMinutes),breaks=1000,xlim=c(1,250))
unique(df[,genres])
unique(df_crew[,category])
head(df_crew[category=='director',])

#Combining datasets to a full one, containing one line for each movie-crew member pair
df_full<-merge(df,df_crew,by='tconst',all.y=TRUE)
df_full<-merge(df_full,df_names,by='nconst',all.x=TRUE)
head(df_full)
Hmisc::describe(df_full)

#Making sure that all the numerical columns are indeed numeric
df_full$isAdult<-as.numeric(df_full$isAdult)
df_full$startYear<-as.numeric(df_full$startYear)
df_full$runtimeMinutes<-as.numeric(df_full$runtimeMinutes)
df_full$birthYear<-as.numeric(df_full$birthYear)
df_full$deathYear<-as.numeric(df_full$deathYear)

describe(df_full)

#filtering and adding columns
df_full$Age<-ifelse(is.na(df_full$birthYear),NA,df_full$startYear-df_full$birthYear) #calculating the crew member's age
df_full<-df_full[df_full$Age<=100,] #dropping movies with crew members having unrealistically high ages 
                                    #this happens sometimes when the book by a dead author is used as a screenplay
df_full<-df_full[df_full$Age>=0,]    #same with negative ages
df_full$Titlelength<-nchar(df_full$primaryTitle) #calculating the length of the title
hist(df_full$Titlelength,breaks=1000)
df_full<-df_full[df_full$Titlelength<=60,] #also removing the clear outliers with titles too long

df_full<-subset(df_full,category %in%c('composer','director','writer','cinematographer','producer','editor')) ##leaving only crew members info
df_full$director<-ifelse(df_full$category=='director',df_full$nconst,NA) #creating a column with the id of director
df_full$producer<-ifelse(df_full$category=='producer',df_full$nconst,NA) #creating a column with the id of producer
df_full$director_age<-ifelse(df_full$category=='director',df_full$Age,NA) #adding a column with director's age

unique(df_full$category) #checking the result
dim(df_full)
#for each movie, calculating a mean age of a crew and writing director's and producer's id for each row as well as director's age
df_full_movie<-as.data.frame(df_full %>% 
  group_by(tconst) %>% 
  mutate(Age_average = (sum(Age)-ifelse(is.na(director_age),0,director_age))/(n()-ifelse(is.na(director_age),0,1)), #calculating average age excluding the director
         director=max(director,na.rm = T),
         producer=max(producer,na.rm = T),
         director_age=ifelse(is.na(max(director,na.rm = T))==F,max(director_age,na.rm = T),NA)) %>% 
  ungroup())

#grouping the dataset
df_full_movie<-as.data.frame(df_full_movie %>%
  distinct(tconst, .keep_all = TRUE)%>%
  group_by(tconst,primaryTitle,isAdult,startYear,runtimeMinutes,genres,Titlelength,director,producer,director_age,Age_average)%>%
  select(tconst,primaryTitle,isAdult,startYear,runtimeMinutes,genres,Titlelength,director,producer,director_age,Age_average))

describe(df_full_movie) #checking the result

head(df_full_movie)
dim(df_full_movie)
df_full_movie<-df_full_movie[complete.cases(df_full_movie[,-c(1,6,8,9)]),] #leaving only complete cases on the variables I'm going to use
dim(df_full_movie)
save(df_full_movie,file="full_movie.Rda") #saving the final dataset

head(PCA_dataset)

PCA_dataset<-df_full_movie[,-c(1,2,6,8,9)] #subsetting the PCA data
dim(PCA_dataset)
save(PCA_dataset,file="PCA_dataset.Rda") #saving the PCA data

#Actual analysis of the resultant dataset!
load("full_movie.Rda")
load("PCA_dataset.Rda")

View(head(df_full_movie[,-c(1,6,8,9)],10))
summary(PCA_dataset)
View(summarytools::descr(PCA_dataset,stats='common')) #descriptive statistics

PCA_dataset<-subset(PCA_dataset,isAdult==0)
PCA_dataset<-PCA_dataset[,-1] #dropping the isAdult column as it has only one movie
View(summarytools::descr(PCA_dataset,stats='common')) #descriptive statistics


library("FactoMineR")
library("factoextra")
library("PerformanceAnalytics")
library("Factoshiny")

chart.Correlation(PCA_dataset, histogram=TRUE, pch=19)

#Plotting the correlation matrix for the variables in use
library(corrplot)
head(PCA_dataset)
cormat<-PCA_dataset #auxilary dataframe for the corrplot
cormat<-round(cor(cormat,method = "pearson"),2) #correlations
p.mat<-cor.mtest(cormat)$p # p-values
colnames(cormat) <- c("startYear", 'runtimeMinutes',"Titlelength", 'director_age', 'Age_average')
rownames(cormat) <- c("startYear", 'runtimeMinutes',"Titlelength", 'director_age', 'Age_average')
corrplot(cormat, type="upper", method = "number",  sig.level = 0.05,p.mat = p.mat,insig="pch", tl.srt = 60,tl.col = "black",number.cex = 1.5,cl.pos = "b",title="Correlation matrix",mar=c(1,0,1,0))

#Principle Component Analysis
library("FactoMineR")
PCAshiny(PCA_dataset) #with web interface


res.pca <- PCA(PCA_dataset, graph = FALSE) #creating an pca object

#Screeplot, to see the contribution of the princpiple components
fviz_screeplot(res.pca, addlabels=TRUE)

#Loading of variables in use on the principle components
fviz_pca_var(res.pca, col.var="contrib")

#Loading of individual movies onto principle components
fviz_pca_ind(res.pca,  col.ind="cos2") +
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.50)+
  theme_minimal()

df_full_movie[rownames(df_full_movie)==105,]
#Clustering
k = 2 #smallest number possible, as the datapoints are strongly merged together
kmeans = kmeans(PCA_dataset, centers = k, nstart = 50)
fviz_cluster(kmeans, data = PCA_dataset)
