library(plumber)
library(jsonlite)
library(ggplot2)

#Commands:
#Start the server: plumber::plumb("cluster_api.R")$run(port = 5762)
#Call the API from command line: 
#curl -X POST -H "Content-Type: application/json" -d cluster_config.json http://localhost:5762/v0/kmeans

#Auxiliary functions
#write here the auxiliary functions if needed

#paramethers:
# min.k: min value of k to the used to train the kmeans models
# max.k: max value of k to the used to train the kmeans models
# data: the features data to train the models
# nstart: nstart paramether of kmeans function
#return:
# wss: a vector with the value of tot.withinss for each trained model
kmeans_elbow <- function(min.k,max.k,data,nstart){
  wss <- sapply(min.k:max.k,function(k){
    kmeans(data,k,nstart=nstart)$tot.withinss
  })
  
  #returns the vector of the tot.withinss of the models with each value of K
  wss
}

#paramethers:
# data: dataframe with the data for the cluster analysis
#return:
# best_features: a list of the best feaures
best_model <- function(data){
  model_features <- names(data)
  
  for (i in 2:length(model_features)) {
    
    
  }
  
}

#* Cluster Analysis using K-Means. The user provides: POST a JSON file with the cluster analysis configuration
#* The JSON file has the path to the csv data file and the cluster hyper-paramethers
#* The endpoint returns the original data with each observation leveled in its cluster. 
#* In the production version the original data will not be returned, only the observation ID (i.e customer_id)
#* @post /v0/kmeans

function(req,res){
  #reading cluster analysis paramethers from config file
  cluster_config <- fromJSON(req$postBody)
  seed <- cluster_config$seed
  k <- cluster_config$k
  nstart <- cluster_config$nstart
  cluster_variables <- cluster_config$cluster_variables
  filepath <- cluster_config$data_filepath
  
  #reading data
  set.seed(as.numeric(seed))
  data <- read.csv(filepath)
  data_features <- names(data)
  
  #selecting features. From the data DF selecting only the columns specified by the user in the config file
  features <- data[data_features %in% cluster_variables]
  
  #training the model
  cluster <- kmeans(features,as.numeric(k),nstart=as.numeric(nstart))
  
  #adding the cluster labels information to the data
  data$cluster_label <- cluster$cluster
  
  #writting output file with clusters information
  write.csv(data,file = "data_clustered.csv",row.names = FALSE)
  
  #sending the output data back to the user
  include_file("data_clustered.csv",res)
}


#* Best K selection using Elbow method. The user provides: POST a JSON file with the cluster analysis configuration
#* The JSON file has the path to the csv data file and the cluster hyper-paramethers. As K the user must pass a list of two integers with the min and max value of K to be tested
#* The endpoint returns a PNG file with the Elbow plot of Total Within-clusters sum of squares, so the user can decide which is the best value of k. 
#* @serializer contentType list(type="image/png")
#* @post /v0/kmeans_best_k

function(req,res){
  #reading cluster analysis paramethers from config file
  cluster_config <- fromJSON(req$postBody)
  seed <- cluster_config$seed
  k <- cluster_config$k
  nstart <- cluster_config$nstart
  cluster_variables <- cluster_config$cluster_variables
  filepath <- cluster_config$data_filepath

  #reading data
  set.seed(as.numeric(seed))
  data <- read.csv(filepath)
  data_features <- names(data)
  
  #selecting features. From the data DF selecting only the columns specified by the user in the config file
  features <- data[data_features %in% cluster_variables]
  
  #training the model with all possible values of k
  wss <- kmeans_elbow(k[1],k[2],features,nstart)
  
  #temporaly file to store the Elbow Plot
  tmp <- tempfile()
  png(tmp)
  
  #Elbow Plot
  elbow.plot <- qplot(k[1]:k[2],wss) + ggtitle("Elbow Plot") + ylab("Total Within-clusters Sum of Squares") + xlab("Number of clusters K") 
  elbow.plot <- elbow.plot + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"))
  print(elbow.plot)
  dev.off()
  
  readBin(tmp, "raw", n = file.info(tmp)$size)
  
}

#* Cluster Analysis using K-Means. The user provides: POST a JSON file with the cluster analysis configuration
#* The JSON file has the path to the csv data file and the cluster hyper-paramethers
#* In this endpoint the user wants to obtain the best model for different combination of features
#* The endpoint returns a json with the list of features of the best model 
#* 
#* @post /v0/kmeans_best_features

#pending improvement: the endpoint returns a json with the list of features of each model, plus the tot-withinss statistic for each one. The list is ordered from the 
#best to the worst model. This could be useful for feature selection and analysis.

#notes: 
#I will try to extrapolate the best subset selection method from Regression Models. 
#More details can be found in "An Introduction to Statistical Learning - James, Witten, Hastie, Tibshirani"
#For the first version I will try to implement the best subset selection method. Then I will try with stepwise selection (forward and backward)

function(req,res){
  #reading cluster analysis paramethers from config file
  cluster_config <- fromJSON(req$postBody)
  seed <- cluster_config$seed
  k <- cluster_config$k
  nstart <- cluster_config$nstart
  cluster_variables <- cluster_config$cluster_variables
  filepath <- cluster_config$data_filepath
  
  #reading data
  set.seed(as.numeric(seed))
  data <- read.csv(filepath)
  data_features <- names(data)
  
  #selecting features. From the data DF selecting only the columns specified by the user in the config file
  features <- data[data_features %in% cluster_variables]
  
  #finding the best subset of features for the model
  best_model <- kmeans_best(features)
  
  #returing the list of best feaures to the user
  best_model
}
