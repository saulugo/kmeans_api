library(plumber)

#* @apiTitle API test

#* Echo provided text
#* @param text: The text to be echoed in the response
#* @get /echo

function(text = "") {
  list(
    message_echo = paste("The text is:", text)
  )
}


#* Sum two provided numbers
#* @param num1 The first number
#* @param num2 The second number
#* @get /sum

function(num1, num2){
  mysum <- as.numeric(num1) + as.numeric(num2)
  response <- list(
    message = paste("La suma es:",mysum)
  )
}

#* Load into a dataframe data from a provided CSV file and return the summary
#* @parm filepath The file to be read
#* @get /summaryDF

function(filepath){
  data <- read.csv(filepath)
  mysummary <- summary(data)
  
  response <- list(
    message = as.character(mysummary)
  )
}




