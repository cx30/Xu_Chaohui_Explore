library("ggplot2")


#Write a function called explore that accepts the following	
#parameters, with defaults for the last three parameters:
#1. A dataframe
#2. A plot switch that can accept three values: off, on, or grid
#3. A threshold cut-off value between 0 and 1 for correlations
#4. An optional vector that contains one or more integers that	
#represent the numbers of bins to use for a histogram. If the	
#vector is not provided, then let ggplot use it???s default.

explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  
  result1 <- freq_table(data)
  result2 <- num_table(data)
  result3 <- r_square(data)
  result4 <- pearCoefs(data, threshold)
  
  Num <- data[sapply(data, is.numeric)]
  result5 <- plotsNum(Num, plotswitch, bins)
  result6 <- plot_gray(data, plotswitch)
  
  return (c(result1, result2, result3, result4, result5, result6))
}



#1 A frequency table for every categorical and logical variable
freq_table <- function(data) {
  #This function takes a dataframe and return a frequecy table for every catagorical and
  #logical variable
  #@parameter: data frame
  #@return the frequency table for categorical and logical variables
  cat_and_log <- c(summary(Filter(is.factor,data)), summary(Filter(is.logical,data))) 
  #loop through the data frame and create the frequecy table for categorical and logical variables
  return(cat_and_log)
}

#2a For numerical variables, a summary statistics table for each numerical variable
num_table <- function(data) {
  #This function takes a dataframe and return a summary statistics table for each numerical variable
  #@parameter: data frame
  #@return the summary statistics table for numerical columns
  num_var <- data[sapply(data, is.numeric)]
  return(summary(num_var)) #return the summary statistics table
}

#2b A data frame that contains each pair of column names in	
#the first column (name the column ???Variable Pairs???) and the	
#associated r-square value in the second column (name the	
#column ???R-Square???).

r_square <- function(data) {
  #@parameter: data frame
  #@return a new data frame that contains each pair of columns names in the first column
  #@and the associated r-square value in the second column
  type <- sapply(data, class) #get the type of all columns
  data <- data[which(type == "numeric")] #extract the numeric columns
  colname <- colnames(data)
  pair_names <- c()
  pair_rsquare <- c()
  for (i in 1:(length(colname) - 1 )) { #looking into every column, starting with 1st column
    for (j in (i+1):length(colname)) { #looking into each column after the first column
      temp <- summary(lm(data[,i]~data[,j]))$r.squared #obtain the r-square value
      pair_names <- c(pair_names, paste(colname[i], colname[j], sep = '-')) #get each pair of the column names
      pair_rsquare <- c(pair_rsquare, temp) #form a new list with r-square values
    }
  }
  new_frame <- data.frame(pair_names, pair_rsquare) #form a new data frame with pair_names and pair_rsquare
  colnames(new_frame) <- c("Variable Pairs","R-Square") #name the columns
  return(new_frame)
}



#2c A data frame that contains each pair of column names in	
#the first column (name the column ???Variable	Pairs???) and	
#correlation coefficient (Pearson) for all coefficients whose	
#absolute value is greater than the correlation threshold (do	
#not repeat any pairs) in the second column (name the	
#column ???Pearson Exceeds Threshold???). (HINT: There is a	
#function that calculates correlation coefficients ??? look	
#carefully at what is returned and optimize how you extract	
#the correlation coefficients)

pearCoefs <- function(data,threshold) {
  #@parameter: a data frame
  #@parameter: a positive number as threshold
  #@return a data frame that contains each pair of column names in the first column and correlation
  #@coefficient (Pearson) for all coeffients whose absolute value is greater than the correlation threshold
  num_data <- data[sapply(data,is.numeric)] #get the numeric columns and store it in a new frame
  num_names <- colnames(num_data) #extract the names of numerical columns
  pairNames <- c() # create a null vector
  pairCoeffs <- c() # create a null vector
  l <- ncol(num_data) # get the number of columns and store it in l
  for (i in 1:(l-1)) { # looking into every column in num_data, starting from 1st column
    for (j in (i+1):l) { #compare each numeric column with each numeric column after it 
      corcoef <- cor(num_data[i],num_data[j],method="pearson") #calculate the correlation coeffiecient using pearson method
      if (abs(corcoef)>threshold) { #if the absolute value of the correlation coeffiecient is larger than the threshold
        pairNames <- c(pairNames,paste(num_names[i],num_names[j],sep='-')) #create the new name pairs
        pairCoeffs <- c(pairCoeffs,corcoef) # store the correlation coefficient that is larger than the threshold
      }
    }
  }
  new_frame <- data.frame(pairNames,pairCoeffs) #create a new data frame that contains pairNames and pairCoeffs
  colnames(new_frame) <- c("Variable Pairs","Pearson Exceeds Threshold") #naming
  return (new_frame) # return the new data frame
}


#3 #If the plot switch parameter is ???on??? or	???grid???, then plot a pair of	
#blue histograms with a vertical red line	at the mean (one using	
#counts and the other density) for every numerical variable at	
#each number of bins integer specified in the bin vector
#parameter. If the plot switch is set to ???grid???, there should be a	
#grid for each count-bin combination and a separate grid for	
#each density-bin size combination. For	example, given 5	
#numeric variables and a vector of three bin number integers,	
#the function should generate 30 individual plots or a total of 6	
#grid plots (with each grid plot containing 5 subplots).

plotsNum <- function(data, plotswitch, bins=NULL) {
  #@parameter: a data frame
  #@parameter: "on" or "grid"
  #this function will return what is described above
  data_frame <- data[sapply(data, is.numeric)]
  for(j in 1:length(bins)){
    if (plotswitch == "on") {
      for(i in 1:ncol(data_frame)){
        print(ggplot(data_frame,aes(x=data_frame[,i],..density..))+
                geom_histogram(bins=bins[j],fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
        #for each bin size, plot density histogram
        
        print(ggplot(data_frame,aes(x=data_frame[,i]))+
                geom_histogram(bins=bins,fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
        #for each bin size, plot count histogram
      }
    }
    else if (plotswitch == "grid") {
      plot.new()
      layout(matrix(c(1:10), 2, 5, byrow = TRUE))
      for(i in 1:ncol(data_frame)){
        print(ggplot(data_frame,aes(x=data_frame[,i],..density..))+
                geom_histogram(bins=bins[j],fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
      }
      
      plot.new()
      layout(matrix(c(1:10), 2, 5, byrow = TRUE))
      for(i in 1:ncol(data_frame)){
        print(ggplot(data_frame,aes(x=data_frame[,i]))+
                geom_histogram(bins=bins[j],fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
      }
      
    }
    else {
    }
  }
}

#4. If the plot switch parameter is "on" or "grid", plot a gray bar
#graph for every categorical and binary variable.
plot_gray <- function(data, plotswitch = "off") {
  dfm_cb <- data[,sapply(data,is.factor)|sapply(data,is.logical)]
  #extract categorical and binary variable
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(dfm_cb)){
      grid.newpage()
      p <- ggplot(dfm_cb,aes(x=data[,i]),colour="gray")+
        geom_bar()+ xlab(colnames(dfm_cb[i]))
      #plot gray bar for every categorial and binary variable
      print(p)
    }
  }
}



  