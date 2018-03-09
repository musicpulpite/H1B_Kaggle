new_dataframe<-function(dataframe, columnindex = 4, threshold = 0.005){
      #extract the names column as a character vector
      names<-as.character(unique(dataframe[[columnindex]]))
      #remove possible NA values
      names<-names[!is.na(names)]
      
      #create and fill a square matrix with the pairwise adist string distances
      #computed for each pair of unique names
      stringdists<-matrix(nrow = length(names), ncol = length(names))
      for (i in 1:length(names)){
            for (j in 1:length(names)){
                  stringdists[i, j]<-adist(names[i], names[j], ignore.case = TRUE)
            }
      }
      #find cutoff value of adist where two strings with distance less than the
      #cutoff will be considered identical
      #this quantile calculation is fed with a threshold parameter from the
      #function call
      cutoff<- quantile(c(stringdists)[c(stringdists) != 0], probs = threshold, names = FALSE, na.rm = TRUE)
      
      #create a new numerical class for names considered to be identical
      #if two strings have the same numerical class they are considered to be
      #identical
      stringclass<-numeric(length(names)+1)
      stringclass[length(names)+1]<-0
      #I have to do a silly thing to stringclass to account for NA values in 
      #the original names column
      for (i in 1:length(names)){
           for (j in 1:length(names)){
                 if(stringdists[i, j] < cutoff){
                       stringdists[i, j]<-min(i, j)
                 } else{
                       stringdists[i, j]<-0
                 }
           } 
      }
      #collapse the stringclasses
      for (i in 1:length(names)){
            stringclass[i]<- which(stringdists[i,] > 0)[1]
      }
      names_class<-stringclass[match(as.character(dataframe[[columnindex]]), c(names,NA))]
      new_dataframe<-cbind(dataframe, names_class)
}
      