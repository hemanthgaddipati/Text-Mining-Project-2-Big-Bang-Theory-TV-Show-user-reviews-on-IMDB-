## Natural Language Processing

  require(tm) || install.packages("tm");library(tm)
  
  require(slam) || install.packages("slam");library(slam)

  require(topicmodels) || install.packages("topicmodels");library(topicmodels)

##Loading the previously created text document

  x <- scan(file.choose(), what = "character", comment.char = ";")

  x

  length(x)

## Creating corpus and cleaning data

  mydata.corpus <- Corpus(VectorSource(x))

  mydata.corpus <- tm_map(mydata.corpus, removePunctuation) 

  my_stopwords <- c(stopwords('english'),"the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

  mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

  mydata.corpus <- tm_map(mydata.corpus, removeNumbers) 

  mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)

## build a term-document matrix

  mydata.tdm <- TermDocumentMatrix(mydata.corpus)

  mydata.tdm

  dim(mydata.tdm)

## dtm <- as.DocumentTermMatrix(mydata.dtm3)

  dtm <- t(mydata.tdm)

  rowTotals <- apply(dtm, 1, sum)

  ?apply

  dtm.new   <- dtm[rowTotals > 0, ]

  lda <- LDA(dtm.new, 10) # find 10 topics

  ?LDA

  term <- terms(lda, 5) # first 5 terms of every topic

  term

  tops <- terms(lda)

  tb <- table(names(tops), unlist(tops))

  tb <- as.data.frame.matrix(tb)

  ?unlist

  cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance

  par(family = "HiraKakuProN-W3")

  plot(cls)

############################################################################################
############################################################################################

## Emotion mining 

  require(syuzhet) || install.packages("syuzhet");library("syuzhet")

  emotion.data <- scan(file.choose(), what = "character" , comment.char = ";" )
  
  a <- get_sentences(emotion.data)

  class(a)

  str(a)

  head(a)

## Creating Sentiment Vector for analysis
  
  sentiment_vector <- get_sentiment(a, method = "bing")

  head(sentiment_vector)

  afinn_s_v <- get_sentiment(a, method = "afinn")

  head(afinn_s_v)

  nrc_vector <- get_sentiment(a, method="nrc")

  head(nrc_vector)

  sum(sentiment_vector)

  mean(sentiment_vector)

  summary(sentiment_vector)

# plot
  
  plot(sentiment_vector, type = "l", main = "Plot Trajectory",
  
        xlab = "User review", ylab = "Emotional Valence")

  abline(h = 0, col = "red")

  ## Extracting the sentence with the most negative emotional valence

    negative <- a[which.min(sentiment_vector)]

    negative

  ## Extracting the most positive sentence

    positive <- a[which.max(sentiment_vector)]

    positive

## more depth

  poa_v <- emotion.data

  poa_sent <- get_sentiment(poa_v, method="bing")

  plot(
        poa_sent, 
        type="h", 
        main="Example Plot Trajectory", 
        xlab = "User Review", 
        ylab= "Emotional Valence"
      )

## percentage based figures
   
##(input vector is not that big to produce percentage based figures but feel free to use the below code if you have large data 

##############################################################################################    
  percent_vals <- get_percentage_values(poa_sent)

  plot(
        percent_vals, 
        type="l", 
        main="Shut the critic Using Percentage-Based Means", 
        xlab = "User Review", 
        ylab= "Emotional Valence", 
        col="red"
      )

  ft_values <- get_transformed_values(
                                        poa_sent, 
                                        low_pass_size = 3, 
                                        x_reverse_len = 100,
                                        scale_vals = TRUE,
                                        scale_range = FALSE
                                      )
####################################################################################
  plot(
        ft_values, 
        type ="h", 
        main ="New values after transformation", 
        xlab = "User Review", 
        ylab = "Emotional Valence", 
        col = "red"
      )

## categorize each sentence by eight emotions

  nrc_data <- get_nrc_sentiment(a)

## subset

  sad_items <- which(nrc_data$sadness > 0)

  head(a[sad_items])

## To view the emotions as a barplot

  barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)


#####################################################################################################################
  
  
  
  
  