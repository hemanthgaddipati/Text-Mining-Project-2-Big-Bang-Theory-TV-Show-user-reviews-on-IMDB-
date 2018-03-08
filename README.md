# Text-Mining-Project-2-Big-Bang-Theory-TV-Show-user-reviews-on-IMDB-

# IDEA/OBJECTIVE : 
	
  The objective is to extract user reviews from IMDB website for the American sitcom T.V series "Big Bang Theory" and then perform text mining techinques on the extrcted data.

# Techniques used : 
	
  Web scraping (using SelectorGadget), TF, TFIDF, Word Cloud, Clustering, Dendrogram, Positive and Negative Sentiment analysis, Semantic Network, K-means clustering, NLP, LDA, Emotion Mining.

  Step 1 :	 To extract the data from IMDB website I used a chrome extension called “SelectorGadget” using this I scraped the data and created a local text document.

  Step 2 :  Before going to the data mining part firstly I wrote all the functions (wordCloud, psoitive and negative sentiment functions, Semantic network function ) so that once we start the text analysis it’d be easier to just call these functions rather than making all the code clumsy.

  Step 3 :  Now that the data’s been extracted and the required functions are written, I started text mining on the data. Firstly I created a “Corpus” and then cleaned the data  i.e striped off the white spaces, converted all the documents into lower case, removed punctuations, removed numbers, took off all the stop words and filler words.

  Step 4 :  Now the corpus is cleaned and ready I’ve proceeded with thee text mining by creating a TDM (Term Document Matrix) for TF and TFIDF. (i.e Term Frequency and Term Frequency Inverse Document Frequency). Now I converted the TDM’s to DTM’s and then arranged all the documents in descending order. Finally I called the wordcloud function and createdd a UNIGRAM using TFIDF. Then Positive and Negative word clouds and drew some Interesting insights from the data however the data has to be seen in context to understand it well enought o make a business decision. To do this I then proceded with Natural Language Processing (NLP) and Emotion Mining.

  Step 5 :  Now in NLP I used LDA technique to get better insights of the data and created 10 groups each of 5 terms and then using Hierarchical clustering plot I can easily relate the groups in a particular cluster. So this means that the clustering is a bit precise. Now that we are done with the elementary NLP lets proceed with the emotion  mining. 
  
  Step 6 : From the emotion mining it seems that the most negative review seems to be as follows :

“I know because the moron I lived with played it so loud I could hear it through 2 doors.) **laugh** came on with each and every phrase of each sentence of **laugh** each and every annoying character **laugh** regardless of how \"funny\" it was - **laugh** which of course it wasn't except for the annoying main character **laugh** who had that annoying feminine sarcasm in each and every glib phrase he spewed to try and make it funny - which it wasn't because it was merely annoying.”
	
  And the most positive review seens to be as follows :

“With the introduction of Bernadette and Amy, and a shift of character from most of the main cast from nerds to nerd stereotypes, it's pretty clear that they tried make the show appeal to a larger audience, and by doing so alienating those who liked the clever and nerdy jokes with multiple layers to appreciate.”

  Step 7 : The barplot of all the eight emotions is developed and studied the data.




