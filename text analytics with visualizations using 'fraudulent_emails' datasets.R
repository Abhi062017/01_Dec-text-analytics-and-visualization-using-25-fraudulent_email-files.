#text analytics with visualizations using 'fraudulent_emails' datasets
getwd()
setwd('C:\\Users\\Abhishek\\Downloads\\R\\code_Dec\\01_Dec')
rm(list = ls())
gc()

library(tm)
#1. Build corpus
corpus <- Corpus(DirSource('.')) #Build Corpus from DirSource
dir('.') #lists files in the working directory
View(corpus)
summary(corpus)
summary(corpus[1:5])
names(corpus)
length(corpus)
inspect(corpus[[1]]) #spits out the text in document 1, chars:4029
meta(corpus[[1]])
content(corpus[[1]]) #note: this spits out the text without whitespaces

#2. Pre-Processing:
getTransformations() #lists out pre-defined text formations

corpus <- tm_map(corpus, stripWhitespace) #stripping whitespaces
inspect(corpus[[1]]) #previous chars:4029, now chars:3999
inspect(corpus[[1]]) #note a bunch of email-Id's in this document 1, chars:3999
#custom function to remove Email-ID's
removeEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
} 
corpus <- tm_map(corpus, content_transformer(removeEmail))
inspect(corpus[[1]]) #Email-ID's are stripped off, chars:3811

inspect(corpus[[2]]) #note a URL in this document 2, chars:2415
#custom function to remove URL's
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
inspect(corpus[[2]]) #URL's stripped off, chars:2386

corpus <- tm_map(corpus, removePunctuation) #stripping off Punctuations
inspect(corpus[[2]]) #Punctuations stripped off, chars:2286

corpus <- tm_map(corpus, removeNumbers) #stripping off Numbers
inspect(corpus[[2]]) #Numbers stripped off, chars:2235

# custom function to remove 'special characters' like '@,/,&,\,%....'

# toSpace <- content_transformer(function(x, pattern) gsub(pattern,'',x))
# corpus <- tm_map(corpus, toSpace, "@")
# corpus <- tm_map(corpus, toSpace, "/") 
# corpus <- tm_map(corpus, toSpace, "&") 
# corpus <- tm_map(corpus, toSpace, "'\'") 
# corpus <- tm_map(corpus, toSpace, "%") 
# corpus <- tm_map(corpus, toSpace, "//") 
# corpus <- tm_map(corpus, toSpace, "'\\'") 
# corpus <- tm_map(corpus, toSpace, "//|") 

#NOTE: 'removePunctuation' takes care of almost all of special characters
#hence, no need to run the above 'commented'code.

inspect(corpus[[1]]) #chars:3587
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase the text
inspect(corpus[[1]]) #chars:3587, no change, we just lowered the text cases.

corpus <- tm_map(corpus, removeWords, stopwords('eng')) #removing 'eng' stopwords
inspect(corpus[[1]]) #chars:2776 from 3587 previous.
stopwords('eng') #174 words

corpus <- tm_map(corpus, removeWords, stopwords('SMART')) #removing 'smart' stopwords
inspect(corpus[[1]]) #chars:2621 from 2776 previous.
stopwords('SMART') #571 words

corpus <- tm_map(corpus, stripWhitespace) #as all these operations insert blankspace
inspect(corpus[[1]]) #chars:2273 from 2621 previous.

library(SnowballC) #stemming
corpus <- tm_map(corpus, stemDocument)
inspect(corpus[[1]]) #chars:1965 from 2273 previous.

#*****************************************************************************
#3. forming dtm
dtm <- DocumentTermMatrix(corpus)
View(dtm)
dtm #note: 88% sparse

#3.1 forming matrix from dtm
dtm.matrix <- as.matrix(dtm)
dim(dtm.matrix) #25 docs & 1392 terms
dtm.matrix[1:10,1:10]

sum(dtm.matrix==0) #30695 is the no. of zero entries in the matrix
sum(dtm.matrix!=0) #4105 is the no.of non-zero entries in the matrix
sum(dtm.matrix>=0) #34800 is the total no. of entries in matrix
sum(dtm.matrix==0)/sum(dtm.matrix>=0) # 88% is the % of sparsity in the dtm.

View(as.matrix(dtm))
freq <- colSums(as.matrix(dtm)) #as 'colSums' is a function of matrix.
length(freq) #total no of terms in our dtm is 1392
freq[1:10] #whatif you wanted the 10 most common terms in all 25 docs?
#ordering terms
freq.sorted.asc <- freq[order(freq)] #ascending order
freq.sorted.dsc <- freq[order(-freq)] #descending order
freq.sorted.asc[1:10] #10 least common terms in all 25 docs
freq.sorted.dsc[1:10] #10 most common terms in all 25 docs
#Alternatively, we can use functions:
findFreqTerms(dtm, lowfreq = 25) #terms that appear atleast 25 times in all docs
findMostFreqTerms(dtm, 5) #5 most frequent terms in each of the 25 docs
termFreq(corpus[[1]]) #each term's freq in doc 1
#All the terms & their frequencies in doc 1, whose length<=4
termFreq(corpus[[1]], control = list(wordLengths = c(-Inf,4)))
#All the terms $ their freq in doc 1, whose length is b/w 8 and 10
termFreq(corpus[[1]], control = list(wordLengths = c(8,10)))

#3.2 removing sparsity:
dtm.sparse <- removeSparseTerms(dtm, 0.1)
dtm.sparse
#value of sparse ??? sparsity in matrix.
#the less the value, the less the sparsity,
#hence the more the appearance,of terms, in all docs.

#eg: if sparse=0.01 (1%), = terms that appear in (nearly) all docs.
#if sparse = 0.99 (99%), = terms that appear in very few docs.
#I want to keep the terms that appear in all docs, not in just 1 or 2,
#hence, I will reduce the sparsity, to get the most prevalent terms.
dtm.sparse$dimnames

#wordAssociation:
findAssocs(dtm, c('money','bank','success'), corlimit = 0.75) #correlation>=0.75

#3.3 correlation plot:
source('http://bioconductor.org/biocLite.R')
biocLite('Rgraphviz')
library(Rgraphviz)
#terms that appear atleast 40 times, with correlation >=0.3
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 40), corThreshold=0.3, weighting = T)
#terms that appear atleast 57 times, with correlation >=0.3
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 57), corThreshold=0.3, weighting = T)


#3.4 word-frequency plot:
freq[1:10]
names(freq[1:10])
library(ggplot2)
df <- data.frame(word=names(freq), frequency=freq)
View(df)
dim(df) #1392 rows, 2 columns

ggplot(subset(df,frequency>25), aes(word, frequency, fill = frequency))+
  geom_bar(stat = 'identity')+ #since we're using a y aesthetic, hence not stat='bin'
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

#3.5 wordcloud:
set.seed(9501)
library(wordcloud)
wordcloud(names(freq),freq, min.freq = 1,
          max.words = 120, rot.per = 0.35,
          random.order=F, colors = brewer.pal(8,'Dark2')) #not the best of worclouds
#creating a custom set of word-frequency, by removing sparse terms
dtm.sparse.wordcloud <- removeSparseTerms(dtm, 0.8)
dtm.sparse.wordcloud #62% sparse, terms:232
frequency.sparse <- colSums(as.matrix(dtm.sparse.wordcloud))
frequency.sparse[1:10]

wordcloud(names(frequency.sparse), frequency.sparse,
          min.freq = 5, max.words = 200, rot.per = 0.35,
          random.order = F, colors = brewer.pal(8,'Dark2'))

#*****************************************************************************
#4. k-means clustering
set.seed(123)
dtm.sparse.kmeans <- removeSparseTerms(dtm, 0.3) #so that the plot is legible
dtm.sparse.kmeans
t(dtm.sparse.kmeans)$dimnames
d <- dist(t(dtm.sparse.kmeans), method = 'euclidian')
str(d)
d[1:10]
max(d)
min(d)
summary(d)
kfit <- kmeans(d,4) #we've taken k=4, at random. Use 'elbow' method to find optimal 'k'
kfit #notice, the bss/tss ratio = 83.6%, which is high, so we can consider,k=4
#note: but to find optimal value of 'k', employ 'elbow' method
kfit$betweenss #Sum of Squared distance within a cluster
kfit$totss

#4.1 plotting the clusters
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color = T,
         shade = T, labels = 2, lines = 0) #notice, cluster 3 has least SSD

#finding optimal no. of cluster ('k')

#Intuition:
#When we check the (between_SS / total_SS) we find it is on higher side. 
#This ratio = amount of total sum of squares of the data points,
#which is between the clusters. We want to increase this value.
#And as we increase the number of clusters we see it increasing,
#but we do not want to overfit the data. Be wary of it.
#Alternatively, we can follow tot.withinss, it should be as less as possible.


#4.2 'Elbow method' to find the optimal no. of cluster
set.seed(123)
#Compute wss for k = 1 to k = 15.
k.max <- 15
data <- d
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k)$tot.withinss})
wss
#plot 'elbow'
plot(1:k.max, wss,
     type="b", pch = 19, frame = F, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares") #stable elbow at k=9

kfit$tot.withinss #least at k=9.
#*****************************************************************************
#5. Cluster Dendograms:
library(cluster)
install.packages('fpc', dependencies = T)
library(fpc)
dtm.sparse.dendo <- removeSparseTerms(dtm, 0.4)
dtm.sparse.dendo
d.dendo <- dist(t(dtm.sparse.dendo), method = 'euclidean')
fit <- hclust(d.dendo,method = 'ward.D2')
plot(fit, hang=-1)

#Adding new borders around our cluster dendogram
plot.new()
plot(fit, hang=-1)
group <- cutree(fit, k=3)
rect.hclust(fit, k=3, border = c('red','purple','green'))

#*****************************************************************************
#save the objects
save.image(file = 'text anaytics_fraudulent.emails_workspace.RData')
