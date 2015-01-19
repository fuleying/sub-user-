library(RODBC)
library(tm)
conn <- odbcConnect("kingdee", uid = "fuleying", pwd = "aaaa")
mydata <- sqlQuery(conn, "select FFUNCTIONKEY , FOPERATIONCOUNT , FOPERATIONDAY 
	from t89 where FPRODUCTREGNO = '3060168233'")
day <- factor(mydata[,3])
functkey <- as.character(mydata[,1])
mylist <- tapply(functkey,day,paste,collapse=" ")
mylist[[1]]	
#as.list(mylist)
mylist <- as.character(mylist)

# convert mylist to a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(mylist))

# Write a Corpus to Disk
#writeCorpus(myCorpus,path="./kingdee/",filenames=paste("kingdee",seq_along(myCorpus),".txt",sep=""))

# remove punctuation  
myCorpus <- tm_map(myCorpus, removePunctuation)
# convert to lower case  
myCorpus <- tm_map(myCorpus, content_transformer(tolower))  
#myCorpus <- Corpus(VectorSource(myCorpus))
myCorpus[[1]]
myCorpus[1:5]$content

 
myTdm <- TermDocumentMatrix(myCorpus)
myTdm
inspect(myTdm[1:37,1:40])
# 转化为数据框
#mydf <- as.data.frame(inspect(myTdm[1:37,1:40]))
# 转化为矩阵
#mydf <- as.matrix(tdm)

# find frequent terms with a frequency bound
findFreqTerms(myTdm,10)
# or
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
# 绘制展示词频的条形图
barplot(termFrequency, las=2)

# find associations with a lower correlation
findAssocs(myTdm,"main00001",0.5)

# Remove sparse terms with the maximal allowed sparsity
tdm <- removeSparseTerms(myTdm, 0.2)
inspect(tdm)
# 转化为数据框
#mydf <- as.data.frame(inspect(tdm))

#extract a document-term matrix with the special terms
inspect(DocumentTermMatrix(myCorpus, control = list(dictionary = c("main00001","a00202"))))

# 查看前6个以“c00301”开头、第31~40列的词项
idx <- which(myTdm$dimnames$Terms == "c00301")
inspect(myTdm[idx+(0:5),31:40])

# 查看词项的行名和列名
rownames(myTdm)
colnames(myTdm)

###################################################
### wordcloud
###################################################
library(wordcloud)
m <- as.matrix(myTdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
set.seed(375) # to make it reproducible
if(require(RColorBrewer)){
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
wordcloud(words=names(wordFreq), freq=wordFreq,c(4,.5),2,,F,,.15,pal)
}

# or
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,
          colors=grayLevels)

###################################################
### Hierarchical Clust
###################################################
# remove sparse terms
myDtm <- DocumentTermMatrix(myCorpus)
myDtm2 <- removeSparseTerms(myDtm, sparse=0.95)
m2 <- as.matrix(myDtm2)
# cluster terms
# method must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
distMatrix <- dist(scale(m2),method = "euclidean")
# method should be one of "ward.D", "ward.D2", "single", "complete", 
# "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
fit

rect.hclust(fit, k=3)
# or
rect.hclust(fit, h=20)

groups <- cutree(fit, k=3)
# or
groups <- cutree(fit, h=18)
groups
table(groups)

sapply(unique(groups),function(g) rownames(m2)[groups==g])

#
#pamRes<-pamk(m2,metric="manhattan")
