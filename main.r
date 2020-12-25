
# REQUIRED PACKAGES

library(plotly)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)
# EN CASO DE NO CONTAR CON "ggradar": 
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(gridExtra)
library(igraph)
library(ggraph)

# EXTRACT FROM GUTEMBERG PROJECT: THE BIBLE, BOOK OF MORMON, DHAMMAPADA (BUDDISM) AND QURAN 

theBible <- gutenberg_download(30)
names(theBible) <- c("book","text")
theBible$book <- "The Bible"

bookMormon <- gutenberg_download(17)
names(bookMormon) <- c("book","text")
bookMormon$book<- "The Book of Mormon"

dhammapada <- gutenberg_download(35185)
names(dhammapada) <- c("book","text")
dhammapada$book<- "Dhammapada"

theQuran <- gutenberg_download(7440)
names(theQuran) <- c("book","text")
theQuran$book<- "The Quran"

# CREATE A SINGLE DATA SET AND SAVE INTO RDATA FILE

allHolybooks <- rbind(theBible,bookMormon,dhammapada,theQuran)
allHolybooks <- allHolybooks[allHolybooks$text !="",]
allHolybooks$text <- gsub('[[:punct:]]|[[:digit:]]','',allHolybooks$text)

rm(theBible,dhammapada,theQuran,bookMormon)

save(list = ls(),file = "allHolybooks.Rdata")

# LOAD THE NEW DATA SET, CLEAN AND SEPARATE WORDS 

load("allHolybooks.Rdata")

cleanHolybooks <- allHolybooks %>%
  group_by(book) %>%
  mutate(numberLine = row_number()) %>%
  ungroup()

vocabulary <- cleanHolybooks %>%
  unnest_tokens(word, text)

vocabulary<- filter(vocabulary, nchar(vocabulary$word)>2)

vocabulary<- vocabulary %>%
  group_by(book) %>%
  mutate(idx = round(100*(numberLine/max(numberLine)),0))

data(stop_words)

cleanTerms <- vocabulary %>%
  anti_join(stop_words)

# PLOT MOST REPEATED WORDS IN ALL HOLY BOOKS

cleanTerms %>%
  count(word, sort = TRUE) %>%
  filter(n > 2300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_bar(stat = "identity", fill="steelblue")+ 
  ylab("word") + xlab("# times repeated") +
  ggtitle("Most repeated words in all Holy Books", "> 2500 times") +
  coord_flip()

ggplotly()

# SENTIMENT INDEXING BY POSITIVE AND NEGATIVE 

booksSentiments <- vocabulary %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = idx, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

booksSentiments<- booksSentiments %>%
  group_by(book) %>%
  mutate(centeredSentiment = as.numeric(scale(sentiment)))

# PLOT SENTIMENT FLOW HOLY BOOKS

ggplot(booksSentiments, aes(index, centeredSentiment, fill=as.factor(sentiment>0))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_segment(mapping=aes(x=0, y=0, xend=103, yend=0), 
               arrow=arrow(angle = 30,length = unit(0.05, "inches"), 
                           ends = "last", type = "closed"), 
               size=0.05, color="#5b5b5b") +
  facet_wrap(~book, ncol = 1) +
  scale_x_continuous(label=function(x){return(paste0(x, "%"))}) +  
  scale_fill_manual("",values = c("#cf0a00","#1a954d")) +
  labs(x= expression("book trajectory from beginning to the end"),
       y= "sentiments") +
  ggtitle("Sentiment flow in Holy Books", "Scaled and centered\n") +
  theme_bw() +
  theme(legend.position="none",
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#f9f9f9"),
        strip.text.x = element_text(size=11,hjust=0.05,face="plain"),
        strip.background = element_blank())

ggplotly()

# LARGEST POSITIVE AND NEGATIVE SENTIMENT WORD CLOUD BY EACH HOLY BOOK

cleanTerms %>%
  filter(book=="The Bible") %>%
  filter(idx=="53") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#cf0a00","#1a954d"),
                   random.order=FALSE,
                   rot.per=0,
                   max.words = 100)

cleanTerms %>%
  filter(book=="Dhammapada") %>%
  filter(idx=="53") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#cf0a00","#1a954d"),
                   random.order=FALSE,
                   rot.per=0,
                   max.words = 100)

cleanTerms %>%
  filter(book=="The Quran") %>%
  filter(idx=="53") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#cf0a00","#1a954d"),
                   random.order=FALSE,
                   rot.per=0,
                   max.words = 100)

cleanTerms %>%
  filter(book=="The Book of Mormon") %>%
  filter(idx=="53") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#cf0a00","#1a954d"),
                   random.order=FALSE,
                   rot.per=0,
                   max.words = 100)

# NEGATIVE AND POSITIVE SENTIMENTS IN PERCENTAGE

bngNegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

bngPositive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

termCount <- cleanTerms %>%
  group_by(book) %>%
  summarize(words = n())

negTermCount<-cleanTerms %>%
  semi_join(bngNegative) %>%
  group_by(book) %>%
  summarize(negativewords = n()) %>%
  left_join(termCount, by = c("book")) %>%
  mutate(neg_words_percent = round(100*negativewords/words,2)) %>%
  ungroup

posTermCount<-cleanTerms %>%
  semi_join(bngPositive) %>%
  group_by(book) %>%
  summarize(positivewords = n()) %>%
  left_join(termCount, by = c("book")) %>%
  mutate(pos_words_percent = round(100*positivewords/words,2)) %>%
  ungroup

allSentiments<-merge(negTermCount,posTermCount)

# PLOT NEGATIVE AND POSITIVE ENTIMENTS IN PERCENTAGE IN EACH HOLY BOOK

ggplot(melt(allSentiments[,c(1,4,6)]),
       aes(x=book, value, fill=variable,  width = 0.5)) +
  geom_bar(stat = "identity",position = position_dodge(width=0.5)) +
  labs(x= "Holy Book",y= "percentage of terms") +
  ggtitle("Negative and Positive word sentiments", "In percentage") +
  scale_fill_manual("",values = c("#cf0a00","#1a954d"),
                    labels= c("Negative Sentiments",
                              "Positive Sentiments")) +
  scale_y_continuous(label=function(y){return(paste0(y, "%"))}) 


ggplotly()

# TOP 10 SENTIMENTS

bookTerms <- vocabulary %>%
  group_by(book,word) %>%
  summarize(count=length(word)) %>%
  inner_join(get_sentiments("bing"), by = c(word = "word")) 

totalWords<- bookTerms %>% 
  group_by(book) %>% 
  summarize(total = sum(count))

bookTerms <- left_join(bookTerms,totalWords)

topSentiments<-bookTerms %>%
  count(book,sentiment, word, wt = count, sort = TRUE) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  group_by(book) %>%
  top_n(n=10,wt=abs(n))  %>%
  arrange(book,n) %>%
  ungroup () %>%
  mutate(order = row_number())

ggplot(topSentiments, aes(order, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free") +
  xlab("sentiment word") +
  ylab("sentiment score * # of occurrences") +
  theme_bw() +
  coord_flip() + 
  scale_x_continuous(
    breaks = topSentiments$order,
    labels = topSentiments$word,
    expand = c(0,0)) +
  ggtitle("Top 10 sentiments", "In each Holy Book\n") +
  coord_flip() +
  facet_wrap(~book,scales = "free",ncol=1)+
  scale_fill_manual("",values = c("#cf0a00","#1a954d"),labels= c("Negative\nSentiments","Positive\nSentiments")) +
  theme_bw() +
  theme(legend.position="none",
        panel.border = element_blank(),
        strip.text.x = element_text(size=11,hjust=0,face="plain"),
        )

ggplotly()

# MOST COMMON SENTIMENTS WITH GGRADAR

commonNegTerms<-bookTerms %>%
  group_by(book) %>%
  mutate(countpercent=count/sum(count)) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(wordimp= sum(countpercent)) %>%
  filter(sentiment=="negative") %>%
  group_by(book) %>%
  top_n(n=10,wt=wordimp) %>%
  arrange(book,wordimp) %>%
  select(book,word,countpercent) %>%
  dcast(book~word)

commonNegTerms[is.na(commonNegTerms)]<-0
x<-ggradar(commonNegTerms, grid.min = 0,
           grid.mid = 0.015,
           grid.max = 0.04,
           axis.label.offset = 1.1,
           axis.label.size = 4,
           grid.label.size = 0,
           group.line.width = 1,
           group.point.size = 1.5,
           background.circle.colour = "#ffbbcc",
           legend.text.size = 12,
           plot.legend = FALSE,
           plot.title = "Most common Negative sentiments \n in each Holy Book")+
  theme(legend.position = "bottom",
        plot.title=element_text(hjust=0.4,face = "plain"))+
  scale_colour_manual(values = rep(c("#fd8529","#3a76b0","#5b59d6","#56c9c4"), 100))

commonPosTerms<-bookTerms %>%
  group_by(book) %>%
  mutate(countpercent=count/sum(count)) %>%
  ungroup() %>%
  group_by(word) %>%
  mutate(wordimp= sum(countpercent)) %>%
  filter(sentiment=="positive") %>%
  group_by(book) %>%
  top_n(n=10,wt=wordimp) %>%
  arrange(book,wordimp) %>%
  select(book,word,countpercent) %>%
  dcast(book~word)

commonPosTerms[is.na(commonPosTerms)]<-0
y<-ggradar(commonPosTerms, grid.min = 0,
           grid.mid = 0.025,
           grid.max = 0.06,
           axis.label.offset = 1.1,
           axis.label.size = 4,
           grid.label.size = 0,
           group.line.width = 1,
           group.point.size = 1.5,
           background.circle.colour = "#ffbbcc",
           legend.text.size = 12,
           plot.legend = FALSE,
           plot.title = "Most common Positive sentiments \n in each Holy Book")+
  theme(legend.position = "bottom",
        plot.title=element_text(hjust=0.4,face = "plain"),
        axis.title = element_text(face = "plain"))+
  scale_colour_manual(values = rep(c("#fd8529","#3a76b0","#5b59d6","#56c9c4"), 100))

tmp <- arrangeGrob(x + theme(legend.position = "none"), y + 
                     theme(legend.position = "none"), layout_matrix = matrix(c(1, 2), nrow = 2))

g <- ggplotGrob(y + theme(legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

grid.arrange(tmp, legend, ncol=2, widths=c(9,6))

# TOP 20 UNIQUE WORDS WITH TF-IDF 

bookTerms2<- cleanTerms %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

bookTerms2 <- left_join(bookTerms2,
                        bookTerms2 %>%
                        group_by(book) %>% 
                        summarize(total = sum(n)))

bookTermsTfIdf <- bookTerms2 %>%
  bind_tf_idf(word, book, n)

bookTermsTfIdf <- anti_join(bookTermsTfIdf , bookTermsTfIdf [duplicated(bookTermsTfIdf[2]),], by="word")

plotTfIdf <- bookTermsTfIdf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

uniqueWords <- plotTfIdf %>% 
  group_by(book) %>% 
  top_n(20) %>% 
  ungroup

ggplot(uniqueWords, aes(word, tf_idf, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 4, scales = "free") +
  coord_flip()+
  scale_fill_manual("",values = c("#fd8529","#3a76b0","#5b59d6","#56c9c4")) +
  labs(x= "word", y= "Tf-Idf") +
  ggtitle("Top 20 unique words", "In each Holy Book \n ") +
  theme(
        panel.background = element_blank(),
        strip.text.x = element_text(size=11,hjust=0.05,face="plain")
        )

# BIGRAM SEMANTIC NETWORK

holyBooksBigrams <- allHolybooks %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

holyBooksBigrams<- filter(holyBooksBigrams, nchar(holyBooksBigrams$bigram)>2)

separatedBigrams <- holyBooksBigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

filteredBigrams <- separatedBigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

countsBigram <- filteredBigrams %>% 
  count(book,word1, word2, sort = TRUE)

countsBigram<- countsBigram[,c(2,3,4,1)]

countsBigramPlot<- countsBigram %>%
  group_by(book) %>%
  top_n(25,wt=n)%>%
  filter (n>2) %>%
  ungroup()

set_graph_style()

plotGraph<- function(df,book,colorname){
  
  set.seed(123)
  
  bigramPlot <- df[df$book==book,] %>%
    graph_from_data_frame()
  
  gh<-df[df$book==book,]
  
  names(gh)<-c("word","word","n","book")
  tt<- rbind(gh[,c(1,4)],gh[,c(2,4)])
  pk<-tt[match(unique(tt$word), tt$word),]
  
  V(bigramPlot)$class<-pk$book
  
  a <- grid::arrow(type = "closed", length = unit(.10, "inches"))
  
  p<- ggraph(bigramPlot, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n),arrow = a) +
    geom_node_point(size = 1,colour = colorname) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
    ggtitle(book, "Semantic network") +
    th_foreground(foreground = 'grey', border = F)+
    theme(legend.position="none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))
  
  return (p)
}

bigram1<-plotGraph(countsBigramPlot,"Dhammapada",colorname="#FB7020")
bigram2<-plotGraph(countsBigramPlot,"The Bible",colorname="#2E61A0")
bigram3<-plotGraph(countsBigramPlot,"The Quran",colorname="#49C0B7")
bigram4<-plotGraph(countsBigramPlot,"The Book of Mormon",colorname="#493FCC")

grid.arrange(bigram1, bigram2, bigram3, bigram4, ncol=2)
