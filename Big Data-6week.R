##6week
#텍스트마이닝/자연어처리(NLP)
emily_text<-c("Because I could not stop for Death -","He kindly stopped for me -","The Carriage held but just Ourseleves -","and Immortality")
emily_text
library(dplyr)
text_df<-tibble(line=1:4,text=emily_text)
text_df
install.packages('tidytext')
library(tidytext)
text_df %>% unnest_tokens(word,text,to_lower = T)
library(janeaustenr)
library(stringr)
original_book<-austen_books() %>% group_by(book) %>% mutate(linenumber=row_number(),chapter=cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case=T)))) %>% ungroup()
original_book
data(stop_words)
tidy_books<-original_book %>% unnest_tokens(input=text,output = "word")
tidy_books
tidy_books<-tidy_books %>% anti_join(stop_words)
tidy_books %>% count(word,sort=T)
library(ggplot2)
tidy_books %>% count(word,sort = T) %>% filter(n>600) %>% mutate(word=reorder(word, n)) %>% ggplot(aes(word, n))+geom_col()+xlab(NULL)+coord_flip()
library(gutenbergr)
hgwells<-gutenberg_download(c(35,36,5230,159))
bronte<-gutenberg_download(c(1260,768,969,9182,767))
tidy_hgwells<-hgwells %>% unnest_tokens(word,text) %>% anti_join(stop_words)
tidy_hgwells %>% count(word,sort=T)
tidy_bronte<-bronte %>% unnest_tokens(word,text) %>% anti_join(stop_words)
tidy_bronte %>% count(word,sort=T)
#
library(dplyr)
library(janeaustenr)
library(tidytext)
book_words<-austen_books() %>% unnest_tokens(word,text) %>% count(book,word,sort=T) %>% ungroup()
book_words
total_words<-book_words %>% group_by(book) %>% summarize(total=sum(n))
total_words
book_words<-left_join(book_words,total_words)
book_words
library(ggplot2)
ggplot(book_words,aes(n/total,fill=book))+geom_histogram(show.legend = F)+xlim(NA,0.0009)+facet_wrap(~book,ncol=3,scales = "free_y")






































