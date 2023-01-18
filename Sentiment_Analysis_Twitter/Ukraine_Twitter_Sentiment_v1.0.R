# Análise de Dados do Twitter através de WebScraping

# Configurando ambiente

setwd("G:/Cursos/DataScience/DSA/Formacao Cientista de Dados/04.BigDataAnalyticscomRMicrosoftAzureMachineLearning/Projeto01")

# Pacotes essenciais
library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(knitr)
library(rmarkdown)

# Autenticação e manipulação Twitter
library(rtweet)
library(httr)

# Text mining
library(SnowballC)
library(tm)

# Visualização Nuvem de Palavras
library(wordcloud)
library(RColorBrewer)

# Manipulação de texto
library(stringr)

# Classificação de emoções
library(syuzhet)
library(textdata)
library(tidytext)

# Autenticando no Twitter (MUITO IMPORTANTE TER ELEVATED ACCESS NA APP)

bearer_token <- "****************************"

bearer_token <- "AAAAAAAAAAAAAAAAAAAAAOAWlQEAAAAA3kqVVwMXWt226LrltenoWIOFDbQ%3DK3ov3QiqMAGDWqmZw9KkHwzmaAHlVlIOmVrSLxofenMbGa0Vb9"

my_app <- rtweet_app(bearer_token)
auth_as(my_app)

# Capturando tweets sobre o tema alvo: Guerra da Ucrânia
tweetdata <- search_tweets("#Ukraine", 
                           n = 500,
                           include_rts = F,
                           type = "recent",
                           parse = T,
                           lang = "en")
head(tweetdata)

# Iniciando o Text Mining

# Tratando os Dados com Expressões Regulares
tweetList <- tweetdata$text # Obtendo o texto dos tweets

# Removendo caracteres especiais
tweetList = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetList)

# Removendo @
tweetList = gsub("@\\w+", "", tweetList)

# Removendo pontuação
tweetList = gsub("[[:punct:]]", "", tweetList)

# Removendo digitos
tweetList = gsub("[[:digit:]]", "", tweetList)

# Removendo links html
tweetList = gsub("http\\w+", "", tweetList) 

# Removendo espacos desnecessários
tweetList = gsub("[ \t]{2,}", "", tweetList)
tweetList = gsub("^\\s+|\\s+$", "", tweetList)
tweetList = gsub("[\n]{2,}", " ", tweetList)


# Wordcloud
pal <- brewer.pal(8, "Dark2") #Criando paleta de cores

wordcloud(tweetList,
          scale = c(4,1),
          min.freq = 4,
          random.color = T,
          random.order = F,
          max.words = 100,
          colors = pal)


# ANÁLISE DE SENTIMENTO

sentiment.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  # Criando um array de scores
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence = gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Palavras positivas e negativas
pos = readLines("palavras_positivas.txt")
neg = readLines("palavras_negativas.txt")

# Criando massa de dados para teste
teste = c("Ukraine is winning", "Ukraine is losing badly", 
          "Russia is horrible", "Things are getting worse",
          "We need to make peace urgently")

# Testando função de pontos
testesentimento = sentiment.score(teste, pos, neg)

# Aplicando a função sentiment score nos tweets extraídos
scores = sentiment.score(tweetList, pos, neg, .progress = 'text')

# Analisando os resultados

scores$pos = as.numeric(scores$score >= 1)
scores$neg = as.numeric(scores$score >= -1)

numpos = sum(scores$pos)
numneg = sum(scores$neg)
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)

# Gerando gráfico de colunas para os sentimentos positivos/negativos
color <- ifelse(global_score >= 50,"darkgreen","darkred")
p <- ggplot(data = scores) + geom_bar(mapping = aes(x = score), fill = color)
p + xlim(-6,6)


# CLASSIFICAÇÃO DE EMOÇÕES

# Criando função para tolower
try.error = function(x)
{
  # Criando missing value
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# Lower case
tweetList = sapply(tweetList, try.error)
# Removendo os NAs
tweetList = tweetList [!is.na(tweetList)]
names(tweetList) = NULL



# Classifcando emoções com syuzhet
emotions <- get_nrc_sentiment(tweetList)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count = emo_bar, emotion = names(emo_bar))

# Barplot com a contagem de emoções
ggplot(emo_sum, aes(x = reorder(emotion, -count), y = count)) +
  geom_bar (stat = "identity")


# Palavras positivas e negativas mais comuns com tidytext

# Classifcando emoções com o Dicionário Bing
df <- as.data.frame(tweetList)

bing <- df %>% unnest_tokens(word, tweetList) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)

# Selecionando Top 10 positivas e negativas 
bing_top10 <-  bing %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))
bing_top10

# Plotando
bing_top10 %>% ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs (y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# Classifcando emoções com o dicionário Loughran

loughran <- df %>% unnest_tokens(word, tweetList) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word, sentiment, sort = TRUE)

# Selecionando Top 10 positivas e negativas 
loughran_top10 <-  loughran %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10, with_ties = F) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))
loughran_top10

# Plotando
loughran_top10 %>% ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs (y = "Contribution to sentiment", x = NULL) +
  coord_flip()

