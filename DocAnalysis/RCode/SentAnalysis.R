library(stringr)
library(lubridate)
library(twitteR)
library(ROAuth)
library(httpuv)
library(rtweet)
library(devtools)
library(rlang)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(syuzhet)
library(fansi)
library(gganimate)
library(tweenr)
library(gifski)
library(png)
library(plotly)

#devtools::install_github('thomasp85/gganimate')
#install.packages("gifski")
#install.packages("png")

setwd("D:/Universita/Statistical Learning/Progetto")
dizsent <- read_csv("dizsent.csv", locale = locale(encoding = "ISO-8859-1"))[, -1]
dizxml <- read_csv("dizxml.csv", locale = locale(encoding = "ISO-8859-1"))[, -1]
diznrc <- read_csv("diznrc.csv", locale = locale(encoding = "ISO-8859-1"))[, -1]
renzi <-  read_csv("MRenzitweetsdef.csv")
salvini <- read_csv("Salvinifinaledef.csv")
dimaio <- read_csv("DiMaiotweetsdef.csv")
berlusconi <- read_csv("Berlusconitweetsdef.csv")
grasso <- read_csv("Grassotweetsdef.csv")
grillo <- read_csv("Grillofinaledef.csv")
martina <- read_csv("Martinatweetsdef.csv")
meloni <- read_csv("Melonitweetsdef.csv")

#####tokenizzare####
stopwords <- c("di", "a", "da", "in", "con", "su", "per", "tra", "fra", "un", "uno", "una", "uni", "a", "e", "i", "o", "u", "la", 
               "li", "le", "lo", "gli", "dello", "dei", "della", "delle", "ai", "al", "allo", "alle", "il")
stopwords <- data.frame(words = stopwords) %>% 
  tbl_df()

remove_reg <- "&amp;|&lt;|&gt;"
rentw <- renzi %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

dimtw <- dimaio %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

saltw <- salvini %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

bertw <- berlusconi %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

gratw <- grasso %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

gritw <- grillo %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

martw <- martina %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))

meltw <- meloni %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stopwords$words,
         str_detect(word, "[a-z]")) %>% 
  mutate(word = str_remove(word, "#")) %>% 
  filter(is_retweet == FALSE) %>% 
  subset( created_at >= "2017-01-01" & created_at <= "2018-12-31" ) %>% 
  filter(!str_detect(word, "<u")) %>% 
  filter(!str_detect(word,"^http"))




veloce <-  function(parole, metodo) {
  if (metodo == "xml"){
  valori <- dizxml %>% filter(Lemma %in% parole )
  a <- data.frame(Lemma = parole, ordine = 1:length(parole) ) %>% 
    tbl_df()
  neutri <- data.frame( Lemma = parole[parole %in% dizxml$Lemma == FALSE] %>% 
                          unique(), score = 0, metodo = NA, polarity=NA, ID = NA, POS = NA) %>% 
    tbl_df()
  mer <- rbind(valori, neutri)
  finito <- merge(a, mer, by = "Lemma") %>% 
    tbl_df()
  finito <- finito[order(finito$ordine), ]
  
  }
  if (metodo == "sentix"){
    valori <- dizsent %>% filter(Lemma %in% parole )
    a <- data.frame(Lemma = parole, ordine = 1:length(parole) ) %>% 
      tbl_df()
    neutri <- data.frame( Lemma = parole[parole %in% dizsent$Lemma == FALSE] %>% 
                            unique(), positivi = 0, negativi = 0, ID = NA, POS = NA, intensita = 0, polarity = 0) %>% 
      tbl_df()
    mer <- rbind(valori, neutri)
    finito <- merge(a, mer, by = "Lemma") %>% 
      tbl_df() 
    finito <- finito[order(finito$ordine), ]
  
  }
  if (metodo == "nrc"){
  valori <- diznrc %>% filter(Lemma %in% parole )
  a <- data.frame(Lemma = parole, ordine = 1:length(parole) ) %>% 
    tbl_df()
  neutri <- data.frame( Lemma = parole[parole %in% diznrc$Lemma == FALSE] %>% 
                          unique(), positivi = 0, negativi = 0, anger = 0, anticipation = 0, disgust= 0, fear = 0, joy = 0, sadness = 0,
                        surprise = 0, trust = 0, sentiment = "neutral") %>% 
    tbl_df()
  mer <- rbind(valori, neutri)
  finito <- merge(a, mer, by = "Lemma") %>% 
    tbl_df() 
  finito <- finito[order(finito$ordine), ]
  }
  return(finito)
}

# risultato1 <- veloce(rentw$word, metodo = "nrc")
# risultato2 <- veloce(rentw$word, metodo = "xml")
# risultato3 <- veloce(rentw$word, metodo = "sentix")

######nrc###########
risrenz <- veloce(rentw$word, metodo = "nrc") %>% 
  cbind(rentw)
risdim <- veloce(dimtw$word, metodo = "nrc") %>% 
  cbind(dimtw)
rissal <- veloce (saltw$word, metodo = "nrc") %>% 
  cbind(saltw)
risber <- veloce (bertw$word, metodo = "nrc") %>% 
  cbind(bertw)

# grafren <- graf1 %>% group_by(settimane = floor_date(created_at, "week")) %>% 
#   filter(!sentiment == "neutral") %>% 
#   summarise(positivi = mean(positivi), negativi = mean(negativi), anger = mean(anger), anticipation = mean(anticipation), disgust = mean(disgust),
#             fear = mean(fear), joy = mean(joy), sadness = mean(sadness), surprise = mean(surprise), trust = mean(trust))

#animazione renzi
sentimenti<-  risrenz %>%  group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise( anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
                         fear = sum(fear), joy = sum(joy), sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust),positive = sum(positivi),negative=sum(negativi))
ciao <- apply(sentimenti[,-1], 1, t)
colonne <- colSums(ciao)
for (i  in 1:24) {
  ciao [, i] <- ciao[, i] / colonne[i]
  }
finale <- data.frame( nomi = names(sentimenti)[-1], valori = as.vector(ciao), 
                      tempo = rep(sentimenti$mesi, each = 10) %>% as.character(), stringsAsFactors = F)
finale$tempo <- as.POSIXct(finale$tempo)

finale %>% tbl_df() %>% 
  ggplot(aes(x = tempo, y = valori,color=nomi)) +
  geom_point() +
  geom_smooth( )  +
  facet_wrap(~ nomi, scale = "free_y", nrow = 2) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        axis.text = element_text(size = 9),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#fff3e6"),
        plot.background = element_rect(fill = "#fff3e6")) 

head(finale)

str(finale)

gg1 <- ggplot(finale) +
  geom_histogram(aes(x =nomi, y= valori, fill= nomi), stat = "identity")+
  transition_states(states=tempo,
                    transition_length = 1,
                    state_length = 1)+
  ease_aes('linear') +
  labs(title="Mese: {closest_state}")
animate(gg1, fps = 15, duration = 50)


#animazione dimaio
sentimenti<-  risdim %>%  group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise( anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
             fear = sum(fear), joy = sum(joy), sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust))
ciao <- apply(sentimenti[,-1], 1, t)
colonne <- colSums(ciao)
for (i  in 1:24) {
  ciao [, i] <- ciao[, i] / colonne[i]
}
finale <- data.frame( nomi = names(sentimenti)[-1], valori = as.vector(ciao), 
                      tempo = rep(sentimenti$mesi, each = 8) %>% as.character(), stringsAsFactors = F)

finale %>%
  ggplot(aes(x = tempo, y = valori, color = nomi)) +
  geom_point() +
  facet_wrap(~ nomi, scale = "free_y", nrow = 3) +
  theme_bw() +
  geom_smooth(formula = nomi~tempo )  +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 9),
        legend.title = element_blank()) 



gg1 <- ggplot(finale) +
  geom_histogram(aes(x =nomi, y= valori, fill= nomi), stat = "identity")+
  transition_states(states=tempo,
                    transition_length = 1,
                    state_length = 1)+
  ease_aes('linear') +
  labs(title="Mese: {closest_state}")


#animazione salv
sentimenti<-  rissal %>%  group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise( anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
             fear = sum(fear), joy = sum(joy), sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust))
ciao <- apply(sentimenti[,-1], 1, t)
colonne <- colSums(ciao)
for (i  in 1:24) {
  ciao [, i] <- ciao[, i] / colonne[i]
}
finale <- data.frame( nomi = names(sentimenti)[-1], valori = as.vector(ciao), 
                      tempo = rep(sentimenti$mesi, each = 8) %>% as.character(), stringsAsFactors = F)

finale %>%
  ggplot(aes(x = tempo, y = valori)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ nomi, scale = "free_y", nrow = 3) +
  theme_bw() +
  geom_smooth(formula = nomi~tempo )  +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 9),
        legend.title = element_blank()) 



gg1 <- ggplot(finale) +
  geom_histogram(aes(x =nomi, y= valori, fill= nomi), stat = "identity")+
  transition_states(states=tempo,
                    transition_length = 1,
                    state_length = 1)+
  ease_aes('linear') +
  labs(title="Mese: {closest_state}")

#animazione ber
sentimenti<-  risber %>%  group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise( anger = sum(anger), anticipation = sum(anticipation), disgust = sum(disgust),
             fear = sum(fear), joy = sum(joy), sadness = sum(sadness), surprise = sum(surprise), trust = sum(trust))
ciao <- apply(sentimenti[,-1], 1, t)
colonne <- colSums(ciao)
for (i  in 1:24) {
  ciao [, i] <- ciao[, i] / colonne[i]
}
finale <- data.frame( nomi = names(sentimenti)[-1], valori = as.vector(ciao), 
                      tempo = rep(sentimenti$mesi, each = 8) %>% as.character(), stringsAsFactors = F)

finale %>%
  ggplot(aes(x = tempo, y = valori, color = nomi)) +
  geom_point() +
  facet_wrap(~ nomi, scale = "free_y", nrow = 3) +
  theme_bw() +
  geom_smooth(formula = nomi~tempo )  +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 9),
        legend.title = element_blank()) 



gg1 <- ggplot(finale) +
  geom_histogram(aes(x =nomi, y= valori, fill= nomi), stat = "identity")+
  transition_states(states=tempo,
                    transition_length = 1,
                    state_length = 1)+
  ease_aes('linear') +
  labs(title="Mese: {closest_state}")




##############grafica#########
risrenz <- veloce(rentw$word, metodo = "sentix") %>% 
  cbind(rentw)
risdim <- veloce(dimtw$word, metodo = "sentix") %>% 
  cbind(dimtw)
rissal <- veloce (saltw$word, metodo = "sentix") %>% 
  cbind(saltw)
risber <- veloce (bertw$word, metodo = "sentix") %>% 
  cbind(bertw)
risgra <- veloce (gratw$word, metodo = "sentix") %>% 
  cbind(gratw)
risgri <- veloce (gritw$word, metodo = "sentix") %>% 
  cbind(gritw)
rismar <- veloce (martw$word, metodo = "sentix") %>% 
  cbind(martw)
rismel <- veloce (meltw$word, metodo = "sentix") %>% 
  cbind(meltw)


# graf1 <- cbind(risrenz, rentw)
# graf2 <- cbind(risdim, dimtw)
# graf3 <- cbind(rissal, saltw)


grafren <- risrenz %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafdim <- risdim %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafsal <- rissal %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafber <- risber %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafgra <- risgra %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafgri <- risgri %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafmar <- rismar %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 
grafmel <- rismel %>% group_by(mesi = floor_date(created_at, "month")) %>% 
  summarise(polarity = mean(polarity), positivi = mean(positivi), negativi = mean(negativi), intensita = mean(intensita)) 

a <- data.frame(mesi = grafren$mesi[grafren$mesi %in% grafber$mesi == FALSE ], polarity = 0, positivi = 0, negativi = 0, intensita = 0) %>% 
  tbl_df()
grafber <- grafber %>% rbind(a)
grafber <- grafber[order(grafber$mesi), ]

grafico <- merge(grafren, grafdim,  by = "mesi") %>% 
  merge(grafsal, by = "mesi") %>% 
  merge(grafber, by = "mesi") %>% 
  merge(grafgra, by = "mesi") %>% 
  merge(grafgri, by = "mesi") %>% 
  merge(grafmar, by = "mesi") %>% 
  merge(grafmel, by = "mesi") 


names(grafico) <- c("settimane", "polre", "posre", "negre", "intre", "poldi", "posdi", "negdi", "intdi", "polsa", "possa", "negsa", "intsa",
                    "polbe", "posbe", "negbe", "intbe", "polgra", "posgra", "neggra", "intgra", "polgri", "posgri", "neggri", "intgri",
                    "polma", "posma", "negma", "intma", "polme", "posme", "negme", "intme")

plot_ly(grafico, x = ~settimane, y = ~polre, name="Renzi",
        type = "scatter", mode = "lines", line = list(color = "#ff8080")) %>%
  add_trace(y= ~ poldi, name="Dimaio", type = "scatter",selected=F, line = list(color = "#ffcc00")) %>%
  add_trace(y = ~ polsa, name = "Salvini", type = "scatter", selected = F, line = list(color = "#009933")) %>% 
  add_trace(y = ~ polbe, name = "Berlusconi", type = "scatter", selected = F, line = list(color = "#66ccff")) %>% 
  add_trace(y = ~ polgri, name = "Grillo", type = "scatter", selected = F, line = list(color = "#ac8a00")) %>% 
  add_trace(y = ~ polgra, name = "Grasso", type = "scatter", selected = F, line = list(color = "#cc0000")) %>% 
  add_trace(y = ~ polma, name = "Martina", type = "scatter", selected = F, line = list(color = "#ffb3b3")) %>% 
  add_trace(y = ~ polme, name = "Meloni", type = "scatter", selected = F, line = list(color = "#0000ff")) %>% 
  layout(xaxis = list(title = "Data"), yaxis = list(title = "Valore"),  title = "Polarity", plot_bgcolor = "#fff3e6", paper_bgcolor="#fff3e6") 

plot_ly(grafico, x = ~settimane, y = ~posre, name="Renzi",
        type = "scatter", mode = "lines", line = list(color = "red")) %>%
  add_trace(y= ~ posdi, name="Dimaio", type = "scatter",selected=F, line = list(color = "yellow")) %>%
  add_trace(y = ~ possa, name = "Salvini", type = "scatter", selected = F, line = list(color = "green")) %>% 
  add_trace(y = ~ posbe, name = "Berlusconi", type = "scatter", selected = F, line = list(color = "lightblue")) %>% 
  add_trace(y = ~ posgri, name = "Grillo", type = "scatter", selected = F, line = list(color = "white")) %>% 
  add_trace(y = ~ posgra, name = "Grasso", type = "scatter", selected = F, line = list(color = "orange")) %>% 
  add_trace(y = ~ posma, name = "Martina", type = "scatter", selected = F, line = list(color = "pink")) %>% 
  add_trace(y = ~ posme, name = "Meloni", type = "scatter", selected = F, line = list(color = "blue")) %>% 
  layout(xaxis = list(title = "Data"), yaxis = list(title = "Valore"),  title = "Positivity", plot_bgcolor = "black") 

plot_ly(grafico, x = ~settimane, y = ~negre, name="Renzi",
        type = "scatter", mode = "lines", line = list(color = "red")) %>%
  add_trace(y= ~ negdi, name="Dimaio", type = "scatter",selected=F, line = list(color = "yellow")) %>%
  add_trace(y = ~ negsa, name = "Salvini", type = "scatter", selected = F, line = list(color = "green")) %>% 
  add_trace(y = ~ negbe, name = "Berlusconi", type = "scatter", selected = F, line = list(color = "lightblue")) %>% 
  add_trace(y = ~ neggri, name = "Grillo", type = "scatter", selected = F, line = list(color = "white")) %>% 
  add_trace(y = ~ neggra, name = "Grasso", type = "scatter", selected = F, line = list(color = "orange")) %>% 
  add_trace(y = ~ negma, name = "Martina", type = "scatter", selected = F, line = list(color = "pink")) %>% 
  add_trace(y = ~ negme, name = "Meloni", type = "scatter", selected = F, line = list(color = "blue")) %>% 
  layout(xaxis = list(title = "Data"), yaxis = list(title = "Valore"),  title = "Negativity", plot_bgcolor = "black") 

plot_ly(grafico, x = ~settimane, y = ~intre, name="Renzi",
        type = "scatter", mode = "lines", line = list(color = "red")) %>%
  add_trace(y= ~ intdi, name="Dimaio", type = "scatter",selected=F, line = list(color = "yellow")) %>%
  add_trace(y = ~ intsa, name = "Salvini", type = "scatter", selected = F, line = list(color = "green")) %>% 
  add_trace(y = ~ intbe, name = "Berlusconi", type = "scatter", selected = F, line = list(color = "lightblue")) %>% 
  add_trace(y = ~ intgri, name = "Grillo", type = "scatter", selected = F, line = list(color = "white")) %>% 
  add_trace(y = ~ intgra, name = "Grasso", type = "scatter", selected = F, line = list(color = "orange")) %>% 
  add_trace(y = ~ intma, name = "Martina", type = "scatter", selected = F, line = list(color = "pink")) %>% 
  add_trace(y = ~ intme, name = "Meloni", type = "scatter", selected = F, line = list(color = "blue")) %>% 
  layout(xaxis = list(title = "Data"), yaxis = list(title = "Valore"),  title = "Intensità", plot_bgcolor = "black") 





#conta parole #
dimtw$word[1:9000] 


a <- risren %>%  group_by(mesi = floor_date(created_at, "month")) %>% summarise(conta=length(negativi))

  
