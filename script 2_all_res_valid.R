library(readr)
library(openxlsx)
library(tidyverse)

#migr_resvalid - All valid permits by reason, length of validity and citizenship 31 December of each year
residence2 <- read_csv("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/migr_resvalid_residence_all valid_2.csv")
siglas <- read.xlsx("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/siglas.xlsx")


residence2 <- left_join(residence2, siglas, by = c("CITIZEN" = "SIGLA"))

residence2$Value <- gsub(",", "", residence2$Value)
typeof(residence2$Value)
residence2$Value <- as.numeric(residence2$Value)

unique(residence2$REASON)


# Education reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Education reasons") %>% 
  # group_by(TIME) %>%       # tirar comentário para calcular por ano. Deixar o group by faz o R somar o ano no cálculo.
  summarise(sum(Value)) %>% 
  sum()

# Family reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Family reasons") %>% 
  # group_by(TIME) %>%       # tirar comentário para calcular por ano. Deixar o group by faz o R somar o ano no cálculo.
  summarise(sum(Value)) %>% 
  sum()


# Remunerated activities reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Remunerated activities reasons") %>% 
  # group_by(TIME) %>%       # tirar comentário para calcular por ano. Deixar o group by faz o R somar o ano no cálculo.
  summarise(sum(Value)) %>% 
  sum()


# Refugee status reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Refugee status") %>% 
  # group_by(TIME) %>%       # tirar comentário para calcular por ano. Deixar o group by faz o R somar o ano no cálculo.
  summarise(sum(Value)) %>% 
  sum()


# Subsidiary protection reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Subsidiary protection") %>% 
 # group_by(TIME) %>%       # tirar comentário para calcular por ano. Deixar o group by faz o R somar o ano no cálculo.
  summarise(sum(Value)) %>%
  sum()


# Other reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Other") %>% 
  #group_by(TIME) %>%      # tirar comentário para calcular por ano. Deixar o group by faz o R somar o ano no cálculo.
  summarise(sum(Value)) %>% 
  sum()
            