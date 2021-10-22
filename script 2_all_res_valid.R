library(readr)
library(openxlsx)
library(tidyverse)

#migr_resvalid - All valid permits by reason, length of validity and citizenship 31 December of each year
residence2 <- read_csv("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/migr_resvalid_residence_all valid_2 (2).csv")
siglas <- read.xlsx("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/siglas.xlsx")


residence2 <- left_join(residence2, siglas, by = c("CITIZEN" = "SIGLA"))

residence2$Value <- gsub(",", "", residence2$Value)
typeof(residence2$Value)
residence2$Value <- as.numeric(residence2$Value)


# Education reasons

residence2 %>%
  select(TIME, REASON, REGIÃO, Value) %>% 
  filter(Value != ":",
         REASON == "Education reasons") %>% 
  group_by(TIME) %>% 
  summarise(sum(Value))

            