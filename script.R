library(readr)

#migr_resvalid - All valid permits by reason, length of validity and citizenship 31 December of each year
residence <- read_csv("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/migr_resvalid_residence_all valid_.csv")

#migr_asydcfcta - First instance decisions on applications by citizenship, age and sex - annual 
asilo <- read_csv("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/migr_asydcfsta_asilo.csv")

#migr_asyresa - Resettled persons by age, sex and citizenship - annual data (rounded)
resettled <- read_csv("C:/Users/letic/Desktop/Pesquisas/Knocking-on-France-s-door/Dados/migr_asyresa_resettled.csv")

library(tidyverse)


# Criando variável de continente
asilo <- asilo %>% 
  mutate(continent = recode(citizen, "AO" = "Africa", 
                                     "CM" = "Africa", 
                                     "CF" = "Africa", 
                                     "TD" = "Africa", 
                                     "CG" = "Africa", 
                                     "CD" = "Africa", 
                                     "GQ" = "Africa", 
                                     "GA" = "Africa", 
                                     "ST" = "Africa", 
                                     "BI" = "Africa", 
                                     "KM" = "Africa", 
                                     "DJ" = "Africa", 
                                     "ER" = "Africa", 
                                     "ET" = "Africa", 
                                     "KE" = "Africa", 
                                     "MG" = "Africa", 
                                     "MW" = "Africa", 
                                     "MU" = "Africa", 
                                     "MZ" = "Africa", 
                                     "RW" = "Africa", 
                                     "SC" = "Africa", 
                                     "SO" = "Africa", 
                                     "UG" = "Africa", 
                                     "TZ" = "Africa", 
                                     "ZM" = "Africa",
                                     "ZW" = "Africa",
                                     "DZ" = "Africa", 
                                     "EG" = "Africa", 
                                     "LY" = "Africa", 
                                     "MA" = "Africa", 
                                     "SS" = "Africa", 
                                     "SD" = "Africa", 
                                     "TN" = "Africa", 
                                     "EH" = "Africa", 
                                     "BW" = "Africa", 
                                     "LS" = "Africa", 
                                     "NA" = "Africa", 
                                     "ZA" = "Africa", 
                                     "SZ" = "Africa", 
                                     "BJ" = "Africa", 
                                     "BF" = "Africa", 
                                     "CV" = "Africa", 
                                     "CI" = "Africa", 
                                     "GM" = "Africa", 
                                     "GH" = "Africa", 
                                     "GN" = "Africa", 
                                     "GW" = "Africa", 
                                     "LR" = "Africa", 
                                     "ML" = "Africa", 
                                     "MR" = "Africa", 
                                     "NE" = "Africa", 
                                     "NG" = "Africa", 
                                     "SN" = "Africa", 
                                     "SL" = "Africa", 
                                     "TG" = "Africa",
                                     "CA" = "America",
                                     "US" = "America",
                                     "AG" = "America",
                                     "BS" = "America",
                                     "BB" = "America",
                                     "CU" = "America",
                                     "DM" = "America",
                                     "DO" = "America",
                                     "GD" = "America",
                                     "HT" = "America",
                                     "JM" = "America",
                                     "KN" = "America",
                                     "LC" = "America",
                                     "VC" = "America",
                                     "TT" = "America",
                                     "BZ" = "America",
                                     "CR" = "America",
                                     "SV" = "America",
                                     "GT" = "America",
                                     "HN" = "America",
                                     "MX" = "America",
                                     "NI" = "America",
                                     "PA" = "America",
                                     "AR" = "America",
                                     "BO" = "America",
                                     "BR" = "America",
                                     "CL" = "America",
                                     "CO" = "America",
                                     "EC" = "America",
                                     "GY" = "America",
                                     "PY" = "America",
                                     "PE" = "America",
                                     "SR" = "America",
                                     "UY" = "America",
                                     "VE" = "America",
                                     "KZ" = "Asia",
                                     "KG" = "Asia",
                                     "TJ" = "Asia",
                                     "TM" = "Asia",
                                     "UZ" = "Asia",
                                     "CN" = "Asia",
                                     "JP" = "Asia",
                                     "MN" = "Asia",
                                     "KP" = "Asia",
                                     "KR" = "Asia",
                                     "TW" = "Asia",
                                     "AF" = "Asia",
                                     "BD" = "Asia",
                                     "BT" = "Asia",
                                     "IN" = "Asia",
                                     "IR" = "Asia",
                                     "MV" = "Asia",
                                     "NP" = "Asia",
                                     "PK" = "Asia",
                                     "LK" = "Asia",
                                     "BN" = "Asia",
                                     "KH" = "Asia",
                                     "ID" = "Asia",
                                     "LA" = "Asia",
                                     "MY" = "Asia",
                                     "MM" = "Asia",
                                     "PH" = "Asia",
                                     "SG" = "Asia",
                                     "TH" = "Asia",
                                     "TL" = "Asia",
                                     "VN" = "Asia",
                                     "AM" = "Asia",
                                     "AZ" = "Asia",
                                     "BH" = "Asia",
                                     "GE" = "Asia",
                                     "IQ" = "Asia",
                                     "IL" = "Asia",
                                     "JO" = "Asia",
                                     "KW" = "Asia",
                                     "LB" = "Asia",
                                     "PS" = "Asia",
                                     "OM" = "Asia",
                                     "QA" = "Asia",
                                     "SA" = "Asia",
                                     "SY" = "Asia",
                                     "AE" = "Asia",
                                     "YE" = "Asia",
                                     "AU" = "Oceania",
                                     "NZ" = "Oceania",
                                     "FJ" = "Oceania",
                                     "PG" = "Oceania",
                                     "SB" = "Oceania",
                                     "VU" = "Oceania",
                                     "KI" = "Oceania",
                                     "MH" = "Oceania",
                                     "FM" = "Oceania",
                                     "NR" = "Oceania",
                                     "PW" = "Oceania",
                                     "CK" = "Oceania",
                                     "WS" = "Oceania",
                                     "TO" = "Oceania",
                                     "TV" = "Oceania",
                                     "STLS" = "Stateless",
                                     "TOTAL" = "Total"))
asilo <- asilo %>% 
  mutate(magrebe = recode(citizen, "DZ" = "Magrebe",
                                   "LY" = "Magrebe",
                                   "MA" = "Magrebe",
                                   "TN" = "Magrebe"))

asilo$age_fator <- factor(asilo$age, levels = c("TOTAL", "Y_LT18", "Y18-34", "Y35-64", "Y_GE65"), labels = c("Total", "18-", "18-34", "35-64", "65+"))



resettled <- resettled %>% 
  mutate(continent = recode(citizen, "AO" = "Africa", 
                            "CM" = "Africa", 
                            "CF" = "Africa", 
                            "TD" = "Africa", 
                            "CG" = "Africa", 
                            "CD" = "Africa", 
                            "GQ" = "Africa", 
                            "GA" = "Africa", 
                            "ST" = "Africa", 
                            "BI" = "Africa", 
                            "KM" = "Africa", 
                            "DJ" = "Africa", 
                            "ER" = "Africa", 
                            "ET" = "Africa", 
                            "KE" = "Africa", 
                            "MG" = "Africa", 
                            "MW" = "Africa", 
                            "MU" = "Africa", 
                            "MZ" = "Africa", 
                            "RW" = "Africa", 
                            "SC" = "Africa", 
                            "SO" = "Africa", 
                            "UG" = "Africa", 
                            "TZ" = "Africa", 
                            "ZM" = "Africa",
                            "ZW" = "Africa",
                            "DZ" = "Africa", 
                            "EG" = "Africa", 
                            "LY" = "Africa", 
                            "MA" = "Africa", 
                            "SS" = "Africa", 
                            "SD" = "Africa", 
                            "TN" = "Africa", 
                            "EH" = "Africa", 
                            "BW" = "Africa", 
                            "LS" = "Africa", 
                            "NA" = "Africa", 
                            "ZA" = "Africa", 
                            "SZ" = "Africa", 
                            "BJ" = "Africa", 
                            "BF" = "Africa", 
                            "CV" = "Africa", 
                            "CI" = "Africa", 
                            "GM" = "Africa", 
                            "GH" = "Africa", 
                            "GN" = "Africa", 
                            "GW" = "Africa", 
                            "LR" = "Africa", 
                            "ML" = "Africa", 
                            "MR" = "Africa", 
                            "NE" = "Africa", 
                            "NG" = "Africa", 
                            "SN" = "Africa", 
                            "SL" = "Africa", 
                            "TG" = "Africa",
                            "CA" = "America",
                            "US" = "America",
                            "AG" = "America",
                            "BS" = "America",
                            "BB" = "America",
                            "CU" = "America",
                            "DM" = "America",
                            "DO" = "America",
                            "GD" = "America",
                            "HT" = "America",
                            "JM" = "America",
                            "KN" = "America",
                            "LC" = "America",
                            "VC" = "America",
                            "TT" = "America",
                            "BZ" = "America",
                            "CR" = "America",
                            "SV" = "America",
                            "GT" = "America",
                            "HN" = "America",
                            "MX" = "America",
                            "NI" = "America",
                            "PA" = "America",
                            "AR" = "America",
                            "BO" = "America",
                            "BR" = "America",
                            "CL" = "America",
                            "CO" = "America",
                            "EC" = "America",
                            "GY" = "America",
                            "PY" = "America",
                            "PE" = "America",
                            "SR" = "America",
                            "UY" = "America",
                            "VE" = "America",
                            "KZ" = "Asia",
                            "KG" = "Asia",
                            "TJ" = "Asia",
                            "TM" = "Asia",
                            "UZ" = "Asia",
                            "CN" = "Asia",
                            "JP" = "Asia",
                            "MN" = "Asia",
                            "KP" = "Asia",
                            "KR" = "Asia",
                            "TW" = "Asia",
                            "AF" = "Asia",
                            "BD" = "Asia",
                            "BT" = "Asia",
                            "IN" = "Asia",
                            "IR" = "Asia",
                            "MV" = "Asia",
                            "NP" = "Asia",
                            "PK" = "Asia",
                            "LK" = "Asia",
                            "BN" = "Asia",
                            "KH" = "Asia",
                            "ID" = "Asia",
                            "LA" = "Asia",
                            "MY" = "Asia",
                            "MM" = "Asia",
                            "PH" = "Asia",
                            "SG" = "Asia",
                            "TH" = "Asia",
                            "TL" = "Asia",
                            "VN" = "Asia",
                            "AM" = "Asia",
                            "AZ" = "Asia",
                            "BH" = "Asia",
                            "GE" = "Asia",
                            "IQ" = "Asia",
                            "IL" = "Asia",
                            "JO" = "Asia",
                            "KW" = "Asia",
                            "LB" = "Asia",
                            "PS" = "Asia",
                            "OM" = "Asia",
                            "QA" = "Asia",
                            "SA" = "Asia",
                            "SY" = "Asia",
                            "AE" = "Asia",
                            "YE" = "Asia",
                            "AU" = "Oceania",
                            "NZ" = "Oceania",
                            "FJ" = "Oceania",
                            "PG" = "Oceania",
                            "SB" = "Oceania",
                            "VU" = "Oceania",
                            "KI" = "Oceania",
                            "MH" = "Oceania",
                            "FM" = "Oceania",
                            "NR" = "Oceania",
                            "PW" = "Oceania",
                            "CK" = "Oceania",
                            "WS" = "Oceania",
                            "TO" = "Oceania",
                            "TV" = "Oceania",
                            "STLS" = "Stateless",
                            "TOTAL" = "Total"))
resettled <- resettled %>% 
  mutate(magrebe = recode(citizen, "DZ" = "Magrebe",
                          "LY" = "Magrebe",
                          "MA" = "Magrebe",
                          "TN" = "Magrebe"))

resettled$age_fator <- factor(resettled$age, levels = c("TOTAL", "Y_LT18", "Y18-34", "Y35-64", "Y_GE65"), labels = c("Total", "18-", "18-34", "35-64", "65+"))


residence <- residence %>% 
  mutate(continent = recode(citizen, "AO" = "Africa", 
                            "CM" = "Africa", 
                            "CF" = "Africa", 
                            "TD" = "Africa", 
                            "CG" = "Africa", 
                            "CD" = "Africa", 
                            "GQ" = "Africa", 
                            "GA" = "Africa", 
                            "ST" = "Africa", 
                            "BI" = "Africa", 
                            "KM" = "Africa", 
                            "DJ" = "Africa", 
                            "ER" = "Africa", 
                            "ET" = "Africa", 
                            "KE" = "Africa", 
                            "MG" = "Africa", 
                            "MW" = "Africa", 
                            "MU" = "Africa", 
                            "MZ" = "Africa", 
                            "RW" = "Africa", 
                            "SC" = "Africa", 
                            "SO" = "Africa", 
                            "UG" = "Africa", 
                            "TZ" = "Africa", 
                            "ZM" = "Africa",
                            "ZW" = "Africa",
                            "DZ" = "Africa", 
                            "EG" = "Africa", 
                            "LY" = "Africa", 
                            "MA" = "Africa", 
                            "SS" = "Africa", 
                            "SD" = "Africa", 
                            "TN" = "Africa", 
                            "EH" = "Africa", 
                            "BW" = "Africa", 
                            "LS" = "Africa", 
                            "NA" = "Africa", 
                            "ZA" = "Africa", 
                            "SZ" = "Africa", 
                            "BJ" = "Africa", 
                            "BF" = "Africa", 
                            "CV" = "Africa", 
                            "CI" = "Africa", 
                            "GM" = "Africa", 
                            "GH" = "Africa", 
                            "GN" = "Africa", 
                            "GW" = "Africa", 
                            "LR" = "Africa", 
                            "ML" = "Africa", 
                            "MR" = "Africa", 
                            "NE" = "Africa", 
                            "NG" = "Africa", 
                            "SN" = "Africa", 
                            "SL" = "Africa", 
                            "TG" = "Africa",
                            "CA" = "America",
                            "US" = "America",
                            "AG" = "America",
                            "BS" = "America",
                            "BB" = "America",
                            "CU" = "America",
                            "DM" = "America",
                            "DO" = "America",
                            "GD" = "America",
                            "HT" = "America",
                            "JM" = "America",
                            "KN" = "America",
                            "LC" = "America",
                            "VC" = "America",
                            "TT" = "America",
                            "BZ" = "America",
                            "CR" = "America",
                            "SV" = "America",
                            "GT" = "America",
                            "HN" = "America",
                            "MX" = "America",
                            "NI" = "America",
                            "PA" = "America",
                            "AR" = "America",
                            "BO" = "America",
                            "BR" = "America",
                            "CL" = "America",
                            "CO" = "America",
                            "EC" = "America",
                            "GY" = "America",
                            "PY" = "America",
                            "PE" = "America",
                            "SR" = "America",
                            "UY" = "America",
                            "VE" = "America",
                            "KZ" = "Asia",
                            "KG" = "Asia",
                            "TJ" = "Asia",
                            "TM" = "Asia",
                            "UZ" = "Asia",
                            "CN" = "Asia",
                            "JP" = "Asia",
                            "MN" = "Asia",
                            "KP" = "Asia",
                            "KR" = "Asia",
                            "TW" = "Asia",
                            "AF" = "Asia",
                            "BD" = "Asia",
                            "BT" = "Asia",
                            "IN" = "Asia",
                            "IR" = "Asia",
                            "MV" = "Asia",
                            "NP" = "Asia",
                            "PK" = "Asia",
                            "LK" = "Asia",
                            "BN" = "Asia",
                            "KH" = "Asia",
                            "ID" = "Asia",
                            "LA" = "Asia",
                            "MY" = "Asia",
                            "MM" = "Asia",
                            "PH" = "Asia",
                            "SG" = "Asia",
                            "TH" = "Asia",
                            "TL" = "Asia",
                            "VN" = "Asia",
                            "AM" = "Asia",
                            "AZ" = "Asia",
                            "BH" = "Asia",
                            "GE" = "Asia",
                            "IQ" = "Asia",
                            "IL" = "Asia",
                            "JO" = "Asia",
                            "KW" = "Asia",
                            "LB" = "Asia",
                            "PS" = "Asia",
                            "OM" = "Asia",
                            "QA" = "Asia",
                            "SA" = "Asia",
                            "SY" = "Asia",
                            "AE" = "Asia",
                            "YE" = "Asia",
                            "AU" = "Oceania",
                            "NZ" = "Oceania",
                            "FJ" = "Oceania",
                            "PG" = "Oceania",
                            "SB" = "Oceania",
                            "VU" = "Oceania",
                            "KI" = "Oceania",
                            "MH" = "Oceania",
                            "FM" = "Oceania",
                            "NR" = "Oceania",
                            "PW" = "Oceania",
                            "CK" = "Oceania",
                            "WS" = "Oceania",
                            "TO" = "Oceania",
                            "TV" = "Oceania",
                            "STLS" = "Stateless",
                            "TOTAL" = "Total"))

residence <- residence %>% 
  mutate(magrebe = recode(citizen, "DZ" = "Magrebe",
                          "LY" = "Magrebe",
                          "MA" = "Magrebe",
                          "TN" = "Magrebe"))


################### Residencia #########################

# Contas
residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(continent ==  "Africa") %>% 
  summarise(sum(OBS_VALUE)) 

residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(continent ==  "Asia") %>% 
  summarise(sum(OBS_VALUE))

residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(continent ==  "America") %>% 
  summarise(sum(OBS_VALUE))

residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(continent ==  "Oceania") %>% 
  summarise(sum(OBS_VALUE))

residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(continent ==  "Stateless") %>% 
  summarise(sum(OBS_VALUE))

residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(continent ==  "Total") %>%  
  summarise(sum(OBS_VALUE))

residence %>%
  group_by(TIME_PERIOD) %>% 
  filter(magrebe == "Magrebe") %>%  
  summarise(sum(OBS_VALUE))
  
# Gráficos

resid_africa <- residence %>% 
  filter(continent == "Africa")

ggplot(resid_africa, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(1500000, 2100000)) +
  scale_y_continuous(breaks = seq(1500000, 2100000, by = 150000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("África - Residência")

####

resid_asia <- residence %>% 
  filter(continent == "Asia")

ggplot(resid_asia, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(300000, 500000)) +
  scale_y_continuous(breaks = seq(300000, 500000, by = 50000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Ásia - Residência")


####

resid_america <- residence %>% 
  filter(continent == "America")

ggplot(resid_america, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(100000, 220000)) +
  scale_y_continuous(breaks = seq(100000, 220000, by = 20000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("América - Residência")


####

resid_oceania <- residence %>% 
  filter(continent == "Oceania")

ggplot(resid_oceania, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(4000, 6000)) +
  scale_y_continuous(breaks = seq(4000, 6000, by = 500)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Oceania - Residência")

####

resid_stateless <- residence %>% 
  filter(continent == "Stateless")

ggplot(resid_stateless, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(100, 250)) +
  scale_y_continuous(breaks = seq(100, 250, by = 50)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Apátridas - Residência")

####

resid_total <- residence %>% 
  filter(continent == "Total")

ggplot(resid_total, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(2300000, 3300000)) +
  scale_y_continuous(breaks = seq(2300000, 3300000, by = 300000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Residência")

####

resid_magrebe <- residence %>% 
  filter(magrebe == "Magrebe")

ggplot(resid_magrebe, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(1200000, 1400000)) +
  scale_y_continuous(breaks = seq(1200000, 1400000, by = 50000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Magrebe - Residência")

################## Asilo #######################

# Contas

# POSITIVE DECISIONS
asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent ==  "Africa") %>% 
  summarise(sum(OBS_VALUE)) 

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent ==  "Asia") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent ==  "America") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent ==  "Oceania") %>%   # Deu zero
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T",decision == "TOTAL_POS", age == "TOTAL",continent ==  "Stateless") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent ==  "Total") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", magrebe == "Magrebe") %>% 
  summarise(sum(OBS_VALUE))

# NEGATIVE DECISIONS

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "REJECTED", age == "TOTAL", continent ==  "Africa") %>% 
  summarise(sum(OBS_VALUE)) 

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "REJECTED", age == "TOTAL", continent ==  "Asia") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "REJECTED", age == "TOTAL", continent ==  "America") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "REJECTED", age == "TOTAL", continent ==  "Oceania") %>%   # Deu zero
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T",decision == "REJECTED", age == "TOTAL", continent ==  "Stateless") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "REJECTED", age == "TOTAL", continent ==  "Total") %>% 
  summarise(sum(OBS_VALUE))

asilo %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", decision == "REJECTED", age == "TOTAL", magrebe == "Magrebe") %>% 
  summarise(sum(OBS_VALUE))


# Gráficos


asilo_africa <- asilo %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent == "Africa")

ggplot(asilo_africa, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(10000, 30000)) +
  scale_y_continuous(breaks = seq(10000, 30000, by = 5000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("África - Asilo")

####

asilo_asia <- asilo %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent == "Asia")

ggplot(asilo_asia, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(20000, 35000)) +
  scale_y_continuous(breaks = seq(20000, 35000, by = 5000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Ásia - Asilo")

####

asilo_america <- asilo %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent == "America")

ggplot(asilo_america, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(300, 1800)) +
  scale_y_continuous(breaks = seq(400, 1800, by = 300)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("América - Asilo")

####

asilo_stateless <- asilo %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", continent == "Stateless")

ggplot(asilo_stateless, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(50, 150)) +
  scale_y_continuous(breaks = seq(50, 150, by = 30)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Apátrida - Asilo")

####

asilo_total <- asilo %>% 
  filter(sex == "T",decision == "TOTAL_POS", age == "TOTAL", age == "TOTAL", continent == "Total")

ggplot(asilo_total, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(10000, 35000)) +
  scale_y_continuous(breaks = seq(10000, 35000, by = 5000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Asilo")

# ------------------
  
asilo_total_rejeicao <- asilo %>% 
  filter(sex == "T",decision == "REJECTED", age == "TOTAL", continent == "Total")

ggplot(asilo_total_rejeicao, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(55000, 90000)) +
  scale_y_continuous(breaks = seq(55000, 90000, by = 5000)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Rejeição - Asilo")

####

asilo_magrebe <- asilo %>% 
  filter(sex == "T", decision == "TOTAL_POS", age == "TOTAL", magrebe == "Magrebe")

ggplot(asilo_magrebe, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(50, 850)) +
  scale_y_continuous(breaks = seq(50, 850, by = 150)) +
  theme_classic() +
  labs(x = "",
       y = "") +
  ggtitle("Magrebe - Asilo")

####### acrescentando genero e idade ###########

asilo_Africa_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "TOTAL_POS", continent == "Africa")

ggplot(asilo_Africa_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("África - Asilo")

#ENGLISH

ggplot(asilo_Africa_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("F", "M"))


####

asilo_Asia_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "TOTAL_POS", continent == "Asia")

ggplot(asilo_Asia_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Ásia - Asilo")

# ENGLISH

ggplot(asilo_Asia_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("F", "M"))

####

asilo_America_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "TOTAL_POS", continent == "America")

ggplot(asilo_America_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("América - Asilo")

# ENGLISH

ggplot(asilo_America_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("F", "M"))

####

asilo_Stateless_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "TOTAL_POS", continent == "Stateless")

ggplot(asilo_Stateless_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Apátridas - Asilo")

####

asilo_Total_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "TOTAL_POS", continent == "Total")

ggplot(asilo_Total_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Asilo")

#---------------------

asilo_Total_rejeicao_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "REJECTED", continent == "Total")

ggplot(asilo_Total_rejeicao_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Rejeição - Asilo")

####

asilo_Magreb_multi <- asilo %>% 
  filter(sex != "T", age_fator != "Total", decision == "TOTAL_POS", magrebe == "Magrebe")

ggplot(asilo_Magreb_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Magrebe - Asilo")

#ENGLISH

ggplot(asilo_Magreb_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("F", "M"))


#################### resettled ########################

# Contas

resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age == "TOTAL", continent == "Africa") %>% 
  summarise(sum(OBS_VALUE))


resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age_fator == "65+", continent == "Asia") %>% 
  summarise(sum(OBS_VALUE))

resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age == "TOTAL", continent == "America") %>%   # Deu zero
  summarise(sum(OBS_VALUE))

resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age == "TOTAL", continent == "Oceania") %>%   # Deu zero, sem dados pra 2018
  summarise(sum(OBS_VALUE))

resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age == "TOTAL", continent == "Stateless") %>%  # Deu zero, sem dados pra 2018
  summarise(sum(OBS_VALUE))

resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age == "TOTAL", continent == "Total") %>%  
  summarise(sum(OBS_VALUE))

resettled %>%
  group_by(TIME_PERIOD) %>% 
  filter(sex == "T", age == "TOTAL", magrebe == "Magrebe") %>%  # Apenas em 2019, todos homens
  summarise(sum(OBS_VALUE))

# Gráficos


resettled_africa <- resettled %>% 
  filter(sex == "T", age == "TOTAL", continent == "Africa")

ggplot(resettled_africa, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 5000)) +
  scale_y_continuous(breaks = seq(0, 5000, by = 1000)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  ggtitle("Reassentamento")


####

resettled_asia <- resettled %>% 
  filter(sex == "T", age == "TOTAL", continent == "Asia")

ggplot(resettled_asia, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(500, 9500)) +
  scale_y_continuous(breaks = seq(500, 9500, by = 1000)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  ggtitle("Reassentamento")


####


resettled_total <- resettled %>% 
  filter(sex == "T", age == "TOTAL", continent == "Total")

ggplot(resettled_total, aes(TIME_PERIOD, OBS_VALUE)) +
  geom_col() +
  coord_cartesian(ylim = c(500, 5700)) +
  scale_y_continuous(breaks = seq(500, 5700, by = 1000)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  ggtitle("Reassentamento")


####### acrescentando genero e idade ###########

resettled_Africa_multi <- resettled %>% 
  filter(sex != "T", age_fator != "Total", continent == "Africa")

ggplot(resettled_Africa_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("África - Reassentamento")

#ENGLISH

ggplot(resettled_Africa_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("F", "M"))


####

resettled_Asia_multi <- resettled %>% 
  filter(sex != "T", age_fator != "Total", continent == "Asia")

ggplot(resettled_Asia_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Ásia - Reassentamento")

#ENGLISH

ggplot(resettled_Asia_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("F", "M"))

####

resettled_Total_multi <- resettled %>% 
  filter(sex != "T", age_fator != "Total", continent == "Total")

ggplot(resettled_Total_multi, aes(TIME_PERIOD, OBS_VALUE, fill = sex)) +
  geom_col() +
  facet_wrap(vars(age_fator)) +
  theme_classic() +
  labs(x = "",
       y = "") + 
  scale_fill_discrete(name = "", labels = c("M", "H")) +
  ggtitle("Reassentamento")




