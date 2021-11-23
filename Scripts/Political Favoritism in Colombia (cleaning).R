## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Favoritism in Colombia (cleaning)
##
## Author:            Carlos A. Toruño Paniagua
##
## Email:             c.toruopaniagua@stud.uni-goettingen.de
##
## Creation date:     May, 2021
##
## This version:      August 16th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##            1. DEFINING FUNCTIONS TO USE
##            2. LOADING AND CLEANING 2011 ELECTORAL DATA
##            3. LOADING AND CLEANING 2015 ELECTORAL DATA
##            4. LOADING AND CLEANING 2019 ELECTORAL DATA
##            5. ELECTORAL DATA MUNICIPAL PANEL
##            6. LOADING OUTCOMES AND CONTROLS
##            7. CREATING A BASELINE COVARIATES DATASET FOR BALANCE CHECKS
##            8. CREATING THE MAIN DATASET
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Working directory when working outside project
#setwd("/Users/carlostorunopaniagua/Documents/MA in Development Economics/Thesis/Data")
rm(list=ls())

# Required packages
lapply(list("haven", "RSQLite", "readxl", "sf", "rmapshaper", "tidyverse", "fuzzyjoin"), 
       library, character.only = TRUE)

    
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1. DEFINING FUNCTIONS TO USE                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining SQL extract function
sql2df <- function(dbpath = c(""), sqlyear = 2000, tab_0 = 0, tab_f = 10) {
  
  # Connecting SQL
  sql <- dbConnect(SQLite(), dbname = dbpath)
  dbListTables(sql)
  
  # Importing databases
  raw.df <- dbReadTable(sql, "x_resum_municipio_01")[FALSE,]
  # I'm just using the firt table to create an empty data frame to work with in the 
  # following loop:
  
  tables <- as.vector(dbListTables(sql)[tab_0:tab_f])
  for (tabs in tables) {
    auxdata.df <- dbReadTable(sql, tabs) %>%
      filter(corp == 3)
    # corp == 3 is for mayor elections
    raw.df <- raw.df %>%
      bind_rows(auxdata.df)
  }
  
  # Renaming outcomes
  assign(paste("election_data_", sqlyear, ".df", sep = ""), raw.df, envir = .GlobalEnv)
  
  # Disconnecting connection
  dbDisconnect(sql)
}
  
# Defining data frame cleaning function
clean_edata <- function(edata = NULL, edata_year = 2000, pres_pp = c(""), ties = "no.ties",
                        pres_pp_code = 0, coa_desc = c(""), others_desc = c("")) {
  
  # Cleaning data at the municipality level
  mun.df <- edata %>%
    group_by(depto, mpio) %>%
    filter(candidato < 996) %>%
    arrange(desc(porc_part_mesa), .by_group = T) %>%
    slice_max(order_by = porc_part_mesa, n = 2) %>%
    mutate(pos = row_number(),
           n = max(pos),
           race = if_else(n >= 2, 1, 0)) %>%
    pivot_wider(id_cols = c(depto, desc_depto, mpio, desc_mpio),
                names_from = pos, values_from = c(partido, desc_partido, porc_part_mesa)) %>%
    mutate(pres_pp_bwon = if_else(partido_1 == pres_pp_code, 1, missing = 0,
                                  if_else(str_detect(desc_partido_1, pres_pp),
                                          1, 0, missing = 0)),
           pres_pp_blost = if_else(partido_2 == pres_pp_code, 1, missing = 0,
                                   if (ties == "reported") {
                                     if_else(str_detect(desc_partido_2, pres_pp),
                                             1, missing = 0,
                                             if_else(partido_3 == pres_pp_code, 1, 0, 
                                                     missing = 0))
                                                        # Not necessary to go to partido_3 string
                                                        # No coalitions in "3rd" place
                                   } else {
                                     if_else(str_detect(desc_partido_2, pres_pp),
                                             1, 0, missing = 0)
                                   }),
           coa_pp_bwon = if_else(pres_pp_bwon == 1, 1, missing = 0,
                                 if_else(str_detect(desc_partido_1, coa_desc),
                                         1, 0, missing = 0)),
           coa_pp_blost = if_else(pres_pp_blost == 1, 1, missing = 0,
                                  if (ties == "reported") {
                                    if_else(str_detect(desc_partido_2, coa_desc),
                                            1, missing = 0,
                                            if_else(str_detect(desc_partido_3, coa_desc),
                                                    1, 0, missing = 0))
                                  } else {
                                    if_else(str_detect(desc_partido_2, coa_desc),
                                            1, 0, missing = 0)
                                  }),
           margin_pres = if_else(pres_pp_bwon == 1, porc_part_mesa_1 - porc_part_mesa_2,
                                 if_else(pres_pp_blost == 1, porc_part_mesa_2 - porc_part_mesa_1,
                                         NA_real_)),
           sample_pres = if_else((pres_pp_bwon == 1 | pres_pp_blost == 1) & abs(margin_pres) <= 10, 
                                 1, 0, missing = 0),
           unidentified_winner = if_else(pres_pp_bwon == 1, 0, missing = 0,
                                         if_else(coa_pp_bwon == 1, 0, missing = 0,
                                                 if_else(str_detect(desc_partido_1,
                                                                    regex(others_desc, 
                                                                          ignore_case = T)),
                                                         0, 1, missing = 0))),
           unidentified_loser = if_else(pres_pp_blost == 1, 0, missing = 0,
                                        if_else(coa_pp_blost == 1, 0, missing = 0,
                                                  if_else(str_detect(desc_partido_2,
                                                                     regex(others_desc, 
                                                                           ignore_case = T)), 
                                                          0, 1, missing = NA_real_))))
  
  # Renaming outcomes                                              
  assign(paste("mun_", edata_year, ".df", sep = ""), mun.df, envir = .GlobalEnv)

}


# Defining political alignment function
polalgn <- function(old_data, alt_data, edata_year = 2000,
                    pres_pp = c(""), pres_pp_code = 0, ties = "no.ties") {
  
  # Joining Joining new info to main dataset
  mun_new.df <- old_data %>%
    left_join(alt_data, by = c("depto", "mpio")) %>%
    { if (ties == "reported") rename(.,desc_pp_3 = desc_partido_3) else . } %>%
    mutate(desc_pp_1 = if_else(!is.na(new_desc_partido_1), new_desc_partido_1, desc_partido_1),
           desc_pp_2 = if_else(!is.na(new_desc_partido_2), new_desc_partido_2, desc_partido_2),
           pres_pp_bwon = if_else(partido_1 == pres_pp_code, 1, missing = 0,
                                  if_else(str_detect(desc_pp_1, pres_pp),
                                          1, 0, missing = 0)),
           pres_pp_blost = if_else(partido_2 == pres_pp_code, 1, missing = 0,
                                   if (ties == "reported") {
                                     if_else(str_detect(desc_pp_2, pres_pp),
                                             1, missing = 0,
                                             if_else(partido_3 == pres_pp_code, 1, 0, 
                                                     missing = 0))
                                     # Not necessary to go to partido_3 string
                                     # No coalitions in "3rd" place
                                   } else {
                                     if_else(str_detect(desc_pp_2, pres_pp),
                                             1, 0, missing = 0)
                                   }),
           margin_pres = if_else(pres_pp_bwon == 1, porc_part_mesa_1 - porc_part_mesa_2,
                                 if_else(pres_pp_blost == 1, porc_part_mesa_2 - porc_part_mesa_1,
                                         NA_real_)),
           sample_pres = if_else((pres_pp_bwon == 1 | pres_pp_blost == 1) & abs(margin_pres) <= 10, 
                                 1, 0, missing = 0))
  
  # Renaming outcomes                                              
  assign(paste("mun_", edata_year, ".df", sep = ""), mun_new.df, envir = .GlobalEnv)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2. LOADING AND CLEANING 2011 ELECTORAL DATA                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading 2011 data
sql2df(dbpath = c("./Data/Data - Raw/Elections/Data 2011/db/Estadisticas.db"),
       sqlyear = 2011, tab_0 = 112, tab_f = 144)

# Cleaning 2011 data
clean_edata(edata = election_data_2011.df,
            edata_year = 2011,
            pres_pp_code = 9,
            pres_pp = c("DE LA U|UNIDAD NAC"),
            coa_desc = c("CAMBIO RAD|LIBERAL|CONSERVADOR"),
            others_desc = paste("LIBERAL|CONSERVADOR|INTEGRACION NA|PIN|CAMBIO RAD|",
                                "VERDE|AUTORIDADES INDIGENAS|AICO|ALIANZA SOCIAL|ASI|",
                                "DE LA U|UNIDAD NAC|POLO DEMOCRATICO|RADICAL|",
                                "AFROVIDES|INCLUSION Y OPORT", sep = ""),
            ties = "reported")

    # Municipalities Bello (Antioquia), Tunungua (Boyaca), Gonzalez (Cesar), Herran (Norte 
    # de San) only report one single candidate running.
    # Municipalities La Victoria (Valle del Cauca) and Villapinzon (Cundinamarca) have both 
    # a tie in second place.

# Loading manual codification of unknown coalitions
coa_2011.df <- read_xlsx("./Data/Data - Raw/Elections/MDS/sub-sample_2011.xlsx") %>%
  filter((!is.na(partido_1) & unidentified_winner == 1) | 
           (!is.na(partido_2) & unidentified_loser == 1)) %>%
  select(2, 3, 7, 9) %>%
  rename(new_desc_partido_1 = partido_1,
         new_desc_partido_2 = partido_2)

# Re-identifying political alignment
polalgn(old_data = mun_2011.df, alt_data = coa_2011.df, edata_year = 2011, 
        pres_pp = c("DE LA U|DE LA  U|UNIDAD NAC"), pres_pp_code = 9,
        ties = "reported")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                3. LOADING AND CLEANING 2015 ELECTORAL DATA                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading 2015 data
sql2df(dbpath = c("./Data/Data - Raw/Elections/Data 2015/db/Estadisticas.db"),
       sqlyear = 2015, tab_0 = 111, tab_f = 143)

# Cleaning 2015 data
clean_edata(edata = election_data_2015.df,
            edata_year = 2015,
            pres_pp_code = 9,
            pres_pp = c("DE LA U|UNIDAD NAC"),
            coa_desc = c("CAMBIO RAD|LIBERAL|UNIÓN PATRI|POLO DEM|VERDE"),
                             # MIRA and Mov Progr. are also part of the pres. coalition but they didn't 
                             # win or barely lost any municipality in 2015.
            others_desc = paste("LIBERAL|CONSERVADOR|CAMBIO RAD|VERDE|AUTORIDADES INDÍGENAS|",
                                "AICO|ALIANZA SOCIAL|ASI|DE LA U|UNIDAD NAC|POLO DEM|RADICAL|",
                                "AFROVIDES|INTEGRACI| MIRA|CENTRO|OPCIÓN|OPCION|",
                                "ALTERNATIVO INDÍGENA|MAIS", 
                                sep = ""),
            ties = "reported")

    # Municipalities Susa (Cundinamarca), La Playa (Norte de Santander), La Salina (Casaanare)
    # and La Jagua del Pilar (Guajira) only report one single candidate running.
    # Municipality of Sachica (Boyaca) presented a tied in second place.

# Loading manual codification of unknown coalitions
coa_data_2015.df <- read_delim("./Data/Data - Raw/Elections/Other sources/MOE_2015_coalitions_details.csv", 
                               delim = ";") %>% 
  select(2:6) %>%
  mutate(Depto = str_replace_all(toupper(Depto), 
                                 c("Á" = "A", "É" = "E", "Í" = "I", 
                                   "Ó" = "O", "Ú|Ü" = "U")),
         Municipio = str_replace_all(toupper(Municipio),
                                     c("Á" = "A", "É" = "E", "Í" = "I", 
                                       "Ó" = "O", "Ú|Ü" = "U")))

coa_2015.df <- mun_2015.df %>% 
  filter(unidentified_winner == 1) %>%
  select(depto, desc_depto, mpio, desc_mpio, desc_partido_1) %>%
  stringdist_left_join(coa_data_2015.df,
                       by = c("desc_depto" = "Depto", "desc_mpio" = "Municipio",
                              "desc_partido_1" = "Nombre de la coalición"),
                       method = "qgram",
                       ignore_case = T) %>%
  rename(new_desc_partido_1_long = "Nombre de la coalición",
         new_desc_partido_1 = "Partidos de la coalición (largo)") %>%
  select(-c(6,7,9)) %>%
  full_join(mun_2015.df %>% 
              filter(unidentified_loser == 1) %>%
              select(depto, desc_depto, mpio, desc_mpio, desc_partido_2),
            by = c("depto", "desc_depto", "mpio", "desc_mpio")) %>%
  stringdist_left_join(coa_data_2015.df,
                       by = c("desc_depto" = "Depto", "desc_mpio" = "Municipio",
                              "desc_partido_2" = "Nombre de la coalición"),
                       method = "qgram",
                       ignore_case = T) %>%
  rename(new_desc_partido_2_long = "Nombre de la coalición",
         new_desc_partido_2 = "Partidos de la coalición (largo)") %>%
  select(depto, mpio, new_desc_partido_1, new_desc_partido_2) %>%
  mutate(new_desc_partido_1 = toupper(new_desc_partido_1),
         new_desc_partido_2 = toupper(new_desc_partido_2))

# Re-identifying political alignment
polalgn(old_data = mun_2015.df, alt_data = coa_2015.df, edata_year = 2015, 
        pres_pp = c("DE LA U|UNIDAD NAC"), pres_pp_code = 9,
        ties = "reported")
rm(coa_data_2015.df)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                4. LOADING AND CLEANING 2019 ELECTIONS                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Unifying 2019 electoral data
excel_sheets("./Data/Data - Raw/Elections/Data 2019/VALLE 2019.xlsx")  # Useful to inspect excel files

election_data_2019.df <- tribble(~depto, ~desc_depto, ~mpio, ~desc_mpio, ~partido, ~desc_partido,
                                 ~candidato, ~desc_candidato, ~votos)
          # I create an empty tibble to work with in the following loop

dptos <- as.list(dir("./Data/Data - Raw/Elections/Data 2019/"))
for (dpto_name in dptos[-c(2,5,6,12,24,32)]) {
          # Files for Antioquia, Bogota, Cauca, N. Santander and Valle have their own data structure
          # Excluded from this loop
  file_name <- paste("./Data/Data - Raw/Elections/Data 2019/", dpto_name, sep = "")
  election_data_2019.df <- election_data_2019.df %>%
    bind_rows(read_xlsx(file_name, sheet = "2019") %>%
                filter(COR == "003") %>%
                group_by(MUN, PAR, CAN) %>%
                summarise(depto = as.numeric(first(DE)),
                          desc_depto = first(DEPNOMBRE),
                          mpio = as.numeric(first(MUN)),
                          desc_mpio = first(MUNNOMBRE),
                          partido = as.numeric(first(PAR)),
                          desc_partido = first(PARNOMBRE),
                          candidato = as.numeric(first(CAN)),
                          desc_candidato = first(CANNOMBRE),
                          votos = sum(VOTOS)) %>%
                ungroup())
}

# Unifying  Antioquia, Bogota, Cauca, N. Santander and Valle electoral data
election_data_2019.df <- election_data_2019.df %>%
  bind_rows(read_xlsx("./Data/Data - Raw/Elections/Data 2019/ANTIOQUIA 2019.xlsx", 
                      sheet = "ALCALDÍA") %>%
              bind_rows(read_xlsx("./Data/Data - Raw/Elections/Data 2019/BOGOTA 2019 POR MESA.xlsx", 
                                  sheet = "ALCALDÍA LOCALIDADES 1 A LA 10"),
                        read_xlsx("./Data/Data - Raw/Elections/Data 2019/BOGOTA 2019 POR MESA.xlsx", 
                                  sheet = "ALCALDÍA LOCALIDADES 11 A LA 98"),
                        read_xlsx("./Data/Data - Raw/Elections/Data 2019/CAUCA 2019.xlsx", 
                                  sheet = "PARTE5"),
                        read_xlsx("./Data/Data - Raw/Elections/Data 2019/NORTE DE SANTANDER 2019.xlsx", 
                                  sheet = "PARTE11"),
                        read_xlsx("./Data/Data - Raw/Elections/Data 2019/VALLE 2019.xlsx", 
                                  sheet = "PARTE 1"),
                        read_xlsx("./Data/Data - Raw/Elections/Data 2019/VALLE 2019.xlsx", 
                                  sheet = "PARTE 2")) %>%
              filter(COR == "003") %>%
              group_by(MUN, PAR, CAN) %>%
              summarise(depto = as.numeric(first(DE)),
                        desc_depto = first(DEPNOMBRE),
                        mpio = as.numeric(first(MUN)),
                        desc_mpio = first(MUNNOMBRE),
                        partido = as.numeric(first(PAR)),
                        desc_partido = first(PARNOMBRE),
                        candidato = as.numeric(first(CAN)),
                        desc_candidato = first(CANNOMBRE),
                        votos = sum(VOTOS)) %>%
              ungroup()) %>%
  group_by(depto, mpio) %>%
  mutate(total_votes = sum(votos),
         porc_part_mesa = (votos/total_votes)*100)

# Cleaning 2019 data
clean_edata(edata = election_data_2019.df,
            edata_year = 2019,
            pres_pp_code = 11,
            pres_pp = c("CENTRO DEM"),
            coa_desc = c("CONSERVADOR|LIBERAL|JUSTA LIB"),
            others_desc = paste("CAMBIO RAD|ALIANZA VERDE|AUTORIDADES IND|AICO|ASI -|",
                                "ALIANZA SOCIAL|SOCIAL DE UNI|DE LA U|COLOMBIA HUMANA|",
                                "POLO|FARC|ALTERNATIVO IND|MAIS| ADA|COLOMBIA RENACIENTE",
                                sep = ""))

    # Municipalities Corrales (Boyaca), Tunungua (Boyaca), Belen (Narino) and San Benito (Santander)
    # only report one single candidate running. No reported second place ties.


coa_data_2019.df <- read_xlsx("./Data/Data - Raw/Elections/MDS/INFO_CANDIDATOS_2019.xlsx") %>%
  select(3,4,6,8) %>%
  mutate(NOMPAR = str_replace_all(toupper(NOMPAR), 
                                          c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                            "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N")),
         NOMPAR = str_replace_all(toupper(NOMPAR), c("COALICION|COAL." = "")),
         NOMMUN = str_replace_all(toupper(NOMMUN), 
                                  c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                    "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N")),
         NOMDEP = str_replace_all(toupper(NOMDEP), 
                                  c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                    "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N")))

coa_2019.df <- mun_2019.df %>% 
  filter(unidentified_winner == 1) %>%
  select(depto, desc_depto, mpio, desc_mpio, desc_partido_1) %>%
  mutate(desc_partido_1 = str_replace_all(toupper(desc_partido_1), 
                                          c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                            "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N")),
         desc_partido_1 = str_replace_all(toupper(desc_partido_1), c("COALICION|COAL." = "")),
         desc_mpio = str_replace_all(toupper(desc_mpio), 
                                     c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                       "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N"))) %>%
  stringdist_left_join(coa_data_2019.df ,
                       by = c("desc_depto" = "NOMDEP", "desc_mpio" = "NOMMUN",
                              "desc_partido_1" = "NOMPAR"),
                       method = "qgram",
                       ignore_case = T) %>%
  rename(new_desc_partido_1 = "PARTIDOS_POLITICOS") %>%
  select(-c(6,8,9)) %>%
  full_join(mun_2019.df %>% 
              filter(unidentified_loser == 1) %>%
              select(depto, desc_depto, mpio, desc_mpio, desc_partido_2) %>%
              mutate(desc_partido_2 = str_replace_all(toupper(desc_partido_2), 
                                                      c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                                        "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N")),
                     desc_partido_2 = str_replace_all(toupper(desc_partido_2), 
                                                      c("COALICION|COAL." = "")),
                     desc_mpio = str_replace_all(toupper(desc_mpio), 
                                                 c("ﾁ|Á" = "A", "ﾉ|É" = "E", "ﾍ|Í" = "I",
                                                   "ﾓ|Ó" = "O", "Ú|Ü|ﾚ" = "U", "ﾑ|Ñ" = "N"))),
            by = c("depto", "desc_depto", "mpio", "desc_mpio")) %>%
  stringdist_left_join(coa_data_2019.df,
                       by = c("desc_depto" = "NOMDEP", "desc_mpio" = "NOMMUN",
                              "desc_partido_2" = "NOMPAR"),
                       method = "qgram",
                       ignore_case = T) %>%
  rename(new_desc_partido_2 = "PARTIDOS_POLITICOS") %>%
  select(c(1,3,6,9))

# Re-identifying political alignment
polalgn(old_data = mun_2019.df, alt_data = coa_2019.df, edata_year = 2019, 
        pres_pp = c("CENTRO DEM"), pres_pp_code = 11)
rm(dptos, coa_data_2019.df)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                5. ELECTORAL DATA MUNICIPAL PANEL                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Unifying yearly datasets
mun_edata.df <- mun_2011.df %>% mutate(year = 2011) %>%
  bind_rows(mun_2015.df %>% mutate(year = 2015)) %>%
  bind_rows(mun_2019.df %>% mutate(year = 2019)) %>%
  select(-c(5:9, 20:23)) %>%
  relocate(1:4,17,15,6,16,7,5,8:14) %>%
  distinct(depto, mpio, year, .keep_all = T) %>%
        # Municipality of Quimbaya in Quindio state is duplicate for 2015 
  ungroup() %>%
  rename(edata_year = year) %>%
  mutate(desc_depto = str_replace_all(desc_depto, 
                                 c("Á" = "A", "É" = "E", "Í" = "I", 
                                   "Ó" = "O", "Ú|Ü" = "U", "Ñ" = "N",
                                   "VALLE" = "VALLE DEL CAUCA",
                                   "SAN ANDRES" = "ARCHIPIELAGO DE SAN ANDRES")),
         desc_mpio = str_replace_all(desc_mpio,
                                     c("Á" = "A", "É" = "E", "Í" = "I", 
                                       "Ó" = "O", "Ú|Ü" = "U", "Ñ" = "N")))

# Rewriting names to match merges
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "BOLIVAR" & 
                            mun_edata.df["desc_depto"] == "ANTIOQUIA"] <- "CIUDAD BOLIVAR"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "CARMEN DE VIBORAL" & 
                            mun_edata.df["desc_depto"] == "ANTIOQUIA"] <- "EL CARMEN DE VIBORAL"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PUERTO NARE-LA MAGDALENA" & 
                            mun_edata.df["desc_depto"] == "ANTIOQUIA"] <- "PUERTO NARE"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SAN ANDRES" & 
                            mun_edata.df["desc_depto"] == "ANTIOQUIA"] <- "SAN ANDRES DE CUERQUIA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SANTUARIO" & 
                            mun_edata.df["desc_depto"] == "ANTIOQUIA"] <- "EL SANTUARIO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "YONDO-CASABE" & 
                            mun_edata.df["desc_depto"] == "ANTIOQUIA"] <- "YONDO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "BOGOTA. D.C." & 
                            mun_edata.df["desc_depto"] == "BOGOTA D.C."] <- "BOGOTA D.C."
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ARROYO HONDO" & 
                            mun_edata.df["desc_depto"] == "BOLIVAR"] <- "ARROYOHONDO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "RIOVIEJO" & 
                            mun_edata.df["desc_depto"] == "BOLIVAR"] <- "RIO VIEJO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "TIQUISIO (PTO. RICO)" & 
                            mun_edata.df["desc_depto"] == "BOLIVAR"] <- "TIQUISIO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "AQUITANIA (PUEBLOVIEJO)" & 
                            mun_edata.df["desc_depto"] == "BOYACA"] <- "AQUITANIA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "VILLA DE LEIVA" & 
                            mun_edata.df["desc_depto"] == "BOYACA"] <- "VILLA DE LEYVA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "LOPEZ (MICAY)" & 
                            mun_edata.df["desc_depto"] == "CAUCA"] <- "LOPEZ"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PAEZ (BELALCAZAR)" & 
                            mun_edata.df["desc_depto"] == "CAUCA"] <- "PAEZ"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PATIA (EL BORDO)" & 
                            mun_edata.df["desc_depto"] == "CAUCA"] <- "PATIA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PURACE (COCONUCO)" & 
                            mun_edata.df["desc_depto"] == "CAUCA"] <- "PURACE"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SOTARA (PAISPAMBA)" & 
                            mun_edata.df["desc_depto"] == "CAUCA"] <- "SOTARA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "MANAURE BALCON DEL CESAR (MANA" & 
                            mun_edata.df["desc_depto"] == "CESAR"] <- "MANAURE"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "COTORRA (BONGO)" & 
                            mun_edata.df["desc_depto"] == "CORDOBA"] <- "COTORRA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "LA APARTADA (FRONTERA)" & 
                            mun_edata.df["desc_depto"] == "CORDOBA"] <- "LA APARTADA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SAN ANDRES DE SOTAVENTO" & 
                            mun_edata.df["desc_depto"] == "CORDOBA"] <- "SAN ANDRES SOTAVENTO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PARATEBUENO (LA NAGUAYA)" & 
                            mun_edata.df["desc_depto"] == "CUNDINAMARCA"] <- "PARATEBUENO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SAN JUAN DE RIOSECO" & 
                            mun_edata.df["desc_depto"] == "CUNDINAMARCA"] <- "SAN JUAN DE RIO SECO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ALTO BAUDO (PIE DE PATO)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "ALTO BAUDO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "BAJO BAUDO (PIZARRO)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "BAJO BAUDO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "MEDIO BAUDO (PUERTO MELUK)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "MEDIO BAUDO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ATRATO (YUTO)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "ATRATO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "BAHIA SOLANO (MUTIS)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "BAHIA SOLANO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "BOJAYA (BELLAVISTA)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "BOJAYA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "EL CANTON DEL SAN PABLO (MAN." & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "EL CANTON DEL SAN PABLO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "MEDIO ATRATO (BETE)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "MEDIO ATRATO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "RIO QUITO (PAIMADO)" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "RIO QUITO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "UNION PANAMERICANA (LAS ANIMAS" & 
                            mun_edata.df["desc_depto"] == "CHOCO"] <- "UNION PANAMERICANA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "LA ARGENTINA (PLATA VIEJA)" & 
                            mun_edata.df["desc_depto"] == "HUILA"] <- "LA ARGENTINA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "TESALIA (CARNICERIAS)" & 
                            mun_edata.df["desc_depto"] == "HUILA"] <- "TESALIA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ARIGUANI (EL DIFICIL)" & 
                            mun_edata.df["desc_depto"] == "MAGDALENA"] <- "ARIGUANI"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "CERRO DE SAN ANTONIO" & 
                            mun_edata.df["desc_depto"] == "MAGDALENA"] <- "CERRO SAN ANTONIO"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ZONA BANANERA (SEVILLA)" & 
                            mun_edata.df["desc_depto"] == "MAGDALENA"] <- "ZONA BANANERA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SAN MARTIN DE LOS LLANOS" & 
                            mun_edata.df["desc_depto"] == "META"] <- "SAN MARTIN"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "VISTA HERMOSA" & 
                            mun_edata.df["desc_depto"] == "META"] <- "VISTAHERMOSA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ALBAN (SAN JOSE)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "ALBAN"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ARBOLEDA (BERRUECOS)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "ARBOLEDA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "COLON (GENOVA)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "COLON"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "CUASPUD (CARLOSAMA)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "CUASPUD"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "EL TABLON" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "EL TABLON DE GOMEZ"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "LOS ANDES (SOTOMAYOR)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "LOS ANDES"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "MAGUI (PAYAN)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "MAGUI"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "MALLAMA (PIEDRANCHA)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "MALLAMA"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "FRANCISCO PIZARRO (SALAHONDA)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "FRANCISCO PIZARRO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ROBERTO PAYAN (SAN JOSE)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "ROBERTO PAYAN"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SANTA BARBARA (ISCUANDE)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "SANTA BARBARA"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SANTACRUZ (GUACHAVES)" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "SANTACRUZ"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "TUMACO" & 
                            mun_edata.df["desc_depto"] == "NARINO"] <- "SAN ANDRES DE TUMACO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "EL CARMEN" & 
                            mun_edata.df["desc_depto"] == "SANTANDER"] <- "EL CARMEN DE CHUCURI"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "COLOSO (RICAURTE)" & 
                            mun_edata.df["desc_depto"] == "SUCRE"] <- "COLOSO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "GALERAS (NUEVA GRANADA)" & 
                            mun_edata.df["desc_depto"] == "SUCRE"] <- "GALERAS"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SAN JUAN DE BETULIA (BETULIA)" & 
                            mun_edata.df["desc_depto"] == "SUCRE"] <- "SAN JUAN DE BETULIA"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "TOLU" & 
                            mun_edata.df["desc_depto"] == "SUCRE"] <- "SANTIAGO DE TOLU"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "TOLUVIEJO" & 
                            mun_edata.df["desc_depto"] == "SUCRE"] <- "TOLU VIEJO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "ARMERO (GUAYABAL)" & 
                            mun_edata.df["desc_depto"] == "TOLIMA"] <- "ARMERO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PAZ DE ARIPORO (MORENO)" & 
                            mun_edata.df["desc_depto"] == "CASANARE"] <- "PAZ DE ARIPORO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "PUERTO LEGUIZAMO" & 
                            mun_edata.df["desc_depto"] == "PUTUMAYO"] <- "LEGUIZAMO"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "SAN MIGUEL (LA DORADA)" & 
                            mun_edata.df["desc_depto"] == "PUTUMAYO"] <- "SAN MIGUEL"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "VALLE DEL GUAMUEZ (LA HORMIGA)" & 
                            mun_edata.df["desc_depto"] == "PUTUMAYO"] <- "VALLE DEL GUAMUEZ"	
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "BUGA" & 
                            mun_edata.df["desc_depto"] == "VALLE DEL CAUCA"] <- "GUADALAJARA DE BUGA"
mun_edata.df["desc_mpio"][mun_edata.df["desc_mpio"] == "CALIMA (DARIEN)" & 
                            mun_edata.df["desc_depto"] == "VALLE DEL CAUCA"] <- "CALIMA"

# Loading municipality DANE ID codes
mun_id.df <- read_xlsx("./Data/mun_id.xlsx") %>%
  mutate(municipality = str_replace_all(toupper(municipality), 
                                        c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", 
                                          "Ú|Ü" = "U", "Ñ" = "N")),
         state = str_replace_all(toupper(state), 
                                 c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", 
                                   "Ú|Ü" = "U", "Ñ" = "N"))) %>%
  stringdist_left_join(mun_edata.df %>%
                         filter(edata_year == 2015) %>%
                         select(1:4),
                       by = c("municipality" = "desc_mpio","state" = "desc_depto"),
                       method = "qgram",
                       ignore_case = T,
                       max_dist = 0.8) %>%
  select(-c(5,7))

# Manually adding codes from LA SIERRA, CAUCA
mun_id.df["depto"][mun_id.df["municipality"] == "LA SIERRA" & 
                     mun_id.df["state"] == "CAUCA"] <- 11
mun_id.df["mpio"][mun_id.df["municipality"] == "LA SIERRA" & 
                     mun_id.df["state"] == "CAUCA"] <- 37
mun_id.df <- mun_id.df %>%
  filter(!is.na(mpio))
        # Municipalities of Santa Fe (Antioquia), El Carmen de Atrato (Choco) and 
        # San Luis de Since (Sucre) don't have any electoral data registered for 2011, 2015 or 2019.


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                6. LOADING OUTCOMES AND CONTROLS                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading social programs data (main outcomes):
socprog.df <- read_delim("./Data/Data - Raw/Social Programs/yearly_mun_inv.csv", delim = ",", 
                         col_types = cols(.default = "d", 
                                          "MUNICIPIO" = "c",
                                          "DEPARTAMENTO" = "c"),
                         locale = locale(encoding = "latin1")) %>%
  select(id_dane = DANE, state = DEPARTAMENTO, municipality = MUNICIPIO, year = Ano_Fecha,
         MFA_PER, MFA_FAM, MFA_INV,
         starts_with("MFA_UNI"),
         starts_with("MFA_SIS"),
         starts_with("MFA_DES"),
         starts_with("NN_PAI_"),
         starts_with("AHE_"),
         starts_with("AHD_")) %>%
  arrange(id_dane, year) %>%
  filter(year > 2013)
        # Not even the locale = locale(encoding = ) could solve the string problems in the municipality
        # and state strings. Drop them.

# Loading controls | baseline checks:

# Loading general characts data:
mun_gdata.df <- read_dta("./Data/Data - Raw/Panel CEDE/Microdatos/PANEL_CARACTERISTICAS_GENERALES(2019).dta",
                         encoding = "latin1") %>%
  select(id_dane = codmpio, year = ano, pobl_tot, indrural, discapital, areaoficialkm2, pobreza, 
         gini, pib_cons, pib_percapita_cons, pib_total, pib_agricola) %>%
  filter(year > 1999)

# Loading Gov. transfer data 
gov_transfers.df <- read_dta("./Data/Data - Raw/Panel CEDE/Microdatos/PANEL_BUEN_GOBIERNO(2019).dta",
                             encoding = "latin1") %>%
  select(id_dane = codmpio, year = ano, y_total, SGP_total) %>%
  filter(year > 1999)

# Loading education data:
education.df <- read_dta("./Data/Data - Raw/Panel CEDE/Microdatos/PANEL_DE_EDUCACION(2019).dta",
                              encoding = "latin1") %>%
  select(id_dane = codmpio, year = ano, ind_alfa, anos_est_mun) %>%
  filter(year > 1999)

# Loading health data:
health.df <- read_dta("./Data/Data - Raw/Panel CEDE/Microdatos/PANEL_SALUD_Y_SERVICIOS(2019).dta",
                         encoding = "latin1") %>%
  select(id_dane = codmpio, year = ano, TMI) %>%
  filter(year > 1999)

# Loading conflict data
conflict.df <- read_dta("./Data/Data - Raw/Panel CEDE/Microdatos/PANEL_CONFLICTO_Y_VIOLENCIA(2019).dta",
                        encoding = "latin1") %>%
  select(id_dane = codmpio, year = ano,
         tpobc_FARC, tpobc_ELN, tpobc_DESC, tpobc_AUC, enfr_FARC, enfr_ELN, enfr_DESC, enfr_AUC,
         desmovilizados, desplazados_expulsion, desplazados_recepcion) %>%
  filter(year > 1999)

# Constructing a full covariates dataset:
mun_panel.df <- mun_id.df %>%
  right_join(gov_transfers.df, by = "id_dane") %>%
  filter(!is.na(municipality ) & !is.na(state)) %>%
      # A lot of municipalities from the staes of Amazonas, Vaupes, Guainia and San Andres
      # are not present in mun_id, probably because they are not "fully" municipalities
      # They also don't present electoral data.
  left_join(mun_gdata.df, by = c("id_dane", "year")) %>%
  left_join(socprog.df %>% select(-c(2,3)), by = c("id_dane", "year")) %>%
  left_join(education.df, by = c("id_dane", "year")) %>%
  left_join(health.df, by = c("id_dane", "year")) %>%
  left_join(conflict.df, by = c("id_dane", "year")) %>%
  mutate(edata_year = if_else(year == 2014 | year == 2015, 2011, 
                              if_else(year >= 2016 & year < 2019, 2015,
                                      if_else(year == 2019, 2018, NA_real_))))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                7. CREATING A BASELINE COVARIATES DATASET FOR BALANCE CHECKS                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Constructing a baseline covariates dataset:
balance.df <- mun_panel.df %>%
  filter(year<2010) %>%
  group_by(id_dane) %>%
  summarise(state = first(state),
            municipality = first(municipality),
            depto = first(depto),
            mpio = first(mpio),
            # across(c(6:17,36:49), ~mean(.x, na.rm = T)),
            across(c(6:17,36:38), ~mean(.x, na.rm = T)),
            across(c(39:49), ~sum(.x, na.rm = T))) %>%
  left_join(mun_edata.df %>% 
              filter(edata_year == 2011) %>%
              select(depto, mpio, pres_pp_bwon, margin_pres, sample_pres),
            by = c("depto", "mpio")) %>%
  rename(polalgn_2011 = pres_pp_bwon, 
         margin_2011 = margin_pres,
         sample_2011 = sample_pres) %>%
  left_join(mun_edata.df %>% 
              filter(edata_year == 2015) %>%
              select(depto, mpio, pres_pp_bwon, margin_pres, sample_pres),
            by = c("depto", "mpio")) %>%
  rename(polalgn_2015 = pres_pp_bwon, 
         margin_2015 = margin_pres,
         sample_2015 = sample_pres) %>%
  mutate(SGP_total = SGP_total/1000000)

# Saving dataset
write_delim(balance.df, "./Data/Data - Processed/colombia_balance.csv", delim = ";")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                8. CREATING THE MAIN DATASET                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Filtering and joining data
mun_epanel.df <- mun_panel.df %>%
  filter(year >= 2014 & year < 2019) %>%
  select(1:6, 9, 19:30, 51) %>%
  left_join(mun_edata.df, by = c("depto", "mpio", "edata_year")) %>%
  mutate(MFA_pc = MFA_INV/MFA_PER,
         MFA_SIS_pc = MFA_SIS_INV/MFA_SIS_PER,
         MFA_DES_pc = MFA_DES_INV/MFA_DES_PER,
         MFA_UNI_pc = MFA_UNI_INV/MFA_UNI_PER,
         # MFA_FAM = (MFA_FAM/pobl_tot)*1000,
         # MFA_SIS_FAM = (MFA_SIS_FAM/pobl_tot)*1000,
         # MFA_DES_FAM = (MFA_DES_FAM/pobl_tot)*1000,
         # MFA_UNI_FAM = (MFA_UNI_FAM/pobl_tot)*1000
         ) %>%
  select(-pobl_tot)
  
# Saving dataset
write_delim(mun_epanel.df, "./Data/Data - Processed/colombia_panel.csv", delim = ";")
write_delim(mun_edata.df, "./Data/Data - Processed/mun_edata.csv", delim = ";")

  