# Carregando pacotes------------------------------------------------------------

#install.packages("PNADcIBGE")
library(PNADcIBGE)
#install.packages("survey")
library(survey)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("readxl")
library(readxl)
#install.packages("data.table")
library(data.table)
#install.packages("Hmisc")
library(Hmisc)

# Importando dados da PNADc-----------------------------------------------------

# Definindo ano de referência---------------------------------------------------

year <- 2019

# Importando dados--------------------------------------------------------------

pnad_raw <- get_pnadc(year = year,
                      interview = 5)

# Criando objeto pnad-----------------------------------------------------------

pnad <- pnad_raw$variables

# Salvando base de dados--------------------------------------------------------

save(pnad,file='C:/Users/lauro/Desktop/Progressividade/PNADc_2019')

# Carregando base de dados do desktop-------------------------------------------

load('C:/Users/lauro/Desktop/Progressividade/PNADc_2019')

# Criando variáveis para compatibilização---------------------------------------

# Identificando domicílios e observações----------------------------------------

pnad <- pnad %>%
  mutate(
    idorighh = paste0(UPA, V1008, V1014),
    idorigperson = paste0(idorighh, V2003))

# UPA: Unidade Primária de Amostragem. UF (2) + Número Sequencial (6) + DV (1).
# V1008: Número de seleção do domicílio.
# V1014: Painel. Grupo de amostra.

# Simplificando o identificador e ordenando-------------------------------------
pnad <- pnad %>% 
  group_by(idorighh) %>%
  mutate(idhh = cur_group_id(),
         idperson = paste0(idhh, "0", row_number())) %>% 
  ungroup(idorighh)%>% 
  arrange(as.numeric(idhh)) 

# Identificando pessoa responsável pelo domicílio-------------------------------

pnad <- pnad %>% 
  mutate(idhead = ifelse(pnad$V2005 == "Pessoa responsável pelo domicílio",
                         yes = as.numeric(pnad$idperson),
                         no = 999999999999999999)) %>% 
  group_by(idhh) %>% 
  mutate(idhead = as.character(min(idhead))) %>% 
  ungroup(idhh)

# Variáveis categóricas (raça, gênero, situação domiciliar)---------------------

#Variáveis de raça, idade e gênero----------------------------------------------

pnad <- pnad %>% 
  mutate(dwt = V1032,
         dag = V2009,
         dgn = V2007) %>% 
  mutate(dgn = case_when(dgn == "Homem"  ~ 1,
                         dgn == "Mulher" ~ 0),
         dra = case_when(V2010 == "Branca"   ~ 1,
                         V2010 == "Preta"    ~ 2,
                         V2010 == "Amarela"  ~ 3,
                         V2010 == "Parda"    ~ 4,
                         V2010 == "Indígena" ~ 5,
                         V2010 == "Ignorado" ~ 9))

# V1032: peso
# V2007: sexo
# V2009: idade do morador na data de referência
# V2010: cor ou raça

# Criando regiões---------------------------------------------------------------

pnad <-
  pnad %>% mutate(
    UF = as.character(UF), 
    norte = ifelse(UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins"), 1,0),
    nordeste = ifelse(UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia"), 1, 0),
    sudeste = ifelse(UF %in% c("Minas Gerais", "Espírito Santos", "Rio de Janeiro", "São Paulo"), 1, 0),
    sul = ifelse(UF %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul"), 1, 0),
    centro = ifelse(UF %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"), 1, 0))

# UF: Unidade da Federação

# Região metropolitana e capital------------------------------------------------

pnad <- pnad %>% 
  mutate(
    V1023 = as.character(V1023),
    metropole = ifelse(V1023 %in% c(
      "Capital", "Resto da RM (Região Metropolitana, excluindo a capital)"),1,0))

# V1023: Tipo de área (1: Capital; 2: Resto da RM; 3: Resto da RIDE; 4: Resto da UF)

# Rural/urbano------------------------------------------------------------------

pnad <- pnad %>%
  mutate(
    V1022 = as.character(V1022),
    rural = ifelse(V1022 == "Rural", 1, 0))

# V1022: situação do domicílio

# Número de pessoas no domícilio------------------------------------------------

pnad <- pnad %>%
  mutate(
    pessoas = V2001
  )

# Número de crianças no domicílio-----------------------------------------------

pnad <- pnad %>%
  group_by(idhh) %>%  
  mutate(criancas = sum(V2009 <= 14, na.rm = TRUE)) %>%  
  ungroup() 

#pnad <- pnad %>%
#  group_by(idorighh) %>%  
#  mutate(criancas = sum(dag <= 14, na.rm = TRUE)) %>%  
#  ungroup() 

# V2009: idade do morador na data de referência.

# Número de idosos no domicílio-------------------------------------------------

pnad <- pnad %>%
  group_by(idhh) %>%  
  mutate(idosos = sum(V2009 >= 60, na.rm = TRUE)) %>%  
  ungroup() 


#pnad <- pnad %>%
#  group_by(idorighh) %>%  
#  mutate(idosos = sum(dag >= 60, na.rm = TRUE)) %>%  
#  ungroup() 

# V2009: idade do morador na data de referência.

# Rendimento do trabalho--------------------------------------------------------

pnad <- pnad %>%
  mutate(
    renda1 = ifelse(is.na(V403312), 0, V403312),
    renda2 = ifelse(is.na(V405012), 0, V405012), 
    renda_trabalho = renda1 + renda2)

# V403312: Qual era o rendimento bruto/retirada mensal que ... recebia/fazia 
# normalmente nesse trabalho ? (valor em dinheiro)

# V405012: valor em dinheiro do rendimento mensal que recebia normalmente nesse 
# trabalho secundário 


# Rendimento domiciliar per capita----------------------------------------------

pnad <- pnad %>%
  group_by(idhh) %>%
  mutate(rendimento_trabalho = sum(renda_trabalho) / pessoas)

# Filtrando pelo responsável pelo domicílio-------------------------------------

pnad <- pnad %>% filter(V2005 == "Pessoa responsável pelo domicílio")


# décimos de renda--------------------------------------------------------------

# Outro critério de renda ?


# Décimos de Rendimento do trabalho---------------------------------------------

decimos_trabalho <- wtd.quantile(pnad$rendimento_trabalho, weights = pnad$V1032, probs = seq(0, 1, by = 0.1))

pnad <- pnad %>%
  mutate(decimo_trabalho1e2 = ifelse(rendimento_trabalho <= decimos_trabalho[3], 1, 0),
         decimo_trabalho3 = ifelse(rendimento_trabalho > decimos_trabalho[3] & rendimento_trabalho <= decimos_trabalho[4], 1, 0),
         decimo_trabalho4 = ifelse(rendimento_trabalho > decimos_trabalho[4] & rendimento_trabalho <= decimos_trabalho[5], 1, 0),
         decimo_trabalho5 = ifelse(rendimento_trabalho > decimos_trabalho[5] & rendimento_trabalho <= decimos_trabalho[6], 1, 0),
         decimo_trabalho6 = ifelse(rendimento_trabalho > decimos_trabalho[6] & rendimento_trabalho <= decimos_trabalho[7], 1, 0),
         decimo_trabalho7 = ifelse(rendimento_trabalho > decimos_trabalho[7] & rendimento_trabalho <= decimos_trabalho[8], 1, 0),
         decimo_trabalho8 = ifelse(rendimento_trabalho > decimos_trabalho[8] & rendimento_trabalho <= decimos_trabalho[9], 1, 0),
         decimo_trabalho9 = ifelse(rendimento_trabalho > decimos_trabalho[9] & rendimento_trabalho <= decimos_trabalho[10], 1, 0),
         decimo_trabalho10 = ifelse(rendimento_trabalho > decimos_trabalho[10], 1, 0))


# Salvar------------------------------------------------------------------------

save(comp_pnad,file='C:/Users/lauro/Desktop/Progressividade/comp_pnad')