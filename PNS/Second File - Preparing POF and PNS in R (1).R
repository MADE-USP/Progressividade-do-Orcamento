# Carregar pacotes necessários
library('dplyr')
library('readstata13')
library('Hmisc')
library('R.utils')
library('data.table')

# Carregar dados
PNS <- fread("C:/Users/laris/OneDrive/Área de Trabalho/PNAD-ONS/PNS3.gz")

# Convertendo variáveis para numérico
PNS <- PNS %>%
  mutate(across(c(C004, VDC001, E01602, E01802, V00281, GP_Consulta, GP_Medicamento, GP_Internacoes, VDF003, GP_Transportes), 
                ~as.numeric(.)))

# Eliminando pessoas que são empregados, parentes de empregado e pensionistas
PNS <- PNS %>% filter(!(C004 %in% c(17, 18, 19)))

# Eliminando domicílios que não foram entrevistados
PNS <- PNS %>% filter(V0015 == 1)

# Removendo a variável 'gp_total'
PNS <- PNS %>% select(-GP_Total)

# Transformando missing em zero 
PNS <- PNS %>%
  mutate(across(c(GP_Consulta, GP_Medicamento, GP_Internacoes, GP_Transportes),
                ~ifelse(is.na(.), 0, .)))

# Mensalizando os gastos em saúde 
PNS <- PNS %>%
  mutate(across(c(GP_Consulta, GP_Medicamento, GP_Internacoes, GP_Transportes),
                ~./12))

# Fazendo os gastos familiares per capita
PNS <- PNS %>%
  group_by(V0024, UPA_PNS, V0006_PNS) %>%
  mutate(across(c(GP_Consulta, GP_Medicamento, GP_Internacoes, GP_Transportes),
                ~sum(.)/VDC001, .names = "{.col}_PC"))

# Fazendo o gasto total per capita
PNS <- PNS %>%
  mutate(GP_Total_PC = GP_Consulta_PC + GP_Medicamento_PC + GP_Internacoes_PC + GP_Transportes_PC)

# Construindo rendimento do trabalho
PNS <- PNS %>%
  mutate(E01602 = ifelse(is.na(E01602), 0, E01602),
         E01802 = ifelse(is.na(E01802), 0, E01802),
         renda_trabalho = E01602 + E01802)

# Fazendo rendimento domiciliar per capita
PNS <- PNS %>%
  group_by(V0024, UPA_PNS, V0006_PNS) %>%
  mutate(rendimento_trabalho = sum(renda_trabalho) / VDC001)

# Mantendo apenas uma observação por domicílio
PNS <- PNS %>% filter(C004 == 1)

# Criando dummies para as UFs
PNS <- PNS %>%
  mutate(norte = ifelse(V0001 >= 11 & V0001 <= 17, 1, 0),
         nordeste = ifelse(V0001 >= 21 & V0001 <= 29, 1, 0),
         sudeste = ifelse(V0001 >= 31 & V0001 <= 35, 1, 0),
         sul = ifelse(V0001 >= 41 & V0001 <= 43, 1, 0),
         centro = ifelse(V0001 >= 50 & V0001 <= 53, 1, 0))

# Criando dummies para RM 
PNS <- PNS %>% mutate(metropole = ifelse(V0031 %in% c(1, 2), 1, 0))

# Criando dummies rural/urbana 
PNS <- PNS %>% mutate(rural = ifelse(V0026 == 2, 1, 0))

# Renomeando variável de renda
PNS <- PNS %>% rename(renda_monet = VDF003)

# Criando décimos de renda
decimos_renda <- wtd.quantile(PNS$renda_monet, weights = PNS$V00281, probs = seq(0, 1, by = 0.1))

PNS <- PNS %>%
  mutate(decimo_renda1 = ifelse(renda_monet <= decimos_renda[2], 1, 0),
         decimo_renda2 = ifelse(renda_monet > decimos_renda[2] & renda_monet <= decimos_renda[3], 1, 0),
         decimo_renda3 = ifelse(renda_monet > decimos_renda[3] & renda_monet <= decimos_renda[4], 1, 0),
         decimo_renda4 = ifelse(renda_monet > decimos_renda[4] & renda_monet <= decimos_renda[5], 1, 0),
         decimo_renda5 = ifelse(renda_monet > decimos_renda[5] & renda_monet <= decimos_renda[6], 1, 0),
         decimo_renda6 = ifelse(renda_monet > decimos_renda[6] & renda_monet <= decimos_renda[7], 1, 0),
         decimo_renda7 = ifelse(renda_monet > decimos_renda[7] & renda_monet <= decimos_renda[8], 1, 0),
         decimo_renda8 = ifelse(renda_monet > decimos_renda[8] & renda_monet <= decimos_renda[9], 1, 0),
         decimo_renda9 = ifelse(renda_monet > decimos_renda[9] & renda_monet <= decimos_renda[10], 1, 0),
         decimo_renda10 = ifelse(renda_monet > decimos_renda[10], 1, 0))

# Criando décimos de rendimento do trabalho
decimos_trabalho <- wtd.quantile(PNS$rendimento_trabalho, weights = PNS$V00281, probs = seq(0, 1, by = 0.1))

PNS <- PNS %>%
  mutate(decimo_trabalho1e2 = ifelse(rendimento_trabalho <= decimos_trabalho[3], 1, 0),
         decimo_trabalho3 = ifelse(rendimento_trabalho > decimos_trabalho[3] & rendimento_trabalho <= decimos_trabalho[4], 1, 0),
         decimo_trabalho4 = ifelse(rendimento_trabalho > decimos_trabalho[4] & rendimento_trabalho <= decimos_trabalho[5], 1, 0),
         decimo_trabalho5 = ifelse(rendimento_trabalho > decimos_trabalho[5] & rendimento_trabalho <= decimos_trabalho[6], 1, 0),
         decimo_trabalho6 = ifelse(rendimento_trabalho > decimos_trabalho[6] & rendimento_trabalho <= decimos_trabalho[7], 1, 0),
         decimo_trabalho7 = ifelse(rendimento_trabalho > decimos_trabalho[7] & rendimento_trabalho <= decimos_trabalho[8], 1, 0),
         decimo_trabalho8 = ifelse(rendimento_trabalho > decimos_trabalho[8] & rendimento_trabalho <= decimos_trabalho[9], 1, 0),
         decimo_trabalho9 = ifelse(rendimento_trabalho > decimos_trabalho[9] & rendimento_trabalho <= decimos_trabalho[10], 1, 0),
         decimo_trabalho10 = ifelse(rendimento_trabalho > decimos_trabalho[10], 1, 0))

# Mantendo variáveis relevantes e criando peso total
PNS <- PNS %>%
  select(V0001, V0024, UPA_PNS, V00281, VDC001, GP_Consulta_PC, GP_Medicamento_PC, GP_Internacoes_PC,
         GP_Transportes_PC, GP_Total_PC, metropole, rural, starts_with("decimo_renda"), 
         starts_with("decimo_trabalho"), norte, sul, sudeste, nordeste, centro) %>%
  mutate(t_peso = V00281 * VDC001)

# Estatísticas descritivas como teste
PNS %>%
  summarise(across(c(GP_Consulta_PC, GP_Medicamento_PC, GP_Internacoes_PC, GP_Transportes_PC),
                   ~weighted.mean(., w = t_peso, na.rm = TRUE)))

# Salvando os dados processados
saveRDS(PNS, file = "PNS_para_o_R_15_jul.rds")

