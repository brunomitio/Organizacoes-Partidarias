## 2012

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2012
# 1)	Iniciados antes de 2012 e encerrados antes de 2012 
inicio_antes_fim_antes12 <- org3 %>% 
  filter(Fim < dmy("01/01/2012"))

# 2)	Iniciado antes de 2012 e ainda não encerrados 
inicio_antes_fim_na12 <- org3 %>% 
  filter((Inicio < dmy("01/01/2012")) & ((is.na(Fim)))) 
inicio_antes_fim_na12_D <- inicio_antes_fim_na12 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na12_CP <- inicio_antes_fim_na12 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na12_CI <- inicio_antes_fim_na12 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na12 <- rbind(inicio_antes_fim_na12_D, inicio_antes_fim_na12_CI, inicio_antes_fim_na12_CP)
rm(inicio_antes_fim_na12_CI, inicio_antes_fim_na12_CP, inicio_antes_fim_na12_D)

# 3)	Iniciado antes de 2012 e encerrados depois de 2012 
inicio_antes_fim_depois12 <- org3 %>% 
  filter((Inicio < dmy("01/01/2012")) & ((Fim > dmy("31/12/2012")))) 
inicio_antes_fim_depois12_D <- inicio_antes_fim_depois12 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois12_CP <- inicio_antes_fim_depois12 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois12_CI <- inicio_antes_fim_depois12 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois12 <- rbind(inicio_antes_fim_depois12_D, inicio_antes_fim_depois12_CI, inicio_antes_fim_depois12_CP)
rm(inicio_antes_fim_depois12_CI, inicio_antes_fim_depois12_CP, inicio_antes_fim_depois12_D)

# 4)	Iniciados antes de 2012 e encerrados em 2012
inicio_antes_fim_durante12 <- org3 %>% 
  filter(Inicio < dmy("01/01/2012")) %>%
  filter((Fim <= dmy("31/12/2012") & (Fim >= dmy("01/01/2012"))))
inicio_antes_fim_durante12_D <- inicio_antes_fim_durante12 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2012"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante12_CP <- inicio_antes_fim_durante12 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2012"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante12_CI <- inicio_antes_fim_durante12 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2012"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante12 <- rbind(inicio_antes_fim_durante12_D, inicio_antes_fim_durante12_CI, inicio_antes_fim_durante12_CP)
rm(inicio_antes_fim_durante12_CI, inicio_antes_fim_durante12_CP, inicio_antes_fim_durante12_D)

# Durante 2012
# 5)	Iniciado em 2012 e encerrado em 2012
inicio_durante_fim_durante12 <- org3 %>%
  filter((Inicio <= dmy("31/12/2012") & (Inicio >= dmy("01/01/2012")))) %>%
  filter(Fim <= dmy("31/12/2012"))
inicio_durante_fim_durante12_D <- inicio_durante_fim_durante12 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante12_CP <- inicio_durante_fim_durante12 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante12_CI <- inicio_durante_fim_durante12 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante12 <- rbind(inicio_durante_fim_durante12_D, inicio_durante_fim_durante12_CP, inicio_durante_fim_durante12_CI)
rm(inicio_durante_fim_durante12_CI, inicio_durante_fim_durante12_D, inicio_durante_fim_durante12_CP)

# 6)	Iniciado em 2012 e não encerrado
inicio_durante_fim_na12 <- org3 %>%
  filter((Inicio <= dmy("31/12/2012") & (Inicio >= dmy("01/01/2012")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na12_D <- inicio_durante_fim_na12 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2012"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na12_CP <- inicio_durante_fim_na12 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2012"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na12_CI <- inicio_durante_fim_na12 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2012"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na12 <- rbind(inicio_durante_fim_na12_D, inicio_durante_fim_na12_CP, inicio_durante_fim_na12_CI)
rm(inicio_durante_fim_na12_CI, inicio_durante_fim_na12_D, inicio_durante_fim_na12_CP)

# 7)	Iniciado em 2012 e encerrado depois de 2012
inicio_durante_fim_depois12 <- org3 %>%
  filter((Inicio <= dmy("31/12/2012") & (Inicio >= dmy("01/01/2012")))) %>%
  filter((Fim > dmy("31/12/2012")))
inicio_durante_fim_depois12_D <- inicio_durante_fim_depois12 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2012"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois12_CP <- inicio_durante_fim_depois12 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2012"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois12_CI <- inicio_durante_fim_depois12 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2012"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois12 <- rbind(inicio_durante_fim_depois12_D, inicio_durante_fim_depois12_CP, inicio_durante_fim_depois12_CI)
rm(inicio_durante_fim_depois12_CI, inicio_durante_fim_depois12_D, inicio_durante_fim_depois12_CP)

#Juntando o somatorio de dias
dias12 <- rbind(inicio_antes_fim_depois12, inicio_antes_fim_na12, inicio_antes_fim_durante12, inicio_durante_fim_durante12, inicio_durante_fim_depois12, inicio_durante_fim_na12)
dias12 <- dias12 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao12 <- org3 %>% 
  filter((Fim >= dmy("31/12/2012") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2012"))
posicao12 <- posicao12 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao12 <- unique(posicao12)

posicao12 <- merge(posicao12, chave12,  all.x = T, all.y = T)
posicao12_2 <- posicao12 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao12_3 <- merge(posicao12, chave12,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao12 <- rbind(posicao12_2, posicao12_3)  
rm(posicao12_2, posicao12_3)

# Gerando Balanco de 2012
bal_12 <- merge(chave12, dias12, "Chave", all.x = T, all.y = T)

bal_12 <- merge(bal_12, posicao12, "Chave", all.x = T, all.y = T)
bal_12_1 <- bal_12 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_12_2 <- bal_12 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_12 <- rbind(bal_12_1, bal_12_2)
rm(bal_12_1, bal_12_2)

bal_12 <- bal_12 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_12$Partido)
as.factor(bal_12$UF)
as.factor(bal_12$Posicao)

rm(inicio_antes_fim_antes12, inicio_antes_fim_na12, inicio_antes_fim_durante12,  inicio_antes_fim_depois12, inicio_durante_fim_na12, inicio_durante_fim_durante12, inicio_durante_fim_depois12, posicao12)
write.csv2(bal_12, "bal_12.csv")