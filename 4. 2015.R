## 2015

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2015
# 1)	Iniciados antes de 2015 e encerrados antes de 2015 
inicio_antes_fim_antes15 <- org3 %>% 
  filter(Fim < dmy("01/01/2015"))

# 2)	Iniciado antes de 2015 e ainda não encerrados 
inicio_antes_fim_na15 <- org3 %>% 
  filter((Inicio < dmy("01/01/2015")) & ((is.na(Fim)))) 
inicio_antes_fim_na15_D <- inicio_antes_fim_na15 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na15_CP <- inicio_antes_fim_na15 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na15_CI <- inicio_antes_fim_na15 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na15 <- rbind(inicio_antes_fim_na15_D, inicio_antes_fim_na15_CI, inicio_antes_fim_na15_CP)
rm(inicio_antes_fim_na15_CI, inicio_antes_fim_na15_CP, inicio_antes_fim_na15_D)

# 3)	Iniciado antes de 2015 e encerrados depois de 2015 
inicio_antes_fim_depois15 <- org3 %>% 
  filter((Inicio < dmy("01/01/2015")) & ((Fim > dmy("31/12/2015")))) 
inicio_antes_fim_depois15_D <- inicio_antes_fim_depois15 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois15_CP <- inicio_antes_fim_depois15 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois15_CI <- inicio_antes_fim_depois15 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois15 <- rbind(inicio_antes_fim_depois15_D, inicio_antes_fim_depois15_CI, inicio_antes_fim_depois15_CP)
rm(inicio_antes_fim_depois15_CI, inicio_antes_fim_depois15_CP, inicio_antes_fim_depois15_D)

# 4)	Iniciados antes de 2015 e encerrados em 2015
inicio_antes_fim_durante15 <- org3 %>% 
  filter(Inicio < dmy("01/01/2015")) %>%
  filter((Fim <= dmy("31/12/2015") & (Fim >= dmy("01/01/2015"))))
inicio_antes_fim_durante15_D <- inicio_antes_fim_durante15 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2015"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante15_CP <- inicio_antes_fim_durante15 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2015"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante15_CI <- inicio_antes_fim_durante15 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2015"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante15 <- rbind(inicio_antes_fim_durante15_D, inicio_antes_fim_durante15_CI, inicio_antes_fim_durante15_CP)
rm(inicio_antes_fim_durante15_CI, inicio_antes_fim_durante15_CP, inicio_antes_fim_durante15_D)

# Durante 2015
# 5)	Iniciado em 2015 e encerrado em 2015
inicio_durante_fim_durante15 <- org3 %>%
  filter((Inicio <= dmy("31/12/2015") & (Inicio >= dmy("01/01/2015")))) %>%
  filter(Fim <= dmy("31/12/2015"))
inicio_durante_fim_durante15_D <- inicio_durante_fim_durante15 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante15_CP <- inicio_durante_fim_durante15 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante15_CI <- inicio_durante_fim_durante15 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante15 <- rbind(inicio_durante_fim_durante15_D, inicio_durante_fim_durante15_CP, inicio_durante_fim_durante15_CI)
rm(inicio_durante_fim_durante15_CI, inicio_durante_fim_durante15_D, inicio_durante_fim_durante15_CP)

# 6)	Iniciado em 2015 e não encerrado
inicio_durante_fim_na15 <- org3 %>%
  filter((Inicio <= dmy("31/12/2015") & (Inicio >= dmy("01/01/2015")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na15_D <- inicio_durante_fim_na15 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2015"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na15_CP <- inicio_durante_fim_na15 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2015"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na15_CI <- inicio_durante_fim_na15 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2015"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na15 <- rbind(inicio_durante_fim_na15_D, inicio_durante_fim_na15_CP, inicio_durante_fim_na15_CI)
rm(inicio_durante_fim_na15_CI, inicio_durante_fim_na15_D, inicio_durante_fim_na15_CP)

# 7)	Iniciado em 2015 e encerrado depois de 2015
inicio_durante_fim_depois15 <- org3 %>%
  filter((Inicio <= dmy("31/12/2015") & (Inicio >= dmy("01/01/2015")))) %>%
  filter((Fim > dmy("31/12/2015")))
inicio_durante_fim_depois15_D <- inicio_durante_fim_depois15 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2015"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois15_CP <- inicio_durante_fim_depois15 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2015"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois15_CI <- inicio_durante_fim_depois15 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2015"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois15 <- rbind(inicio_durante_fim_depois15_D, inicio_durante_fim_depois15_CP, inicio_durante_fim_depois15_CI)
rm(inicio_durante_fim_depois15_CI, inicio_durante_fim_depois15_D, inicio_durante_fim_depois15_CP)

#Juntando o somatorio de dias
dias15 <- rbind(inicio_antes_fim_depois15, inicio_antes_fim_na15, inicio_antes_fim_durante15, inicio_durante_fim_durante15, inicio_durante_fim_depois15, inicio_durante_fim_na15)
dias15 <- dias15 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao15 <- org3 %>% 
  filter((Fim >= dmy("31/12/2015") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2015"))
posicao15 <- posicao15 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao15 <- unique(posicao15)

posicao15 <- merge(posicao15, chave15,  all.x = T, all.y = T)
posicao15_2 <- posicao15 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao15_3 <- merge(posicao15, chave15,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao15 <- rbind(posicao15_2, posicao15_3)  
rm(posicao15_2, posicao15_3)

# Gerando Balanco de 2015
bal_15 <- merge(chave15, dias15, "Chave", all.x = T, all.y = T)

bal_15 <- merge(bal_15, posicao15, "Chave", all.x = T, all.y = T)
bal_15_1 <- bal_15 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_15_2 <- bal_15 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_15 <- rbind(bal_15_1, bal_15_2)
rm(bal_15_1, bal_15_2)

bal_15 <- bal_15 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_15$Partido)
as.factor(bal_15$UF)
as.factor(bal_15$Posicao)

rm(inicio_antes_fim_antes15, inicio_antes_fim_na15, inicio_antes_fim_durante15,  inicio_antes_fim_depois15, inicio_durante_fim_na15, inicio_durante_fim_durante15, inicio_durante_fim_depois15, posicao15)
write.csv2(bal_15, "bal_15.csv")
