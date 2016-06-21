## 2008

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2008
# 1)	Iniciados antes de 2008 e encerrados antes de 2008 
inicio_antes_fim_antes08 <- org3 %>% 
  filter(Fim < dmy("01/01/2008"))

# 2)	Iniciado antes de 2008 e ainda não encerrados 
inicio_antes_fim_na08 <- org3 %>% 
  filter((Inicio < dmy("01/01/2008")) & ((is.na(Fim)))) 
inicio_antes_fim_na08_D <- inicio_antes_fim_na08 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na08_CP <- inicio_antes_fim_na08 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na08_CI <- inicio_antes_fim_na08 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na08 <- rbind(inicio_antes_fim_na08_D, inicio_antes_fim_na08_CI, inicio_antes_fim_na08_CP)
rm(inicio_antes_fim_na08_CI, inicio_antes_fim_na08_CP, inicio_antes_fim_na08_D)

# 3)	Iniciado antes de 2008 e encerrados depois de 2008 
inicio_antes_fim_depois08 <- org3 %>% 
  filter((Inicio < dmy("01/01/2008")) & ((Fim > dmy("31/12/2008")))) 
inicio_antes_fim_depois08_D <- inicio_antes_fim_depois08 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois08_CP <- inicio_antes_fim_depois08 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois08_CI <- inicio_antes_fim_depois08 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois08 <- rbind(inicio_antes_fim_depois08_D, inicio_antes_fim_depois08_CI, inicio_antes_fim_depois08_CP)
rm(inicio_antes_fim_depois08_CI, inicio_antes_fim_depois08_CP, inicio_antes_fim_depois08_D)

# 4)	Iniciados antes de 2008 e encerrados em 2008
inicio_antes_fim_durante08 <- org3 %>% 
  filter(Inicio < dmy("01/01/2008")) %>%
  filter((Fim <= dmy("31/12/2008") & (Fim >= dmy("01/01/2008"))))
inicio_antes_fim_durante08_D <- inicio_antes_fim_durante08 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2008"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante08_CP <- inicio_antes_fim_durante08 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2008"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante08_CI <- inicio_antes_fim_durante08 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2008"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante08 <- rbind(inicio_antes_fim_durante08_D, inicio_antes_fim_durante08_CI, inicio_antes_fim_durante08_CP)
rm(inicio_antes_fim_durante08_CI, inicio_antes_fim_durante08_CP, inicio_antes_fim_durante08_D)

# Durante 2008
# 5)	Iniciado em 2008 e encerrado em 2008
inicio_durante_fim_durante08 <- org3 %>%
  filter((Inicio <= dmy("31/12/2008") & (Inicio >= dmy("01/01/2008")))) %>%
  filter(Fim <= dmy("31/12/2008"))
inicio_durante_fim_durante08_D <- inicio_durante_fim_durante08 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante08_CP <- inicio_durante_fim_durante08 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante08_CI <- inicio_durante_fim_durante08 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante08 <- rbind(inicio_durante_fim_durante08_D, inicio_durante_fim_durante08_CP, inicio_durante_fim_durante08_CI)
rm(inicio_durante_fim_durante08_CI, inicio_durante_fim_durante08_D, inicio_durante_fim_durante08_CP)

# 6)	Iniciado em 2008 e não encerrado
inicio_durante_fim_na08 <- org3 %>%
  filter((Inicio <= dmy("31/12/2008") & (Inicio >= dmy("01/01/2008")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na08_D <- inicio_durante_fim_na08 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2008"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na08_CP <- inicio_durante_fim_na08 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2008"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na08_CI <- inicio_durante_fim_na08 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2008"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na08 <- rbind(inicio_durante_fim_na08_D, inicio_durante_fim_na08_CP, inicio_durante_fim_na08_CI)
rm(inicio_durante_fim_na08_CI, inicio_durante_fim_na08_D, inicio_durante_fim_na08_CP)

# 7)	Iniciado em 2008 e encerrado depois de 2008
inicio_durante_fim_depois08 <- org3 %>%
  filter((Inicio <= dmy("31/12/2008") & (Inicio >= dmy("01/01/2008")))) %>%
  filter((Fim > dmy("31/12/2008")))
inicio_durante_fim_depois08_D <- inicio_durante_fim_depois08 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2008"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois08_CP <- inicio_durante_fim_depois08 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2008"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois08_CI <- inicio_durante_fim_depois08 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2008"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois08 <- rbind(inicio_durante_fim_depois08_D, inicio_durante_fim_depois08_CP, inicio_durante_fim_depois08_CI)
rm(inicio_durante_fim_depois08_CI, inicio_durante_fim_depois08_D, inicio_durante_fim_depois08_CP)

#Juntando o somatorio de dias
dias08 <- rbind(inicio_antes_fim_depois08, inicio_antes_fim_na08, inicio_antes_fim_durante08, inicio_durante_fim_durante08, inicio_durante_fim_depois08, inicio_durante_fim_na08)
dias08 <- dias08 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao08 <- org3 %>% 
  filter((Fim >= dmy("31/12/2008") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2008"))
posicao08 <- posicao08 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao08 <- unique(posicao08)

posicao08 <- merge(posicao08, chave08,  all.x = T, all.y = T)
posicao08_2 <- posicao08 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao08_3 <- merge(posicao08, chave08,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao08 <- rbind(posicao08_2, posicao08_3)  
rm(posicao08_2, posicao08_3)

# Gerando Balanco de 2008
bal_08 <- merge(chave08, dias08, "Chave", all.x = T, all.y = T)

bal_08 <- merge(bal_08, posicao08, "Chave", all.x = T, all.y = T)
bal_08_1 <- bal_08 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_08_2 <- bal_08 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_08 <- rbind(bal_08_1, bal_08_2)
rm(bal_08_1, bal_08_2)

bal_08 <- bal_08 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_08$Partido)
as.factor(bal_08$UF)
as.factor(bal_08$Posicao)

rm(inicio_antes_fim_antes08, inicio_antes_fim_na08, inicio_antes_fim_durante08,   inicio_antes_fim_depois08, inicio_durante_fim_na08, inicio_durante_fim_durante08, inicio_durante_fim_depois08, posicao08)
write.csv2(bal_08, "bal_08.csv")
