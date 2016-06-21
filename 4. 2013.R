## 2013

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2013
# 1)	Iniciados antes de 2013 e encerrados antes de 2013 
inicio_antes_fim_antes13 <- org3 %>% 
  filter(Fim < dmy("01/01/2013"))

# 2)	Iniciado antes de 2013 e ainda não encerrados 
inicio_antes_fim_na13 <- org3 %>% 
  filter((Inicio < dmy("01/01/2013")) & ((is.na(Fim)))) 
inicio_antes_fim_na13_D <- inicio_antes_fim_na13 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na13_CP <- inicio_antes_fim_na13 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na13_CI <- inicio_antes_fim_na13 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na13 <- rbind(inicio_antes_fim_na13_D, inicio_antes_fim_na13_CI, inicio_antes_fim_na13_CP)
rm(inicio_antes_fim_na13_CI, inicio_antes_fim_na13_CP, inicio_antes_fim_na13_D)

# 3)	Iniciado antes de 2013 e encerrados depois de 2013 
inicio_antes_fim_depois13 <- org3 %>% 
  filter((Inicio < dmy("01/01/2013")) & ((Fim > dmy("31/12/2013")))) 
inicio_antes_fim_depois13_D <- inicio_antes_fim_depois13 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois13_CP <- inicio_antes_fim_depois13 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois13_CI <- inicio_antes_fim_depois13 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois13 <- rbind(inicio_antes_fim_depois13_D, inicio_antes_fim_depois13_CI, inicio_antes_fim_depois13_CP)
rm(inicio_antes_fim_depois13_CI, inicio_antes_fim_depois13_CP, inicio_antes_fim_depois13_D)

# 4)	Iniciados antes de 2013 e encerrados em 2013
inicio_antes_fim_durante13 <- org3 %>% 
  filter(Inicio < dmy("01/01/2013")) %>%
  filter((Fim <= dmy("31/12/2013") & (Fim >= dmy("01/01/2013"))))
inicio_antes_fim_durante13_D <- inicio_antes_fim_durante13 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2013"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante13_CP <- inicio_antes_fim_durante13 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2013"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante13_CI <- inicio_antes_fim_durante13 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2013"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante13 <- rbind(inicio_antes_fim_durante13_D, inicio_antes_fim_durante13_CI, inicio_antes_fim_durante13_CP)
rm(inicio_antes_fim_durante13_CI, inicio_antes_fim_durante13_CP, inicio_antes_fim_durante13_D)

# Durante 2013
# 5)	Iniciado em 2013 e encerrado em 2013
inicio_durante_fim_durante13 <- org3 %>%
  filter((Inicio <= dmy("31/12/2013") & (Inicio >= dmy("01/01/2013")))) %>%
  filter(Fim <= dmy("31/12/2013"))
inicio_durante_fim_durante13_D <- inicio_durante_fim_durante13 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante13_CP <- inicio_durante_fim_durante13 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante13_CI <- inicio_durante_fim_durante13 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante13 <- rbind(inicio_durante_fim_durante13_D, inicio_durante_fim_durante13_CP, inicio_durante_fim_durante13_CI)
rm(inicio_durante_fim_durante13_CI, inicio_durante_fim_durante13_D, inicio_durante_fim_durante13_CP)

# 6)	Iniciado em 2013 e não encerrado
inicio_durante_fim_na13 <- org3 %>%
  filter((Inicio <= dmy("31/12/2013") & (Inicio >= dmy("01/01/2013")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na13_D <- inicio_durante_fim_na13 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2013"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na13_CP <- inicio_durante_fim_na13 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2013"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na13_CI <- inicio_durante_fim_na13 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2013"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na13 <- rbind(inicio_durante_fim_na13_D, inicio_durante_fim_na13_CP, inicio_durante_fim_na13_CI)
rm(inicio_durante_fim_na13_CI, inicio_durante_fim_na13_D, inicio_durante_fim_na13_CP)

# 7)	Iniciado em 2013 e encerrado depois de 2013
inicio_durante_fim_depois13 <- org3 %>%
  filter((Inicio <= dmy("31/12/2013") & (Inicio >= dmy("01/01/2013")))) %>%
  filter((Fim > dmy("31/12/2013")))
inicio_durante_fim_depois13_D <- inicio_durante_fim_depois13 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2013"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois13_CP <- inicio_durante_fim_depois13 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2013"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois13_CI <- inicio_durante_fim_depois13 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2013"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois13 <- rbind(inicio_durante_fim_depois13_D, inicio_durante_fim_depois13_CP, inicio_durante_fim_depois13_CI)
rm(inicio_durante_fim_depois13_CI, inicio_durante_fim_depois13_D, inicio_durante_fim_depois13_CP)

#Juntando o somatorio de dias
dias13 <- rbind(inicio_antes_fim_depois13, inicio_antes_fim_na13, inicio_antes_fim_durante13, inicio_durante_fim_durante13, inicio_durante_fim_depois13, inicio_durante_fim_na13)
dias13 <- dias13 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao13 <- org3 %>% 
  filter((Fim >= dmy("31/12/2013") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2013"))
posicao13 <- posicao13 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao13 <- unique(posicao13)

posicao13 <- merge(posicao13, chave13,  all.x = T, all.y = T)
posicao13_2 <- posicao13 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao13_3 <- merge(posicao13, chave13,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao13 <- rbind(posicao13_2, posicao13_3)  
rm(posicao13_2, posicao13_3)

# Gerando Balanco de 2013
bal_13 <- merge(chave13, dias13, "Chave", all.x = T, all.y = T)

bal_13 <- merge(bal_13, posicao13, "Chave", all.x = T, all.y = T)
bal_13_1 <- bal_13 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_13_2 <- bal_13 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_13 <- rbind(bal_13_1, bal_13_2)
rm(bal_13_1, bal_13_2)

bal_13 <- bal_13 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_13$Partido)
as.factor(bal_13$UF)
as.factor(bal_13$Posicao)

rm(inicio_antes_fim_antes13, inicio_antes_fim_na13, inicio_antes_fim_durante13,  inicio_antes_fim_depois13, inicio_durante_fim_na13, inicio_durante_fim_durante13, inicio_durante_fim_depois13, posicao13)
write.csv2(bal_13, "bal_13.csv")