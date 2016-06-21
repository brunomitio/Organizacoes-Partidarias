## 2009

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2009
# 1)	Iniciados antes de 2009 e encerrados antes de 2009 
inicio_antes_fim_antes09 <- org3 %>% 
  filter(Fim < dmy("01/01/2009"))

# 2)	Iniciado antes de 2009 e ainda não encerrados 
inicio_antes_fim_na09 <- org3 %>% 
  filter((Inicio < dmy("01/01/2009")) & ((is.na(Fim)))) 
inicio_antes_fim_na09_D <- inicio_antes_fim_na09 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na09_CP <- inicio_antes_fim_na09 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na09_CI <- inicio_antes_fim_na09 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na09 <- rbind(inicio_antes_fim_na09_D, inicio_antes_fim_na09_CI, inicio_antes_fim_na09_CP)
rm(inicio_antes_fim_na09_CI, inicio_antes_fim_na09_CP, inicio_antes_fim_na09_D)

# 3)	Iniciado antes de 2009 e encerrados depois de 2009 
inicio_antes_fim_depois09 <- org3 %>% 
  filter((Inicio < dmy("01/01/2009")) & ((Fim > dmy("31/12/2009")))) 
inicio_antes_fim_depois09_D <- inicio_antes_fim_depois09 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois09_CP <- inicio_antes_fim_depois09 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois09_CI <- inicio_antes_fim_depois09 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois09 <- rbind(inicio_antes_fim_depois09_D, inicio_antes_fim_depois09_CI, inicio_antes_fim_depois09_CP)
rm(inicio_antes_fim_depois09_CI, inicio_antes_fim_depois09_CP, inicio_antes_fim_depois09_D)

# 4)	Iniciados antes de 2009 e encerrados em 2009
inicio_antes_fim_durante09 <- org3 %>% 
  filter(Inicio < dmy("01/01/2009")) %>%
  filter((Fim <= dmy("31/12/2009") & (Fim >= dmy("01/01/2009"))))
inicio_antes_fim_durante09_D <- inicio_antes_fim_durante09 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2009"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante09_CP <- inicio_antes_fim_durante09 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2009"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante09_CI <- inicio_antes_fim_durante09 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2009"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante09 <- rbind(inicio_antes_fim_durante09_D, inicio_antes_fim_durante09_CI, inicio_antes_fim_durante09_CP)
rm(inicio_antes_fim_durante09_CI, inicio_antes_fim_durante09_CP, inicio_antes_fim_durante09_D)

# Durante 2009
# 5)	Iniciado em 2009 e encerrado em 2009
inicio_durante_fim_durante09 <- org3 %>%
  filter((Inicio <= dmy("31/12/2009") & (Inicio >= dmy("01/01/2009")))) %>%
  filter(Fim <= dmy("31/12/2009"))
inicio_durante_fim_durante09_D <- inicio_durante_fim_durante09 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante09_CP <- inicio_durante_fim_durante09 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante09_CI <- inicio_durante_fim_durante09 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante09 <- rbind(inicio_durante_fim_durante09_D, inicio_durante_fim_durante09_CP, inicio_durante_fim_durante09_CI)
rm(inicio_durante_fim_durante09_CI, inicio_durante_fim_durante09_D, inicio_durante_fim_durante09_CP)

# 6)	Iniciado em 2009 e não encerrado
inicio_durante_fim_na09 <- org3 %>%
  filter((Inicio <= dmy("31/12/2009") & (Inicio >= dmy("01/01/2009")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na09_D <- inicio_durante_fim_na09 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2009"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na09_CP <- inicio_durante_fim_na09 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2009"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na09_CI <- inicio_durante_fim_na09 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2009"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na09 <- rbind(inicio_durante_fim_na09_D, inicio_durante_fim_na09_CP, inicio_durante_fim_na09_CI)
rm(inicio_durante_fim_na09_CI, inicio_durante_fim_na09_D, inicio_durante_fim_na09_CP)

# 7)	Iniciado em 2009 e encerrado depois de 2009
inicio_durante_fim_depois09 <- org3 %>%
  filter((Inicio <= dmy("31/12/2009") & (Inicio >= dmy("01/01/2009")))) %>%
  filter((Fim > dmy("31/12/2009")))
inicio_durante_fim_depois09_D <- inicio_durante_fim_depois09 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2009"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois09_CP <- inicio_durante_fim_depois09 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2009"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois09_CI <- inicio_durante_fim_depois09 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2009"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois09 <- rbind(inicio_durante_fim_depois09_D, inicio_durante_fim_depois09_CP, inicio_durante_fim_depois09_CI)
rm(inicio_durante_fim_depois09_CI, inicio_durante_fim_depois09_D, inicio_durante_fim_depois09_CP)

#Juntando o somatorio de dias
dias09 <- rbind(inicio_antes_fim_depois09, inicio_antes_fim_na09, inicio_antes_fim_durante09, inicio_durante_fim_durante09, inicio_durante_fim_depois09, inicio_durante_fim_na09)
dias09 <- dias09 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao09 <- org3 %>% 
  filter((Fim >= dmy("31/12/2009") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2009"))
posicao09 <- posicao09 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao09 <- unique(posicao09)

posicao09 <- merge(posicao09, chave09,  all.x = T, all.y = T)
posicao09_2 <- posicao09 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao09_3 <- merge(posicao09, chave09,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao09 <- rbind(posicao09_2, posicao09_3)  
rm(posicao09_2, posicao09_3)

# Gerando Balanco de 2009
bal_09 <- merge(chave09, dias09, "Chave", all.x = T, all.y = T)

bal_09 <- merge(bal_09, posicao09, "Chave", all.x = T, all.y = T)
bal_09_1 <- bal_09 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_09_2 <- bal_09 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_09 <- rbind(bal_09_1, bal_09_2)
rm(bal_09_1, bal_09_2)

bal_09 <- bal_09 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_09$Partido)
as.factor(bal_09$UF)
as.factor(bal_09$Posicao)

rm(inicio_antes_fim_antes09, inicio_antes_fim_na09, inicio_antes_fim_durante09,  inicio_antes_fim_depois09, inicio_durante_fim_na09, inicio_durante_fim_durante09, inicio_durante_fim_depois09, posicao09)
write.csv2(bal_09, "bal_09.csv")