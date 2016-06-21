## 2014

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2014
# 1)	Iniciados antes de 2014 e encerrados antes de 2014 
inicio_antes_fim_antes14 <- org3 %>% 
  filter(Fim < dmy("01/01/2014"))

# 2)	Iniciado antes de 2014 e ainda não encerrados 
inicio_antes_fim_na14 <- org3 %>% 
  filter((Inicio < dmy("01/01/2014")) & ((is.na(Fim)))) 
inicio_antes_fim_na14_D <- inicio_antes_fim_na14 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na14_CP <- inicio_antes_fim_na14 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na14_CI <- inicio_antes_fim_na14 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na14 <- rbind(inicio_antes_fim_na14_D, inicio_antes_fim_na14_CI, inicio_antes_fim_na14_CP)
rm(inicio_antes_fim_na14_CI, inicio_antes_fim_na14_CP, inicio_antes_fim_na14_D)

# 3)	Iniciado antes de 2014 e encerrados depois de 2014 
inicio_antes_fim_depois14 <- org3 %>% 
  filter((Inicio < dmy("01/01/2014")) & ((Fim > dmy("31/12/2014")))) 
inicio_antes_fim_depois14_D <- inicio_antes_fim_depois14 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois14_CP <- inicio_antes_fim_depois14 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois14_CI <- inicio_antes_fim_depois14 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois14 <- rbind(inicio_antes_fim_depois14_D, inicio_antes_fim_depois14_CI, inicio_antes_fim_depois14_CP)
rm(inicio_antes_fim_depois14_CI, inicio_antes_fim_depois14_CP, inicio_antes_fim_depois14_D)

# 4)	Iniciados antes de 2014 e encerrados em 2014
inicio_antes_fim_durante14 <- org3 %>% 
  filter(Inicio < dmy("01/01/2014")) %>%
  filter((Fim <= dmy("31/12/2014") & (Fim >= dmy("01/01/2014"))))
inicio_antes_fim_durante14_D <- inicio_antes_fim_durante14 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2014"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante14_CP <- inicio_antes_fim_durante14 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2014"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante14_CI <- inicio_antes_fim_durante14 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2014"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante14 <- rbind(inicio_antes_fim_durante14_D, inicio_antes_fim_durante14_CI, inicio_antes_fim_durante14_CP)
rm(inicio_antes_fim_durante14_CI, inicio_antes_fim_durante14_CP, inicio_antes_fim_durante14_D)

# Durante 2014
# 5)	Iniciado em 2014 e encerrado em 2014
inicio_durante_fim_durante14 <- org3 %>%
  filter((Inicio <= dmy("31/12/2014") & (Inicio >= dmy("01/01/2014")))) %>%
  filter(Fim <= dmy("31/12/2014"))
inicio_durante_fim_durante14_D <- inicio_durante_fim_durante14 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante14_CP <- inicio_durante_fim_durante14 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante14_CI <- inicio_durante_fim_durante14 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante14 <- rbind(inicio_durante_fim_durante14_D, inicio_durante_fim_durante14_CP, inicio_durante_fim_durante14_CI)
rm(inicio_durante_fim_durante14_CI, inicio_durante_fim_durante14_D, inicio_durante_fim_durante14_CP)

# 6)	Iniciado em 2014 e não encerrado
inicio_durante_fim_na14 <- org3 %>%
  filter((Inicio <= dmy("31/12/2014") & (Inicio >= dmy("01/01/2014")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na14_D <- inicio_durante_fim_na14 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2014"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na14_CP <- inicio_durante_fim_na14 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2014"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na14_CI <- inicio_durante_fim_na14 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2014"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na14 <- rbind(inicio_durante_fim_na14_D, inicio_durante_fim_na14_CP, inicio_durante_fim_na14_CI)
rm(inicio_durante_fim_na14_CI, inicio_durante_fim_na14_D, inicio_durante_fim_na14_CP)

# 7)	Iniciado em 2014 e encerrado depois de 2014
inicio_durante_fim_depois14 <- org3 %>%
  filter((Inicio <= dmy("31/12/2014") & (Inicio >= dmy("01/01/2014")))) %>%
  filter((Fim > dmy("31/12/2014")))
inicio_durante_fim_depois14_D <- inicio_durante_fim_depois14 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2014"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois14_CP <- inicio_durante_fim_depois14 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2014"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois14_CI <- inicio_durante_fim_depois14 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2014"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois14 <- rbind(inicio_durante_fim_depois14_D, inicio_durante_fim_depois14_CP, inicio_durante_fim_depois14_CI)
rm(inicio_durante_fim_depois14_CI, inicio_durante_fim_depois14_D, inicio_durante_fim_depois14_CP)

#Juntando o somatorio de dias
dias14 <- rbind(inicio_antes_fim_depois14, inicio_antes_fim_na14, inicio_antes_fim_durante14, inicio_durante_fim_durante14, inicio_durante_fim_depois14, inicio_durante_fim_na14)
dias14 <- dias14 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao14 <- org3 %>% 
  filter((Fim >= dmy("31/12/2014") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2014"))
posicao14 <- posicao14 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao14 <- unique(posicao14)

posicao14 <- merge(posicao14, chave14,  all.x = T, all.y = T)
posicao14_2 <- posicao14 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao14_3 <- merge(posicao14, chave14,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao14 <- rbind(posicao14_2, posicao14_3)  
rm(posicao14_2, posicao14_3)

# Gerando Balanco de 2014
bal_14 <- merge(chave14, dias14, "Chave", all.x = T, all.y = T)

bal_14 <- merge(bal_14, posicao14, "Chave", all.x = T, all.y = T)
bal_14_1 <- bal_14 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_14_2 <- bal_14 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_14 <- rbind(bal_14_1, bal_14_2)
rm(bal_14_1, bal_14_2)

bal_14 <- bal_14 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_14$Partido)
as.factor(bal_14$UF)
as.factor(bal_14$Posicao)

rm(inicio_antes_fim_antes14, inicio_antes_fim_na14, inicio_antes_fim_durante14, inicio_antes_fim_depois14, inicio_durante_fim_na14, inicio_durante_fim_durante14, inicio_durante_fim_depois14, posicao14)
write.csv2(bal_14, "bal_14.csv")