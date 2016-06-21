## 2011

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2011
# 1)	Iniciados antes de 2011 e encerrados antes de 2011 
inicio_antes_fim_antes11 <- org3 %>% 
  filter(Fim < dmy("01/01/2011"))

# 2)	Iniciado antes de 2011 e ainda não encerrados 
inicio_antes_fim_na11 <- org3 %>% 
  filter((Inicio < dmy("01/01/2011")) & ((is.na(Fim)))) 
inicio_antes_fim_na11_D <- inicio_antes_fim_na11 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na11_CP <- inicio_antes_fim_na11 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na11_CI <- inicio_antes_fim_na11 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na11 <- rbind(inicio_antes_fim_na11_D, inicio_antes_fim_na11_CI, inicio_antes_fim_na11_CP)
rm(inicio_antes_fim_na11_CI, inicio_antes_fim_na11_CP, inicio_antes_fim_na11_D)

# 3)	Iniciado antes de 2011 e encerrados depois de 2011 
inicio_antes_fim_depois11 <- org3 %>% 
  filter((Inicio < dmy("01/01/2011")) & ((Fim > dmy("31/12/2011")))) 
inicio_antes_fim_depois11_D <- inicio_antes_fim_depois11 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois11_CP <- inicio_antes_fim_depois11 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois11_CI <- inicio_antes_fim_depois11 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois11 <- rbind(inicio_antes_fim_depois11_D, inicio_antes_fim_depois11_CI, inicio_antes_fim_depois11_CP)
rm(inicio_antes_fim_depois11_CI, inicio_antes_fim_depois11_CP, inicio_antes_fim_depois11_D)

# 4)	Iniciados antes de 2011 e encerrados em 2011
inicio_antes_fim_durante11 <- org3 %>% 
  filter(Inicio < dmy("01/01/2011")) %>%
  filter((Fim <= dmy("31/12/2011") & (Fim >= dmy("01/01/2011"))))
inicio_antes_fim_durante11_D <- inicio_antes_fim_durante11 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2011"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante11_CP <- inicio_antes_fim_durante11 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2011"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante11_CI <- inicio_antes_fim_durante11 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2011"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante11 <- rbind(inicio_antes_fim_durante11_D, inicio_antes_fim_durante11_CI, inicio_antes_fim_durante11_CP)
rm(inicio_antes_fim_durante11_CI, inicio_antes_fim_durante11_CP, inicio_antes_fim_durante11_D)

# Durante 2011
# 5)	Iniciado em 2011 e encerrado em 2011
inicio_durante_fim_durante11 <- org3 %>%
  filter((Inicio <= dmy("31/12/2011") & (Inicio >= dmy("01/01/2011")))) %>%
  filter(Fim <= dmy("31/12/2011"))
inicio_durante_fim_durante11_D <- inicio_durante_fim_durante11 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante11_CP <- inicio_durante_fim_durante11 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante11_CI <- inicio_durante_fim_durante11 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante11 <- rbind(inicio_durante_fim_durante11_D, inicio_durante_fim_durante11_CP, inicio_durante_fim_durante11_CI)
rm(inicio_durante_fim_durante11_CI, inicio_durante_fim_durante11_D, inicio_durante_fim_durante11_CP)

# 6)	Iniciado em 2011 e não encerrado
inicio_durante_fim_na11 <- org3 %>%
  filter((Inicio <= dmy("31/12/2011") & (Inicio >= dmy("01/01/2011")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na11_D <- inicio_durante_fim_na11 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2011"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na11_CP <- inicio_durante_fim_na11 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2011"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na11_CI <- inicio_durante_fim_na11 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2011"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na11 <- rbind(inicio_durante_fim_na11_D, inicio_durante_fim_na11_CP, inicio_durante_fim_na11_CI)
rm(inicio_durante_fim_na11_CI, inicio_durante_fim_na11_D, inicio_durante_fim_na11_CP)

# 7)	Iniciado em 2011 e encerrado depois de 2011
inicio_durante_fim_depois11 <- org3 %>%
  filter((Inicio <= dmy("31/12/2011") & (Inicio >= dmy("01/01/2011")))) %>%
  filter((Fim > dmy("31/12/2011")))
inicio_durante_fim_depois11_D <- inicio_durante_fim_depois11 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2011"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois11_CP <- inicio_durante_fim_depois11 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2011"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois11_CI <- inicio_durante_fim_depois11 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2011"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois11 <- rbind(inicio_durante_fim_depois11_D, inicio_durante_fim_depois11_CP, inicio_durante_fim_depois11_CI)
rm(inicio_durante_fim_depois11_CI, inicio_durante_fim_depois11_D, inicio_durante_fim_depois11_CP)

#Juntando o somatorio de dias
dias11 <- rbind(inicio_antes_fim_depois11, inicio_antes_fim_na11, inicio_antes_fim_durante11, inicio_durante_fim_durante11, inicio_durante_fim_depois11, inicio_durante_fim_na11)
dias11 <- dias11 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao11 <- org3 %>% 
  filter((Fim >= dmy("31/12/2011") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2011"))
posicao11 <- posicao11 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao11 <- unique(posicao11)

posicao11 <- merge(posicao11, chave11,  all.x = T, all.y = T)
posicao11_2 <- posicao11 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao11_3 <- merge(posicao11, chave11,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao11 <- rbind(posicao11_2, posicao11_3)  
rm(posicao11_2, posicao11_3)

# Gerando Balanco de 2011
bal_11 <- merge(chave11, dias11, "Chave", all.x = T, all.y = T)

bal_11 <- merge(bal_11, posicao11, "Chave", all.x = T, all.y = T)
bal_11_1 <- bal_11 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_11_2 <- bal_11 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_11 <- rbind(bal_11_1, bal_11_2)
rm(bal_11_1, bal_11_2)

bal_11 <- bal_11 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_11$Partido)
as.factor(bal_11$UF)
as.factor(bal_11$Posicao)

rm(inicio_antes_fim_antes11, inicio_antes_fim_na11, inicio_antes_fim_durante11, inicio_antes_fim_depois11, inicio_durante_fim_na11, inicio_durante_fim_durante11, inicio_durante_fim_depois11, posicao11)
write.csv2(bal_11, "bal_11.csv")
