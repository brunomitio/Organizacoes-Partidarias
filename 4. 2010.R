## 2010

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2010
# 1)	Iniciados antes de 2010 e encerrados antes de 2010 
inicio_antes_fim_antes10 <- org3 %>% 
  filter(Fim < dmy("01/01/2010"))

# 2)	Iniciado antes de 2010 e ainda não encerrados 
inicio_antes_fim_na10 <- org3 %>% 
  filter((Inicio < dmy("01/01/2010")) & ((is.na(Fim)))) 
inicio_antes_fim_na10_D <- inicio_antes_fim_na10 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na10_CP <- inicio_antes_fim_na10 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na10_CI <- inicio_antes_fim_na10 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na10 <- rbind(inicio_antes_fim_na10_D, inicio_antes_fim_na10_CI, inicio_antes_fim_na10_CP)
rm(inicio_antes_fim_na10_CI, inicio_antes_fim_na10_CP, inicio_antes_fim_na10_D)

# 3)	Iniciado antes de 2010 e encerrados depois de 2010 
inicio_antes_fim_depois10 <- org3 %>% 
  filter((Inicio < dmy("01/01/2010")) & ((Fim > dmy("31/12/2010")))) 
inicio_antes_fim_depois10_D <- inicio_antes_fim_depois10 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois10_CP <- inicio_antes_fim_depois10 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois10_CI <- inicio_antes_fim_depois10 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois10 <- rbind(inicio_antes_fim_depois10_D, inicio_antes_fim_depois10_CI, inicio_antes_fim_depois10_CP)
rm(inicio_antes_fim_depois10_CI, inicio_antes_fim_depois10_CP, inicio_antes_fim_depois10_D)

# 4)	Iniciados antes de 2010 e encerrados em 2010
inicio_antes_fim_durante10 <- org3 %>% 
  filter(Inicio < dmy("01/01/2010")) %>%
  filter((Fim <= dmy("31/12/2010") & (Fim >= dmy("01/01/2010"))))
inicio_antes_fim_durante10_D <- inicio_antes_fim_durante10 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2010"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante10_CP <- inicio_antes_fim_durante10 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2010"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante10_CI <- inicio_antes_fim_durante10 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2010"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante10 <- rbind(inicio_antes_fim_durante10_D, inicio_antes_fim_durante10_CI, inicio_antes_fim_durante10_CP)
rm(inicio_antes_fim_durante10_CI, inicio_antes_fim_durante10_CP, inicio_antes_fim_durante10_D)

# Durante 2010
# 5)	Iniciado em 2010 e encerrado em 2010
inicio_durante_fim_durante10 <- org3 %>%
  filter((Inicio <= dmy("31/12/2010") & (Inicio >= dmy("01/01/2010")))) %>%
  filter(Fim <= dmy("31/12/2010"))
inicio_durante_fim_durante10_D <- inicio_durante_fim_durante10 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante10_CP <- inicio_durante_fim_durante10 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante10_CI <- inicio_durante_fim_durante10 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante10 <- rbind(inicio_durante_fim_durante10_D, inicio_durante_fim_durante10_CP, inicio_durante_fim_durante10_CI)
rm(inicio_durante_fim_durante10_CI, inicio_durante_fim_durante10_D, inicio_durante_fim_durante10_CP)

# 6)	Iniciado em 2010 e não encerrado
inicio_durante_fim_na10 <- org3 %>%
  filter((Inicio <= dmy("31/12/2010") & (Inicio >= dmy("01/01/2010")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na10_D <- inicio_durante_fim_na10 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2010"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na10_CP <- inicio_durante_fim_na10 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2010"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na10_CI <- inicio_durante_fim_na10 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2010"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na10 <- rbind(inicio_durante_fim_na10_D, inicio_durante_fim_na10_CP, inicio_durante_fim_na10_CI)
rm(inicio_durante_fim_na10_CI, inicio_durante_fim_na10_D, inicio_durante_fim_na10_CP)

# 7)	Iniciado em 2010 e encerrado depois de 2010
inicio_durante_fim_depois10 <- org3 %>%
  filter((Inicio <= dmy("31/12/2010") & (Inicio >= dmy("01/01/2010")))) %>%
  filter((Fim > dmy("31/12/2010")))
inicio_durante_fim_depois10_D <- inicio_durante_fim_depois10 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2010"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois10_CP <- inicio_durante_fim_depois10 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2010"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois10_CI <- inicio_durante_fim_depois10 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2010"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois10 <- rbind(inicio_durante_fim_depois10_D, inicio_durante_fim_depois10_CP, inicio_durante_fim_depois10_CI)
rm(inicio_durante_fim_depois10_CI, inicio_durante_fim_depois10_D, inicio_durante_fim_depois10_CP)

#Juntando o somatorio de dias
dias10 <- rbind(inicio_antes_fim_depois10, inicio_antes_fim_na10, inicio_antes_fim_durante10, inicio_durante_fim_durante10, inicio_durante_fim_depois10, inicio_durante_fim_na10)
dias10 <- dias10 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao10 <- org3 %>% 
  filter((Fim >= dmy("31/12/2010") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2010"))
posicao10 <- posicao10 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao10 <- unique(posicao10)

posicao10 <- merge(posicao10, chave10,  all.x = T, all.y = T)
posicao10_2 <- posicao10 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao10_3 <- merge(posicao10, chave10,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao10 <- rbind(posicao10_2, posicao10_3)  
rm(posicao10_2, posicao10_3)

# Gerando Balanco de 2010
bal_10 <- merge(chave10, dias10, "Chave", all.x = T, all.y = T)

bal_10 <- merge(bal_10, posicao10, "Chave", all.x = T, all.y = T)
bal_10_1 <- bal_10 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_10_2 <- bal_10 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_10 <- rbind(bal_10_1, bal_10_2)
rm(bal_10_1, bal_10_2)

bal_10 <- bal_10 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_10$Partido)
as.factor(bal_10$UF)
as.factor(bal_10$Posicao)

rm(inicio_antes_fim_antes10, inicio_antes_fim_na10, inicio_antes_fim_durante10, inicio_antes_fim_depois10, inicio_durante_fim_na10, inicio_durante_fim_durante10, inicio_durante_fim_depois10, posicao10)
write.csv2(bal_10, "bal_10.csv")