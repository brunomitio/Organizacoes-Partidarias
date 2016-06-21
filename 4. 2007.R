## 2007

library(lubridate)
library(stringr)
library(dplyr)

##CALCULO DOS DIAS
# Antes de 2007
# 1)	Iniciados antes de 2007 e encerrados antes de 2007 
inicio_antes_fim_antes07 <- org3 %>% 
  filter(Fim < dmy("01/01/2007"))

# 2)	Iniciado antes de 2007 e ainda não encerrados 
inicio_antes_fim_na07 <- org3 %>% 
  filter((Inicio < dmy("01/01/2007")) & ((is.na(Fim)))) 
inicio_antes_fim_na07_D <- inicio_antes_fim_na07 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na07_CP <- inicio_antes_fim_na07 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na07_CI <- inicio_antes_fim_na07 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_na07 <- rbind(inicio_antes_fim_na07_D, inicio_antes_fim_na07_CI, inicio_antes_fim_na07_CP)
rm(inicio_antes_fim_na07_CI, inicio_antes_fim_na07_CP, inicio_antes_fim_na07_D)

# 3)	Iniciado antes de 2007 e encerrados depois de 2007 
inicio_antes_fim_depois07 <- org3 %>% 
  filter((Inicio < dmy("01/01/2007")) & ((Fim > dmy("31/12/2007")))) 
inicio_antes_fim_depois07_D <- inicio_antes_fim_depois07 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = 365,
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois07_CP <- inicio_antes_fim_depois07 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = 365,
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois07_CI <- inicio_antes_fim_depois07 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = 365,
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_depois07 <- rbind(inicio_antes_fim_depois07_D, inicio_antes_fim_depois07_CI, inicio_antes_fim_depois07_CP)
rm(inicio_antes_fim_depois07_CI, inicio_antes_fim_depois07_CP, inicio_antes_fim_depois07_D)

# 4)	Iniciados antes de 2007 e encerrados em 2007
inicio_antes_fim_durante07 <- org3 %>% 
  filter(Inicio < dmy("01/01/2007")) %>%
  filter((Fim <= dmy("31/12/2007") & (Fim >= dmy("01/01/2007"))))
inicio_antes_fim_durante07_D <- inicio_antes_fim_durante07 %>%
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, dmy("01/01/2007"), units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante07_CP <- inicio_antes_fim_durante07 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, dmy("01/01/2007"), units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante07_CI <- inicio_antes_fim_durante07 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, dmy("01/01/2007"), units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_antes_fim_durante07 <- rbind(inicio_antes_fim_durante07_D, inicio_antes_fim_durante07_CI, inicio_antes_fim_durante07_CP)
rm(inicio_antes_fim_durante07_CI, inicio_antes_fim_durante07_CP, inicio_antes_fim_durante07_D)

# Durante 2007
# 5)	Iniciado em 2007 e encerrado em 2007
inicio_durante_fim_durante07 <- org3 %>%
  filter((Inicio <= dmy("31/12/2007") & (Inicio >= dmy("01/01/2007")))) %>%
  filter(Fim <= dmy("31/12/2007"))
inicio_durante_fim_durante07_D <- inicio_durante_fim_durante07 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(Fim, Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante07_CP <- inicio_durante_fim_durante07 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante07_CI <- inicio_durante_fim_durante07 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(Fim, Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_durante07 <- rbind(inicio_durante_fim_durante07_D, inicio_durante_fim_durante07_CP, inicio_durante_fim_durante07_CI)
rm(inicio_durante_fim_durante07_CI, inicio_durante_fim_durante07_D, inicio_durante_fim_durante07_CP)

# 6)	Iniciado em 2007 e não encerrado
inicio_durante_fim_na07 <- org3 %>%
  filter((Inicio <= dmy("31/12/2007") & (Inicio >= dmy("01/01/2007")))) %>%
  filter((is.na(Fim)))
inicio_durante_fim_na07_D <- inicio_durante_fim_na07 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2007"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na07_CP <- inicio_durante_fim_na07 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2007"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na07_CI <- inicio_durante_fim_na07 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2007"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_na07 <- rbind(inicio_durante_fim_na07_D, inicio_durante_fim_na07_CP, inicio_durante_fim_na07_CI)
rm(inicio_durante_fim_na07_CI, inicio_durante_fim_na07_D, inicio_durante_fim_na07_CP)

# 7)	Iniciado em 2007 e encerrado depois de 2007
inicio_durante_fim_depois07 <- org3 %>%
  filter((Inicio <= dmy("31/12/2007") & (Inicio >= dmy("01/01/2007")))) %>%
  filter((Fim > dmy("31/12/2007")))
inicio_durante_fim_depois07_D <- inicio_durante_fim_depois07 %>% 
  filter(Tipo == "DIRETORIO") %>% 
  mutate(dias_D_ano = difftime(dmy("31/12/2007"), Inicio, units = "days"),
         dias_CP_ano = 0,
         dias_CI_ano = 0) %>% 
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois07_CP <- inicio_durante_fim_depois07 %>% 
  filter(Tipo == "COMISSAO PROVISORIA") %>% 
  mutate(dias_CP_ano = difftime(dmy("31/12/2007"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CI_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois07_CI <- inicio_durante_fim_depois07 %>% 
  filter(Tipo == "COMISSAO INTERVENTORA") %>% 
  mutate(dias_CI_ano = difftime(dmy("31/12/2007"), Inicio, units = "days"),
         dias_D_ano = 0,
         dias_CP_ano = 0) %>%
  select(Chave, Tipo, dias_D_ano, dias_CP_ano, dias_CI_ano)
inicio_durante_fim_depois07 <- rbind(inicio_durante_fim_depois07_D, inicio_durante_fim_depois07_CP, inicio_durante_fim_depois07_CI)
rm(inicio_durante_fim_depois07_CI, inicio_durante_fim_depois07_D, inicio_durante_fim_depois07_CP)

#Juntando o somatorio de dias
dias07 <- rbind(inicio_antes_fim_depois07, inicio_antes_fim_na07, inicio_antes_fim_durante07, inicio_durante_fim_durante07, inicio_durante_fim_depois07, inicio_durante_fim_na07)
dias07 <- dias07 %>%
  mutate(chave2 = Chave) %>% 
  group_by(Chave, chave2) %>% 
  summarise(dias_D_ano2 = sum(dias_D_ano),
            dias_CP_ano2 = sum(dias_CP_ano),
            dias_CI_ano2 = sum(dias_CI_ano)) %>% 
  select(dias_D_ano = dias_D_ano2, dias_CP_ano = dias_CP_ano2, dias_CI_ano = dias_CI_ano2)

## POSICAO NO ULTIMO DIA
posicao07 <- org3 %>% 
  filter((Fim >= dmy("31/12/2007") | (is.na(Fim)))) %>% 
  filter(Inicio <= dmy("31/12/2007"))
posicao07 <- posicao07 %>% 
  mutate(Posicao = Tipo) %>% 
  select(Chave, Posicao)
posicao07 <- unique(posicao07)

posicao07 <- merge(posicao07, chave07,  all.x = T, all.y = T)
posicao07_2 <- posicao07 %>% 
  filter(is.na(Posicao)) %>% 
  mutate(Posicao = "Nenhum")
posicao07_3 <- merge(posicao07, chave07,  all.x = T, all.y = T) %>% 
  filter(!is.na(Posicao))
posicao07 <- rbind(posicao07_2, posicao07_3)  
rm(posicao07_2, posicao07_3)

# Gerando Balanco de 2007
bal_07 <- merge(chave07, dias07, "Chave", all.x = T, all.y = T)

bal_07 <- merge(bal_07, posicao07, "Chave", all.x = T, all.y = T)
bal_07_1 <- bal_07 %>% 
  filter(is.na(dias_D_ano) & is.na(dias_CP_ano) & is.na(dias_CI_ano)) %>% 
  mutate(dias_D_ano = 0,
         dias_CP_ano = 0,
         dias_CI_ano = 0,
         dias_N_ano = 365) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_07_2 <- bal_07 %>% 
  filter(!is.na(dias_D_ano) | !is.na(dias_CP_ano) | !is.na(dias_CI_ano)) %>% 
  mutate(dias_N_ano = 365 - dias_D_ano - dias_CP_ano - dias_CI_ano) %>% 
  select(Chave:dias_CI_ano, dias_N_ano, Posicao)
bal_07 <- rbind(bal_07_1, bal_07_2)
rm(bal_07_1, bal_07_2)

bal_07 <- bal_07 %>% 
  select(Chave, Municipio = Municipio.x, Cod_Ibge = Cod_Ibge.x, UF = UF.x, dias_D_ano:Posicao) %>% 
  mutate(Partido = substr(Chave, 1, nchar(Chave)-7)) %>% 
  select(Chave, Partido, Cod_Ibge:Posicao)

as.factor(bal_07$Partido)
as.factor(bal_07$UF)
as.factor(bal_07$Posicao)

rm(inicio_antes_fim_antes07, inicio_antes_fim_na07,inicio_antes_fim_durante07,  inicio_antes_fim_depois07, inicio_durante_fim_na07, inicio_durante_fim_durante07, inicio_durante_fim_depois07, posicao07)
write.csv2(bal_07, "bal_07.csv")