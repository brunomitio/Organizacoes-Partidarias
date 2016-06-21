## Tabela dos Partidos
url <- "http://www.tse.jus.br/partidos/partidos-politicos/registrados-no-tse"
library(XML)
library(dplyr)
lista <- readHTMLTable(url)
partidos <- lista[[1]]
partidos <- partidos[-1,c(2:6)]
names(partidos) <- c("SIGLA", "NOME", "DEFERIMENTO", "PRESIDENTE_NACIONAL","N")
partidos <- partidos[!is.na(partidos$SIGLA),]
rm(lista, url)
row.names(partidos) <- NULL
write.csv2(partidos, "tabela partidos.csv")


## Geracao das chaves

#2007
mun2007 <- readxl::read_excel("municipios.xlsx", sheet = "2007")
mun2007 <- mun2007[,c(4,6,7)]
part2007 <- data.frame(partidos[1:27,1])
part2007$partidos.1.27..1. <- toupper(part2007$partidos.1.27..1.)

chave07 <- c()
muni <- c()
for(i in part2007$partidos.1.27..1.) {
  print(i)
  for(j in mun2007$Cod_Ibge){
    a <- c(paste(i, j))
    muni <- rbind(muni,a)}
  chave07 <- rbind(chave07, muni)}
chave07 <- unique(chave07)

rownames(chave07) <- NULL

chave07 <- as.data.frame(chave07)
library(stringr)
chave07 <- chave07 %>% 
  select(chave = V1)
chave07$chave <- as.character(chave07$chave)
chave07 <- chave07 %>% 
  mutate(Cod_Ibge = substrRight(chave, 7))

chave07 <- merge(chave07, mun2007, by = "Cod_Ibge", all = T)
chave07 <- chave07 %>% 
  select(Chave = chave, Cod_Ibge, Municipio, UF)


#2008
chave08 <- chave07

#2009
chave09 <- chave07

#2010
chave10 <- chave07

#2011
part2011 <- data.frame(partidos[1:29,1])
part2011$partidos.1.29..1. <- toupper(part2011$partidos.1.29..1.)

chave11 <- c()
muni <- c()
for(i in part2011$partidos.1.29..1.) {
  print(i)
  for(j in mun2007$Cod_Ibge){
    a <- c(paste(i, j))
    muni <- rbind(muni,a)}
  chave11 <- rbind(chave11, muni)}
chave11 <- unique(chave11)

rownames(chave11) <- NULL

chave11 <- as.data.frame(chave11)
library(lubridate)
library(stringr)
library(dplyr)

chave11$V1 <- as.character(chave11$V1)
chave11 <- merge(chave11, mun2007, by = "Cod_Ibge", all = T)

chave11 <- chave11 %>% 
  mutate(Cod_Ibge = substrRight(V1, 7)) %>% 
  select(Chave = V1, Cod_Ibge, Municipio, UF)

chave11 <- as.data.frame(chave11)
library(stringr)
chave11 <- chave11 %>% 
  select(chave = V1)
chave11$chave <- as.character(chave11$chave)
chave11 <- chave11 %>% 
  mutate(Cod_Ibge = substrRight(chave, 7))

chave11 <- merge(chave11, mun2007, by = "Cod_Ibge", all = T)
chave11 <- chave11 %>% 
  select(Chave = chave, Cod_Ibge, Municipio, UF)

#2012
part2012 <- data.frame(partidos[1:30,1])
part2012$partidos.1.30..1. <- toupper(part2012$partidos.1.30..1.)

chave12 <- c()
muni <- c()
for(i in part2012$partidos.1.30..1.) {
  print(i)
  for(j in mun2007$Cod_Ibge){
    a <- c(paste(i, j))
    muni <- rbind(muni,a)}
  chave12 <- rbind(chave12, muni)}
chave12 <- unique(chave12)

rownames(chave12) <- NULL

chave12 <- as.data.frame(chave12)
library(lubridate)
library(stringr)
library(dplyr)

chave12$V1 <- as.character(chave12$V1)
chave12 <- merge(chave12, mun2007, by = "Cod_Ibge", all = T)

chave12 <- chave12 %>% 
  mutate(Cod_Ibge = substrRight(V1, 7)) %>% 
  select(Chave = V1, Cod_Ibge, Municipio, UF)

chave12 <- as.data.frame(chave12)
library(stringr)
chave12 <- chave12 %>% 
  select(chave = V1)
chave12$chave <- as.character(chave12$chave)
chave12 <- chave12 %>% 
  mutate(Cod_Ibge = substrRight(chave, 7))

chave12 <- merge(chave12, mun2007, by = "Cod_Ibge", all = T)
chave12 <- chave12 %>% 
  select(Chave = chave, Cod_Ibge, Municipio, UF)

#2013
mun2013 <- readxl::read_excel("municipios.xlsx", sheet = "2013")
mun2013 <- mun2013[,c(1,4,5)]
part2013 <- data.frame(partidos[1:32,1])
part2013$partidos.1.32..1. <- toupper(part2013$partidos.1.32..1.)

chave13 <- c()
muni <- c()
for(i in part2013$partidos.1.32..1.) {
  print(i)
  for(j in mun2013$Cod_Ibge){
    a <- c(paste(i, j))
    muni <- rbind(muni,a)}
  chave13 <- rbind(chave13, muni)}
chave13 <- unique(chave13)

rownames(chave13) <- NULL

chave13 <- as.data.frame(chave13)
library(stringr)
chave13 <- chave13 %>% 
  select(chave = V1)
chave13$chave <- as.character(chave13$chave)
chave13 <- chave13 %>% 
  mutate(Cod_Ibge = substrRight(chave, 7))

chave13 <- merge(chave13, mun2013, by = "Cod_Ibge", all = T)
chave13 <- chave13 %>% 
  select(Chave = chave, Cod_Ibge, Municipio, UF)

#2014
part2014 <- data.frame(partidos[1:35,1])
part2014$partidos.1.35..1. <- toupper(part2014$partidos.1.35..1.)

chave14 <- c()
muni <- c()
for(i in part2014$partidos.1.35..1.) {
  print(i)
  for(j in mun2013$Cod_Ibge){
    a <- c(paste(i, j))
    muni <- rbind(muni,a)}
  chave14 <- rbind(chave14, muni)}
chave14 <- unique(chave14)

rownames(chave14) <- NULL

chave14 <- as.data.frame(chave14)
library(stringr)
chave14 <- chave14 %>% 
  select(chave = V1)
chave14$chave <- as.character(chave14$chave)
chave14 <- chave14 %>% 
  mutate(Cod_Ibge = substrRight(chave, 7))

chave14 <- merge(chave14, mun2013, by = "Cod_Ibge", all = T)
chave14 <- chave14 %>% 
  select(Chave = chave, Cod_Ibge, Municipio, UF)

#2015
chave15 <- chave14
