# 1. Explorando o banco do TSE e extraindo o agrupamento por municipios
library(dplyr)
org1 <- read.csv(file = 'ORGAOSPART_1.csv', header = T, sep = ";", dec = ".")
org2 <- read.csv(file = 'ORGAOSPART_2.csv', header = T, sep = ";", dec = ".")
org2 <- org2 %>% 
  select(-X)
org3 <- read.csv(file = 'ORGAOSPART_3.csv', header = T, sep = ";", dec = ".")
org4 <- read.csv(file = 'ORGAOSPART_4.csv', header = T, sep = ";", dec = ".")

ls(org1)==ls(org2)
ls(org2)==ls(org3)
ls(org3)==ls(org4)
ls(org1)==ls(org2)

orgTot <- rbind(org1,org2,org3,org4)

orgEstado <- orgTot %>% 
  filter(op.ABRANGENCIA =="ESTADUAL")
orgRegional <- orgTot %>% 
  filter(op.ABRANGENCIA =="REGIONAL")
orgMunicipal <- orgTot %>% 
  filter(op.ABRANGENCIA =="MUNICIPAL")
orgERRO <- orgTot %>% 
  filter(op.ABRANGENCIA != "ESTADUAL") %>% 
  filter(op.ABRANGENCIA != "NACIONAL") %>%
  filter(op.ABRANGENCIA != "REGIONAL") %>% 
  filter(op.ABRANGENCIA != "MUNICIPAL")

orgMunicipal$SG_PARTIDO <- toupper(gsub(" ", "", orgMunicipal$SG_PARTIDO, fixed = TRUE))
orgMunicipal$SG_PARTIDO <- factor(orgMunicipal$SG_PARTIDO) 

orgResumo <- orgMunicipal %>%
  group_by(localidade,SG_PARTIDO,TIPO, DT_INI_VIG_ORGAO, DT_FIM_VIG_ORGAO, sg_ue_sup, dlo.end_orgao, nr_cep) %>% 
  summarise(membros = length(no_membro))

orgResumo$localidade2 <- rm_accent(orgResumo$localidade)
orgResumo$TIPO <- toupper(orgResumo$TIPO)
orgResumo$TIPO <- rm_accent(orgResumo$TIPO)

orgResumo <- orgResumo %>% 
  select(Partido = SG_PARTIDO, Tipo = TIPO, municipio = localidade2, UF = sg_ue_sup,  Cep = nr_cep, Endereco = dlo.end_orgao, membros, Inicio = DT_INI_VIG_ORGAO, Fim = DT_FIM_VIG_ORGAO)
orgResumo <- orgResumo[,c(2:10)]
orgResumo$Tipo <- factor(orgResumo$Tipo)
orgResumo$municipio <- toupper(orgResumo$municipio)
orgResumo$municipio <- gsub("'O", "O ", orgResumo$municipio)
orgResumo$municipio <- gsub("'A", "A ", orgResumo$municipio)
orgResumo$municipio_uf <- paste(orgResumo$municipio,orgResumo$UF)
orgResumo$municipio_uf <- toupper(orgResumo$municipio_uf)

rm(org1, org2, org3, org4, orgTot, orgEstado, orgRegional, orgMunicipal, orgERRO)

summary(orgResumo)

write.csv2(orgResumo, "orgResumo.csv")

# 2. Codigo IBGE
library(dplyr)

ibge <- readxl::read_excel("DTB_2014_Municipio.xls")
ibge <- ibge[, c(2,8,9)]
names(ibge) <- c("uf", "codigo", "nome")

ibge$uf <- factor(ibge$uf, 
                   levels=c("Acre","Alagoas","Amazonas","Amapá",
                            "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                            "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais",
                            "Pará","Paraíba","Paraná","Pernambuco","Piauí","Rio de Janeiro",
                            "Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima",
                                     "Santa Catarina","São Paulo","Sergipe","Tocantins"), 
                   labels=c("AC", "AL", "AM", "AP", 
                            "BA", "CE", "DF", "ES", "GO", 
                            "MA","MT","MS","MG",
                            "PA", "PB", "PR","PE", "PI",  "RJ", 
                            "RN", "RS","RO","RR",
                            "SC", "SP","SE", "TO"))
ibge$nome <- rm_accent(ibge$nome)
ibge$nome <- gsub("D'O", "DO O", ibge$nome)
ibge$nome <- gsub("'A", "A A", ibge$nome)
ibge$nome <- toupper(ibge$nome)
ibge$municipio_uf <- paste(ibge$nome, ibge$uf)

#3 Mactching Cod Ibge

orgResumoIBGE <- merge(orgResumo, ibge, "municipio_uf", all = T)

library(dplyr)
orgResumoIBGE_sem_cod <- orgResumoIBGE %>% 
  filter(is.na(codigo))
orgResumoIBGE_corretos <- orgResumoIBGE %>% 
  filter(!is.na(codigo))

write.csv2(orgResumoIBGE_sem_cod, "ajustar.csv")

# A partir do csv gerado, procurei os nomes que não batem e gerei o csv 'ibge_ajustado' no excel:
ibge_ajustado <- read.csv(file = 'ibge_ajustado.csv', header = T, sep = ";", dec = ".")

ibge_ajustado1 <- ibge_ajustado %>% 
  filter(municipio_uf_ajustado == '#N/D') %>% 
  select(uf, codigo, nome, municipio_uf)

ibge_ajustado2 <- ibge_ajustado %>% 
  filter(municipio_uf_ajustado != '#N/D') %>% 
  select(uf, codigo = codigo.1, nome = municipio_ajustado, municipio_uf = municipio_uf_ajustado)

ibge_ajustado <- rbind(ibge_ajustado1, ibge_ajustado2)

org <- merge(orgResumo, ibge_ajustado, "municipio_uf", all = T)
org <- org %>% 
  filter(municipio_uf != "BRASILIA DF") %>% 
  filter(municipio_uf != "FERNANDO DE NORONHA PE") %>% 
  select(Partido, Tipo, UF, Municipio = nome, Cod_Ibge = codigo, Cep:Fim)

rm(ibge, ibge_ajustado1, ibge_ajustado2, orgResumo, orgResumoIBGE, orgResumoIBGE_sem_cod, orgResumoIBGE_corretos)

# 4 arrumando as datas

library(lubridate)
library(dplyr)
org$Inicio <- dmy(org$Inicio)
org$Fim <- dmy(org$Fim)

# Retirando os casos onde a data Fim e anterior a data Inicio
org2 <- org %>% 
  mutate(Duracao_dias = difftime(Fim, Inicio, units = "days")) %>% 
  filter((is.na(Duracao_dias)) | (Duracao_dias >= 0)) %>% 
  mutate(Chave = paste(Partido,Cod_Ibge)) %>% 
  select(Chave, Partido:Duracao_dias)