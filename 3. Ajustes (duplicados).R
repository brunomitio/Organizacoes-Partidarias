## ajustes
library(lubridate)
library(dplyr)

# retirando partidos que ja nao existem mais
org3 <- org2 %>% 
  filter(Partido!="MB",
         Partido!="PAN",
         Partido!="PL",
         Partido!= "PRONA",
         Partido!= "PS",
         Partido!= "PST",
         Partido!="PTS")

# Ajustes dos municipios
org3_2 <- org3 %>%
  filter(((Cod_Ibge == "1504752") & (Inicio < dmy("01/01/2013"))))
org3_3 <- org3 %>%
  filter(((Cod_Ibge == "4212650") & (Inicio < dmy("01/01/2013"))))
org3_4 <- org3 %>%
  filter(((Cod_Ibge == "4220000") & (Inicio < dmy("01/01/2013"))))
org3_5 <- org3 %>%
  filter(((Cod_Ibge == "4314548") & (Inicio < dmy("01/01/2013"))))
org3_6 <- org3 %>%
  filter(((Cod_Ibge == "5006275") & (Inicio < dmy("01/01/2013"))))
org3_7 <- rbind(org3_2, org3_3, org3_4, org3_5, org3_6)
org3 <- org3 %>% 
  filter(!((Cod_Ibge == "1504752") & (Inicio < dmy("01/01/2013")))) %>% 
  filter(!((Cod_Ibge == "4212650") & (Inicio < dmy("01/01/2013")))) %>% 
  filter(!((Cod_Ibge == "4220000") & (Inicio < dmy("01/01/2013")))) %>% 
  filter(!((Cod_Ibge == "4314548") & (Inicio < dmy("01/01/2013")))) %>% 
  filter(!((Cod_Ibge == "5006275") & (Inicio < dmy("01/01/2013"))))
org3_7 <- org3_7 %>% 
  filter(!(Fim < dmy("01/01/2013"))) %>% 
  mutate(Inicio = dmy("01/01/2013"))
org3 <- rbind(org3, org3_7)
rm(org3_2, org3_3, org3_4, org3_5, org3_6, org3_7)

# Ajustes para 2007
org3 <- org3 %>% 
  filter(!((Chave == "PSD 3504206") & (Inicio == dmy("01/10/2001")))) %>% 
  filter(!((Chave == "PROS 2107506") & (Inicio == dmy("05/06/2001")))) %>% 
  filter(!((Chave == "DEM 4306056") & (Inicio == dmy("09/03/2007")))) %>% 
  filter(!((Chave == "DEM 4306056") & (Inicio == dmy("19/04/2007")))) %>%
  filter(!((Chave == "DEM 4306056") & (Inicio == dmy("20/10/2007")))) %>% 
  filter(!((Chave == "PMN 2613701") & (Fim == dmy("09/08/2008")))) %>% 
  filter(!((Chave == "PMN 2613701") & (Fim == dmy("29/10/2008")))) %>% 
  filter(!((Chave == "PSB 4303905") & (Fim == dmy("25/02/2008")))) %>% 
  filter(!((Chave == "PSC 4306767") & (Fim == dmy("25/03/2008")))) %>% 
  filter(!((Chave == "PT 5208004") & (Fim == dmy("19/11/2008")))) %>% 
  filter(!((Chave == "PTN 4320008") & (Inicio == dmy("11/06/2008")))) %>% 
  filter(!((Chave == "PV 4313300") & (Inicio == dmy("01/04/2008"))))

org3_2 <- org3 %>% 
  filter(((Chave == "PSC 4306767") & (Inicio == dmy("17/07/2007")))) %>% 
  mutate(Fim = dmy("24/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSC 4306767") & (Inicio == dmy("17/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTN 4320008") & (Inicio == dmy("04/12/2007")))) %>% 
  mutate(Fim = dmy("24/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTN 4320008") & (Inicio == dmy("04/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 2205565") & (Inicio == dmy("17/10/2003")))) %>% 
  mutate(Fim = dmy("04/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 2205565") & (Inicio == dmy("17/10/2003") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 2206696") & (Inicio == dmy("26/12/2003")))) %>% 
  mutate(Fim = dmy("19/07/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 2206696") & (Fim == dmy("26/12/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 2207801") & (Inicio == dmy("14/12/2003")))) %>% 
  mutate(Fim = dmy("30/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 2207801") & (Fim == dmy("14/12/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 2210656") & (Inicio == dmy("27/09/2003")))) %>% 
  mutate(Fim = dmy("20/06/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 2210656") & (Fim == dmy("27/09/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1721257") & (Inicio == dmy("26/11/2006")))) %>% 
  mutate(Fim = dmy("19/09/2007"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 1721257") & (Inicio == dmy("26/11/2006")))) %>% 
  mutate(Inicio = dmy("19/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1721257") & (Inicio == dmy("26/11/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2206803") & (Inicio == dmy("18/04/2006")))) %>% 
  mutate(Fim = dmy("25/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2206803") & (Inicio == dmy("18/04/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2205805") & (Inicio == dmy("29/11/2006")))) %>% 
  mutate(Fim = dmy("15/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2205805") & (Fim == dmy("14/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2206308") & (Inicio == dmy("29/11/2006")))) %>% 
  mutate(Fim = dmy("18/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2206308") & (Fim == dmy("14/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2209377") & (Inicio == dmy("29/11/2006")))) %>% 
  mutate(Fim = dmy("18/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2209377") & (Fim == dmy("14/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 2202117") & (Inicio == dmy("06/09/2007")))) %>% 
  mutate(Fim = dmy("19/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 2202117") & (Inicio == dmy("06/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 5214606") & (Inicio == dmy("11/09/2007")))) %>% 
  mutate(Fim = dmy("19/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 5214606") & (Inicio == dmy("11/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2200459") & (Inicio == dmy("14/05/2007")))) %>% 
  mutate(Fim = dmy("21/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2200459") & (Inicio == dmy("14/05/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2201101") & (Inicio == dmy("02/07/2007")))) %>% 
  mutate(Fim = dmy("17/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2201101") & (Inicio == dmy("02/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2201770") & (Inicio == dmy("03/09/2007")))) %>% 
  mutate(Fim = dmy("27/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2201770") & (Inicio == dmy("03/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2202307") & (Inicio == dmy("26/09/2007")))) %>% 
  mutate(Fim = dmy("14/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2202307") & (Inicio == dmy("26/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2205201") & (Inicio == dmy("09/04/2007")))) %>% 
  mutate(Fim = dmy("28/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2205201") & (Inicio == dmy("09/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2205300") & (Inicio == dmy("03/09/2007")))) %>% 
  mutate(Fim = dmy("12/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2205300") & (Inicio == dmy("03/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2205953") & (Inicio == dmy("20/08/2007")))) %>% 
  mutate(Fim = dmy("21/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2205953") & (Inicio == dmy("20/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2207108") & (Inicio == dmy("10/09/2007")))) %>% 
  mutate(Fim = dmy("20/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2207108") & (Inicio == dmy("10/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2208650") & (Inicio == dmy("11/10/2007")))) %>% 
  mutate(Fim = dmy("29/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2208650") & (Inicio == dmy("11/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2210656") & (Inicio == dmy("20/08/2007")))) %>% 
  mutate(Fim = dmy("21/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2210656") & (Inicio == dmy("20/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2211407") & (Inicio == dmy("10/09/2007")))) %>% 
  mutate(Fim = dmy("29/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2211407") & (Inicio == dmy("10/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 1505064") & (Inicio == dmy("04/10/2007")))) %>% 
  mutate(Fim = dmy("01/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 1505064") & (Inicio == dmy("04/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 1506351") & (Inicio == dmy("04/10/2007")))) %>% 
  mutate(Fim = dmy("15/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 1506351") & (Inicio == dmy("04/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 1507409") & (Inicio == dmy("30/11/2007")))) %>% 
  mutate(Fim = dmy("17/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 1507409") & (Inicio == dmy("30/11/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2201200") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("19/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2201200") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2201200") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("19/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2201200") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2204402") & (Inicio == dmy("23/08/2007")))) %>% 
  mutate(Fim = dmy("29/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2204402") & (Inicio == dmy("23/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2205706") & (Inicio == dmy("21/09/2007")))) %>% 
  mutate(Fim = dmy("28/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2205706") & (Inicio == dmy("21/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2206209") & (Inicio == dmy("07/09/2007")))) %>% 
  mutate(Fim = dmy("21/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2206209") & (Inicio == dmy("07/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2206704") & (Inicio == dmy("11/09/2007")))) %>% 
  mutate(Fim = dmy("02/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2206704") & (Inicio == dmy("11/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2210052") & (Inicio == dmy("11/09/2007")))) %>% 
  mutate(Fim = dmy("22/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2210052") & (Inicio == dmy("11/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 5212204") & (Inicio == dmy("24/09/2007")))) %>% 
  mutate(Fim = dmy("20/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 5212204") & (Inicio == dmy("24/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1504208") & (Inicio == dmy("01/08/2007")))) %>% 
  mutate(Fim = dmy("21/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1504208") & (Inicio == dmy("01/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2201960") & (Inicio == dmy("10/10/2007")))) %>% 
  mutate(Fim = dmy("24/10/2007"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 2201960") & (Inicio == dmy("10/10/2007")))) %>% 
  mutate(Inicio = dmy("23/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2201960") & (Inicio == dmy("10/10/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2204808") & (Inicio == dmy("12/09/2007")))) %>% 
  mutate(Fim = dmy("15/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2204808") & (Inicio == dmy("12/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2209757") & (Inicio == dmy("08/08/2007")))) %>% 
  mutate(Fim = dmy("19/10/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2209757") & (Inicio == dmy("08/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2209906") & (Inicio == dmy("04/07/2007")))) %>% 
  mutate(Fim = dmy("29/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2209906") & (Inicio == dmy("04/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2203420") & (Inicio == dmy("03/07/2007")))) %>% 
  mutate(Fim = dmy("17/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2203420") & (Inicio == dmy("03/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2205557") & (Inicio == dmy("05/07/2007")))) %>% 
  mutate(Fim = dmy("21/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2205557") & (Inicio == dmy("05/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2206050") & (Inicio == dmy("17/07/2007")))) %>% 
  mutate(Fim = dmy("15/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2206050") & (Inicio == dmy("17/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2206357") & (Inicio == dmy("02/10/2007")))) %>% 
  mutate(Fim = dmy("06/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2206357") & (Inicio == dmy("02/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2209658") & (Inicio == dmy("24/09/2007")))) %>% 
  mutate(Fim = dmy("26/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2209658") & (Inicio == dmy("24/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2203909") & (Inicio == dmy("07/05/2007")))) %>% 
  mutate(Fim = dmy("01/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2203909") & (Inicio == dmy("07/05/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2205250") & (Inicio == dmy("16/04/2007")))) %>% 
  mutate(Fim = dmy("25/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2205250") & (Inicio == dmy("16/04/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2205953") & (Inicio == dmy("26/09/2007")))) %>% 
  mutate(Fim = dmy("17/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2205953") & (Inicio == dmy("26/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2206209") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("29/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2206209") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2209302") & (Inicio == dmy("04/09/2007")))) %>% 
  mutate(Fim = dmy("29/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2209302") & (Inicio == dmy("04/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 4302808") & (Inicio == dmy("03/08/2007")))) %>% 
  mutate(Fim = dmy("15/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 4302808") & (Inicio == dmy("03/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 5218789") & (Inicio == dmy("14/09/2007")))) %>% 
  mutate(Fim = dmy("21/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 5218789") & (Inicio == dmy("14/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 5221700") & (Inicio == dmy("14/09/2007")))) %>% 
  mutate(Fim = dmy("22/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 5221700") & (Inicio == dmy("14/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 1720978") & (Inicio == dmy("20/08/2007")))) %>% 
  mutate(Fim = dmy("01/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 1720978") & (Inicio == dmy("20/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2204303") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("21/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2204303") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2207777") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("20/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2207777") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2208650") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("21/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2208650") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2209757") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("28/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2209757") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 5219456") & (Inicio == dmy("15/10/2007")))) %>% 
  mutate(Fim = dmy("21/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 5219456") & (Inicio == dmy("15/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSL 1300607") & (Inicio == dmy("30/10/2007")))) %>% 
  mutate(Fim = dmy("10/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSL 1300607") & (Inicio == dmy("30/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 1708304") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("30/11/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 1708304") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 1709302") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("01/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 1709302") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 1712405") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("01/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 1712405") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 1713601") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("01/12/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 1713601") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1502004") & (Inicio == dmy("02/09/2007")))) %>% 
  mutate(Fim = dmy("16/09/2007"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1502004") & (Inicio == dmy("02/09/2007")))) %>% 
  mutate(Inicio = dmy("18/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1502004") & (Inicio == dmy("02/09/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

# Ajustes para 2008
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2409100") & (Inicio == dmy("27/12/2007") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 5102637") & (Inicio == dmy("22/09/2007") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 5200175") & (Inicio == dmy("06/01/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSDB 5208004") & (Inicio == dmy("25/06/2008") & (Tipo == "COMISSAO INTERVENTORA")))) %>% 
  filter(!((Chave == "PV 1301159") & (Inicio == dmy("17/06/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PV 4314902") & (Inicio == dmy("01/03/2008") & (Fim == dmy("21/05/2008"))))) %>% 
  filter(!((Chave == "PDT 2402006") & (Inicio == dmy("03/05/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 1200013") & (Inicio == dmy("01/06/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 1200054") & (Inicio == dmy("01/06/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 1200104") & (Inicio == dmy("01/06/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 1200708") & (Inicio == dmy("01/06/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSC 4303905") & (Fim == dmy("26/11/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PPS 5108105") & (Inicio == dmy("31/10/2008") & (Tipo == "COMISSAO PROVISORIA"))))

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 2206753") & (Inicio == dmy("13/10/2008")))) %>% 
  mutate(Fim = dmy("16/12/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 2206753") & (Inicio == dmy("13/10/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2202109") & (Inicio == dmy("26/09/2007")))) %>% 
  mutate(Fim = dmy("18/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2202109") & (Inicio == dmy("26/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)
  
org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 5209200") & (Inicio == dmy("12/11/2006")))) %>% 
  mutate(Fim = dmy("25/05/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 5209200") & (Inicio == dmy("12/11/2006")))) %>% 
  mutate(Inicio = dmy("24/08/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 5209200") & (Inicio == dmy("12/11/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1500909") & (Inicio == dmy("19/11/2007")))) %>% 
  mutate(Fim = dmy("03/04/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1500909") & (Inicio == dmy("19/11/2007")))) %>% 
  mutate(Inicio = dmy("05/10/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1500909") & (Inicio == dmy("19/11/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2207405") & (Inicio == dmy("23/11/2007")))) %>% 
  mutate(Fim = dmy("11/02/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 2207405") & (Inicio == dmy("23/11/2007")))) %>% 
  mutate(Inicio = dmy("11/08/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2207405") & (Inicio == dmy("23/11/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2206100") & (Inicio == dmy("02/03/2008")))) %>% 
  mutate(Fim = dmy("04/03/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 2206100") & (Inicio == dmy("02/03/2008")))) %>% 
  mutate(Inicio = dmy("01/09/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2206100") & (Inicio == dmy("02/03/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2209005") & (Inicio == dmy("08/12/2007")))) %>% 
  mutate(Fim = dmy("04/03/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 2209005") & (Inicio == dmy("08/12/2007")))) %>% 
  mutate(Inicio = dmy("01/09/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2209005") & (Inicio == dmy("08/12/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 1301308") & (Inicio == dmy("23/05/2007")))) %>% 
  mutate(Fim = dmy("06/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 1301308") & (Inicio == dmy("23/05/2007") & (Tipo == "COMISSAO INTERVENTORA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PRP 3202652") & (Inicio == dmy("01/03/2006")))) %>% 
  mutate(Inicio = dmy("01/03/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PRP 3202652") & (Inicio == dmy("01/03/2006"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 5215306") & (Inicio == dmy("19/08/2007")))) %>% 
  mutate(Fim = dmy("10/07/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSDB 5215306") & (Inicio == dmy("19/08/2007")))) %>% 
  mutate(Inicio = dmy("25/07/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 5215306") & (Inicio == dmy("19/08/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSOL 1301308") & (Inicio == dmy("19/08/2007")))) %>% 
  mutate(Fim = dmy("14/12/2007"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSOL 1301308") & (Inicio == dmy("19/08/2007")))) %>% 
  mutate(Inicio = dmy("16/12/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PSOL 1301308") & (Inicio == dmy("19/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PTDOB 1302306") & (Inicio == dmy("09/08/2007")))) %>% 
  mutate(Fim = dmy("20/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PTDOB 1302306") & (Inicio == dmy("09/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PV 1300102") & (Inicio == dmy("22/05/2008")))) %>% 
  mutate(Fim = dmy("11/06/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PV 1300102") & (Inicio == dmy("22/05/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PV 1301100") & (Inicio == dmy("08/05/2008")))) %>% 
  mutate(Fim = dmy("11/06/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PV 1301100") & (Inicio == dmy("08/05/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 1503200") & (Inicio == dmy("21/09/2007")))) %>% 
  mutate(Fim = dmy("12/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 1503200") & (Inicio == dmy("21/09/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 5205000") & (Inicio == dmy("20/10/2007")))) %>% 
  mutate(Fim = dmy("23/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 5205000") & (Inicio == dmy("20/10/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCB 2203909") & (Inicio == dmy("29/09/2007")))) %>% 
  mutate(Fim = dmy("24/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PCB 2203909") & (Inicio == dmy("29/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCB 2205300") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("27/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PCB 2205300") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCB 2206704") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("20/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PCB 2206704") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 1600501") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("01/06/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 1600501") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2611507") & (Inicio == dmy("14/07/2007")))) %>% 
  mutate(Fim = dmy("02/07/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2611507") & (Inicio == dmy("14/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2414209") & (Inicio == dmy("28/02/2008")))) %>% 
  mutate(Fim = dmy("09/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2414209") & (Inicio == dmy("28/02/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2203107") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("02/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2203107") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2204501") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("01/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2204501") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2205508") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("28/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2205508") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2209708") & (Inicio == dmy("19/12/2007")))) %>% 
  mutate(Fim = dmy("07/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2209708") & (Inicio == dmy("19/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2211001") & (Inicio == dmy("21/09/2007")))) %>% 
  mutate(Fim = dmy("07/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2211001") & (Inicio == dmy("21/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1200807") & (Inicio == dmy("01/01/2006")))) %>% 
  mutate(Fim = dmy("13/04/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 1200807") & (Inicio == dmy("01/01/2006")))) %>% 
  mutate(Inicio = dmy("15/10/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1200807") & (Inicio == dmy("01/01/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 5204102") & (Inicio == dmy("02/04/2008")))) %>% 
  mutate(Fim = dmy("09/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 5204102") & (Inicio == dmy("02/04/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2200400") & (Inicio == dmy("14/09/2007")))) %>% 
  mutate(Fim = dmy("20/01/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2200400") & (Inicio == dmy("14/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2202802") & (Inicio == dmy("25/10/2007")))) %>% 
  mutate(Fim = dmy("29/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2202802") & (Inicio == dmy("25/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2203230") & (Inicio == dmy("21/08/2007")))) %>% 
  mutate(Fim = dmy("02/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2203230") & (Inicio == dmy("21/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2208874") & (Inicio == dmy("12/02/2008")))) %>% 
  mutate(Fim = dmy("23/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2208874") & (Inicio == dmy("12/02/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2210508") & (Inicio == dmy("12/02/2008")))) %>% 
  mutate(Fim = dmy("22/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2210508") & (Inicio == dmy("12/02/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2211357") & (Inicio == dmy("13/01/2008")))) %>% 
  mutate(Fim = dmy("11/02/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 2211357") & (Inicio == dmy("13/01/2008")))) %>% 
  mutate(Inicio = dmy("11/08/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2211357") & (Inicio == dmy("13/01/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2203354") & (Inicio == dmy("20/09/2007")))) %>% 
  mutate(Fim = dmy("01/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2203354") & (Inicio == dmy("20/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2205300") & (Inicio == dmy("20/09/2007")))) %>% 
  mutate(Fim = dmy("15/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2205300") & (Inicio == dmy("20/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2206803") & (Inicio == dmy("05/10/2007")))) %>% 
  mutate(Fim = dmy("29/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2206803") & (Inicio == dmy("05/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2208858") & (Inicio == dmy("28/09/2007")))) %>% 
  mutate(Fim = dmy("26/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2208858") & (Inicio == dmy("28/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2210383") & (Inicio == dmy("28/09/2007")))) %>% 
  mutate(Fim = dmy("19/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2210383") & (Inicio == dmy("28/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2210656") & (Inicio == dmy("04/10/2007")))) %>% 
  mutate(Fim = dmy("07/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2210656") & (Inicio == dmy("04/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2200608") & (Inicio == dmy("17/07/2007")))) %>% 
  mutate(Fim = dmy("25/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2200608") & (Inicio == dmy("17/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2201176") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("06/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2201176") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2201606") & (Inicio == dmy("17/07/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2201606") & (Inicio == dmy("17/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2201739") & (Inicio == dmy("26/09/2007")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2201739") & (Inicio == dmy("26/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2201739") & (Inicio == dmy("26/09/2007")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2201739") & (Inicio == dmy("26/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2201903") & (Inicio == dmy("25/09/2007")))) %>% 
  mutate(Fim = dmy("18/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2201903") & (Inicio == dmy("25/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2202000") & (Inicio == dmy("26/03/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2202000") & (Inicio == dmy("26/03/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2202174") & (Inicio == dmy("09/04/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2202174") & (Inicio == dmy("09/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2202554") & (Inicio == dmy("13/06/2007")))) %>% 
  mutate(Fim = dmy("18/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2202554") & (Inicio == dmy("13/06/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2202802") & (Inicio == dmy("17/07/2007")))) %>% 
  mutate(Fim = dmy("06/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2202802") & (Inicio == dmy("17/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2203503") & (Inicio == dmy("11/09/2007")))) %>% 
  mutate(Fim = dmy("06/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2203503") & (Inicio == dmy("11/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2203602") & (Inicio == dmy("26/09/2007")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2203602") & (Inicio == dmy("26/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2203701") & (Inicio == dmy("31/07/2007")))) %>% 
  mutate(Fim = dmy("18/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2203701") & (Inicio == dmy("31/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2204105") & (Inicio == dmy("03/10/2007")))) %>% 
  mutate(Fim = dmy("01/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2204105") & (Inicio == dmy("03/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2204808") & (Inicio == dmy("30/04/2007")))) %>% 
  mutate(Fim = dmy("06/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2204808") & (Inicio == dmy("30/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2207108") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2207108") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2207207") & (Inicio == dmy("20/08/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2207207") & (Inicio == dmy("20/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2207751") & (Inicio == dmy("30/04/2007")))) %>% 
  mutate(Fim = dmy("27/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2207751") & (Inicio == dmy("30/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2208601") & (Inicio == dmy("04/07/2007")))) %>% 
  mutate(Fim = dmy("27/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2208601") & (Inicio == dmy("04/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2208700") & (Inicio == dmy("04/07/2007")))) %>% 
  mutate(Fim = dmy("28/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2208700") & (Inicio == dmy("04/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2208908") & (Inicio == dmy("19/09/2007")))) %>% 
  mutate(Fim = dmy("08/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2208908") & (Inicio == dmy("19/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2209203") & (Inicio == dmy("04/07/2007")))) %>% 
  mutate(Fim = dmy("18/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2209203") & (Inicio == dmy("04/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2209401") & (Inicio == dmy("30/04/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2209401") & (Inicio == dmy("30/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2210508") & (Inicio == dmy("04/07/2007")))) %>% 
  mutate(Fim = dmy("06/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2210508") & (Inicio == dmy("04/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2210706") & (Inicio == dmy("25/09/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2210706") & (Inicio == dmy("25/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2211407") & (Inicio == dmy("09/04/2007")))) %>% 
  mutate(Fim = dmy("05/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2211407") & (Inicio == dmy("09/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 4304507") & (Inicio == dmy("25/02/2008")))) %>% 
  mutate(Fim = dmy("15/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 4304507") & (Inicio == dmy("25/02/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 4311700") & (Inicio == dmy("25/02/2008")))) %>% 
  mutate(Fim = dmy("13/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 4311700") & (Inicio == dmy("25/02/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 4316451") & (Inicio == dmy("05/04/2007")))) %>% 
  mutate(Fim = dmy("12/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 4316451") & (Inicio == dmy("05/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 5108600") & (Inicio == dmy("05/10/2007")))) %>% 
  mutate(Fim = dmy("01/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 5108600") & (Inicio == dmy("05/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSC 1501501") & (Inicio == dmy("27/09/2007")))) %>% 
  mutate(Fim = dmy("26/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSC 1501501") & (Inicio == dmy("27/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 1720937") & (Inicio == dmy("20/08/2007")))) %>% 
  mutate(Fim = dmy("13/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 1720937") & (Inicio == dmy("20/08/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2200905") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("14/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2200905") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2202307") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("28/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2202307") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2202752") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("23/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2202752") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2209401") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("21/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2209401") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2210383") & (Inicio == dmy("13/02/2008")))) %>% 
  mutate(Fim = dmy("28/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2210383") & (Inicio == dmy("13/02/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2210607") & (Inicio == dmy("06/12/2007")))) %>% 
  mutate(Fim = dmy("23/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2210607") & (Inicio == dmy("06/12/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 5208806") & (Inicio == dmy("11/09/2007")))) %>% 
  mutate(Fim = dmy("15/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 5208806") & (Inicio == dmy("11/09/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 5216908") & (Inicio == dmy("04/10/2007")))) %>% 
  mutate(Fim = dmy("22/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 5216908") & (Inicio == dmy("04/10/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSOL 1300680") & (Inicio == dmy("07/07/2007")))) %>% 
  mutate(Fim = dmy("04/01/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PSOL 1300680") & (Inicio == dmy("07/07/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2202000") & (Inicio == dmy("18/09/2005")))) %>% 
  mutate(Fim = dmy("31/01/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2202000") & (Inicio == dmy("18/09/2005") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2202117") & (Inicio == dmy("18/09/2005")))) %>% 
  mutate(Fim = dmy("31/01/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2202117") & (Inicio == dmy("18/09/2005") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2202711") & (Inicio == dmy("18/07/2006")))) %>% 
  mutate(Fim = dmy("31/01/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2202711") & (Inicio == dmy("18/07/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2204352") & (Inicio == dmy("28/04/2006")))) %>% 
  mutate(Fim = dmy("08/02/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2204352") & (Inicio == dmy("28/04/2006") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 5005103") & (Inicio == dmy("30/06/2008")))) %>% 
  mutate(Fim = dmy("17/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 5005103") & (Inicio == dmy("30/06/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2200053") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2200053") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2200251") & (Inicio == dmy("09/03/2008")))) %>% 
  mutate(Fim = dmy("14/03/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PTB 2200251") & (Inicio == dmy("09/03/2008")))) %>% 
  mutate(Inicio = dmy("01/11/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2200251") & (Inicio == dmy("09/03/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2200400") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("18/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2200400") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2200608") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2200608") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2201002") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("19/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2201002") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2201200") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("25/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2201200") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2201606") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2201606") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2201903") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("25/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2201903") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2202604") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("27/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2202604") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2202802") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("29/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2202802") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2204204") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("11/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2204204") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2205102") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("25/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2205102") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2205250") & (Inicio == dmy("08/03/2008")))) %>% 
  mutate(Fim = dmy("14/03/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PTB 2205250") & (Inicio == dmy("08/03/2008")))) %>% 
  mutate(Inicio = dmy("01/11/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2205250") & (Inicio == dmy("08/03/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2205557") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("13/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2205557") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2209609") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("28/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2209609") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2209807") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("28/03/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2209807") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2210052") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("18/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2210052") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2210201") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("12/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2210201") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2210656") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2210656") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2210979") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("26/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2210979") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 2211605") & (Inicio == dmy("15/03/2008")))) %>% 
  mutate(Fim = dmy("25/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 2211605") & (Inicio == dmy("15/03/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 5208004") & (Inicio == dmy("17/01/2008")))) %>% 
  mutate(Fim = dmy("20/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 5208004") & (Inicio == dmy("17/01/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 5210109") & (Inicio == dmy("21/01/2008")))) %>% 
  mutate(Fim = dmy("09/04/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 5210109") & (Inicio == dmy("21/01/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 5212808") & (Inicio == dmy("24/01/08")))) %>% 
  mutate(Fim = dmy("29/06/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PTB 5212808") & (Inicio == dmy("24/01/08")))) %>% 
  mutate(Inicio = dmy("28/12/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 5212808") & (Inicio == dmy("24/01/08") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PV 4322509") & (Inicio == dmy("16/06/2008")))) %>% 
  mutate(Fim = dmy("19/05/2008"))
org3 <- org3 %>% 
  filter(!((Chave == "PV 4322509") & (Inicio == dmy("16/06/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2205508") & (Inicio == dmy("18/04/2006")))) %>% 
  mutate(Fim = dmy("30/09/2007"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2205508") & (Inicio == dmy("18/04/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

# Ajustes para 2009
org3 <- org3 %>% 
  filter(!((Chave == "DEM 1720903") & (Inicio == dmy("30/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2408003") & (Inicio == dmy("22/02/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PDT 2414506") & (Inicio == dmy("28/03/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PSB 3106200") & (Inicio == dmy("27/07/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PCDOB 2110005") & (Inicio == dmy("27/09/2009") & (Tipo == "COMISSAO EXECUTIVA")))) %>% 
  filter(!((Chave == "PV 4311403") & (Fim == dmy("30/06/2009") & (Tipo == "COMISSAO PROVISORIA"))))

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 1508100") & (Inicio == dmy("10/11/2007")))) %>% 
  mutate(Fim = dmy("22/03/2009"))
org3_3 <- org3 %>% 
  filter(((Chave == "DEM 1508100") & (Inicio == dmy("10/11/2007")))) %>% 
  mutate(Inicio = dmy("21/07/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 1508100") & (Inicio == dmy("10/11/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 5005707") & (Inicio == dmy("23/03/2008")))) %>% 
  mutate(Fim = dmy("28/06/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 5005707") & (Inicio == dmy("23/03/2008")))) %>% 
  mutate(Inicio = dmy("16/01/2010"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 5005707") & (Inicio == dmy("23/03/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 1301209") & (Inicio == dmy("12/02/2008")))) %>% 
  mutate(Fim = dmy("12/08/2009"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSB 1301209") & (Inicio == dmy("12/02/2008")))) %>% 
  mutate(Inicio = dmy("11/11/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 1301209") & (Inicio == dmy("12/02/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 5204805") & (Inicio == dmy("30/03/2008")))) %>% 
  mutate(Fim = dmy("15/04/2009"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSB 5204805") & (Inicio == dmy("30/03/2008")))) %>% 
  mutate(Inicio = dmy("15/06/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 5204805") & (Inicio == dmy("30/03/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSOL 1304401") & (Inicio == dmy("07/04/2007")))) %>% 
  mutate(Fim = dmy("19/01/2008"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSOL 1304401") & (Inicio == dmy("07/04/2007")))) %>% 
  mutate(Inicio = dmy("21/01/2010"))
org3 <- org3 %>% 
  filter(!((Chave == "PSOL 1304401") & (Inicio == dmy("07/04/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 1718865") & (Inicio == dmy("02/12/2007")))) %>% 
  mutate(Fim = dmy("07/04/2009"))
org3_3 <- org3 %>% 
  filter(((Chave == "PT 1718865") & (Inicio == dmy("02/12/2007")))) %>% 
  mutate(Inicio = dmy("05/10/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 1718865") & (Inicio == dmy("02/12/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 1200450") & (Inicio == dmy("10/05/2007")))) %>% 
  mutate(Fim = dmy("17/01/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 1200450") & (Inicio == dmy("10/05/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2205508") & (Inicio == dmy("24/06/2007")))) %>% 
  mutate(Fim = dmy("03/06/2009"))
org3_3 <- org3 %>% 
  filter(((Chave == "PDT 2205508") & (Inicio == dmy("24/06/2007")))) %>% 
  mutate(Inicio = dmy("05/09/2009"),
         Fim = dmy("20/09/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2205508") & (Inicio == dmy("24/06/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2209153") & (Inicio == dmy("15/07/2007")))) %>% 
  mutate(Fim = dmy("03/06/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2209153") & (Inicio == dmy("15/07/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 5002704") & (Inicio == dmy("22/08/2009")))) %>% 
  mutate(Fim = dmy("08/09/2009"))
org3_3 <- org3 %>% 
  filter(((Chave == "PDT 5002704") & (Inicio == dmy("22/08/2009")))) %>% 
  mutate(Inicio = dmy("14/10/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 5002704") & (Inicio == dmy("22/08/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2611606") & (Inicio == dmy("21/10/2007")))) %>% 
  mutate(Fim = dmy("14/05/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2611606") & (Inicio == dmy("21/10/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1709807") & (Inicio == dmy("07/10/2009")))) %>% 
  mutate(Fim = dmy("24/05/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1709807") & (Inicio == dmy("07/10/2009") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1718204") & (Inicio == dmy("25/06/2008")))) %>% 
  mutate(Fim = dmy("24/05/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1718204") & (Inicio == dmy("25/06/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 4314050") & (Inicio == dmy("31/08/2007")))) %>% 
  mutate(Fim = dmy("23/04/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 4314050") & (Inicio == dmy("31/08/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTC 5212501") & (Inicio == dmy("15/09/2009")))) %>% 
  mutate(Fim = dmy("27/05/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PTC 5212501") & (Inicio == dmy("15/09/2009") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1100106") & (Inicio == dmy("26/11/2006")))) %>% 
  mutate(Fim = dmy("30/12/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1100106") & (Inicio == dmy("26/11/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1100379") & (Inicio == dmy("26/11/2006")))) %>% 
  mutate(Fim = dmy("30/12/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1100379") & (Inicio == dmy("26/11/2006") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 1101450") & (Inicio == dmy("05/11/2009")))) %>% 
  mutate(Fim = dmy("30/12/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 1101450") & (Inicio == dmy("05/11/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2209807") & (Inicio == dmy("12/03/2007")))) %>% 
  mutate(Fim = dmy("30/12/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2209807") & (Inicio == dmy("12/03/2007") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2207702") & (Inicio == dmy("27/07/2007")))) %>% 
  mutate(Fim = dmy("03/06/2009"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2207702") & (Inicio == dmy("27/07/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

# Ajustes para 2010
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2100105") & (Inicio == dmy("19/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2100808") & (Inicio == dmy("19/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2101202") & (Inicio == dmy("29/08/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2101806") & (Inicio == dmy("27/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2102077") & (Inicio == dmy("30/08/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2103554") & (Inicio == dmy("13/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2106706") & (Inicio == dmy("26/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2110278") & (Inicio == dmy("22/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2114007") & (Inicio == dmy("13/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PHS 5210208") & (Inicio == dmy("02/10/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PMN 1503309") & (Inicio == dmy("29/09/2009") & (Tipo == "COMISSAO PROVISORIA")))) %>%  
  filter(!((Chave == "PSDB 5201405") & (Fim == dmy("30/03/2010") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PEN 1400175") & (Inicio == dmy("23/03/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PEN 1400209") & (Inicio == dmy("23/03/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PEN 1400308") & (Inicio == dmy("23/03/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PPL 1501402") & (Inicio == dmy("17/10/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PPL 3506359") & (Inicio == dmy("19/08/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PPL 3513009") & (Inicio == dmy("12/07/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PPL 3513504") & (Inicio == dmy("01/08/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PPL 3519071") & (Inicio == dmy("05/08/2009") & (Tipo == "DIRETORIO")))) %>%
  filter(!((Chave == "PPL 4314902") & (Inicio == dmy("29/08/2009") & (Tipo == "DIRETORIO")))) %>% 
  filter(!((Chave == "PTN 3503901") & (Fim == dmy("22/11/2011") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSL 3516309") & (Fim == dmy("10/03/2012") & (Tipo == "COMISSAO PROVISORIA")))) 

org3_2 <- org3 %>% 
  filter(((Chave == "PT 5107701") & (Inicio == dmy("03/04/2008")))) %>% 
  mutate(Fim = dmy("19/02/2010"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 5107701") & (Inicio == dmy("03/04/2008") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2208403") & (Inicio == dmy("30/08/2009")))) %>% 
  mutate(Fim = dmy("25/11/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2208403") & (Inicio == dmy("30/08/2009")))) %>%
  filter(!((Chave == "PCDOB 2208403") & (Inicio == dmy("25/11/2009"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSD 2919157") & (Inicio == dmy("03/10/2010")))) %>% 
  mutate(Inicio = dmy("27/09/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PSD 2919157") & (Inicio == dmy("03/10/2010"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSD 3529807") & (Inicio == dmy("01/10/2010")))) %>% 
  mutate(Inicio = dmy("27/09/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PSD 3529807") & (Inicio == dmy("01/10/2010"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2516201") & (Inicio == dmy("26/02/2010")))) %>% 
  mutate(Fim = dmy("15/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PT 2516201") & (Inicio == dmy("26/02/2010")))) %>% 
  mutate(Inicio = dmy("27/02/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2516201") & (Inicio == dmy("26/02/2010") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

# Ajustes para 2011
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 4113700") & (Inicio == dmy("02/07/2012") & (Tipo == "DIRETORIO")))) %>% 
  filter(!((Chave == "PRB 3506359") & (Inicio == dmy("19/09/2011") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PTN 2903201") & (Inicio == dmy("19/09/2011") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSDC 3301009") & (Inicio == dmy("02/05/2011") & (Tipo == "DIRETORIO")))) %>% 
  filter(!((Chave == "PTN 2903201") & (Inicio == dmy("24/08/2011") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSDB 2205581") & (Inicio == dmy("04/10/2011") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PRTB 3106200") & (Inicio == dmy("02/08/2011") & (Tipo == "COMISSAO PROVISORIA"))))
  
org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 2927705") & (Inicio == dmy("03/11/2009")))) %>% 
  mutate(Fim = dmy("31/07/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 2927705") & (Inicio == dmy("03/11/2009")))) %>% 
  mutate(Inicio = dmy("02/11/2011"),
         Fim = dmy("02/04/2012"))
org3_4 <- org3 %>% 
  filter(((Chave == "PMDB 2927705") & (Inicio == dmy("03/11/2009")))) %>% 
  mutate(Inicio = dmy("18/05/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 2927705") & (Inicio == dmy("03/11/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3, org3_4)
rm(org3_2, org3_3, org3_4)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 3512001") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Fim = dmy("08/08/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 3512001") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Inicio = dmy("10/11/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 3512001") & (Inicio == dmy("26/10/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 3519204") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Fim = dmy("13/06/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 3519204") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Inicio = dmy("10/11/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 3519204") & (Inicio == dmy("26/10/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 3548807") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Fim = dmy("09/05/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 3548807") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Inicio = dmy("15/10/2011"),
         Fim = dmy("26/03/2012"))
org3_4 <- org3 %>% 
  filter(((Chave == "PMDB 3548807") & (Inicio == dmy("26/10/2009")))) %>% 
  mutate(Inicio = dmy("21/10/2012"),
         Fim = dmy("05/11/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 3548807") & (Inicio == dmy("26/10/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3, org3_4)
rm(org3_2, org3_3, org3_4)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 3548807") & (Inicio == dmy("06/11/2012")))) %>% 
  mutate(Fim = dmy("20/02/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 3548807") & (Inicio == dmy("06/11/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 2100873") & (Inicio == dmy("10/06/2008")))) %>% 
  mutate(Fim = dmy("11/03/2010"))
org3_3 <- org3 %>% 
  filter(((Chave == "PT 2100873") & (Inicio == dmy("10/06/2008")))) %>% 
  mutate(Inicio = dmy("13/03/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 2100873") & (Inicio == dmy("10/06/2008") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPL 1501402") & (Inicio == dmy("04/09/2011")))) %>% 
  mutate(Inicio = dmy("04/10/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PPL 1501402") & (Inicio == dmy("04/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PEN 3505609") & (Inicio == dmy("26/05/2011")))) %>% 
  mutate(Inicio = dmy("19/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PEN 3505609") & (Inicio == dmy("26/05/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PEN 3539509") & (Inicio == dmy("26/05/2011")))) %>% 
  mutate(Inicio = dmy("19/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PEN 3539509") & (Inicio == dmy("26/05/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PEN 2304400") & (Inicio == dmy("04/04/2011")))) %>% 
  mutate(Inicio = dmy("19/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PEN 2304400") & (Inicio == dmy("04/04/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PEN 3514601") & (Inicio == dmy("26/05/2011")))) %>% 
  mutate(Inicio = dmy("19/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PEN 3514601") & (Inicio == dmy("26/05/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 5007109") & (Inicio == dmy("03/05/2009")))) %>% 
  mutate(Fim = dmy("03/10/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PHS 5007109") & (Inicio == dmy("03/05/2009")))) %>% 
  mutate(Inicio = dmy("04/10/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 5007109") & (Inicio == dmy("03/05/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 2604106") & (Inicio == dmy("13/06/2011")))) %>% 
  mutate(Inicio = dmy("29/05/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 2604106") & (Inicio == dmy("13/06/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 4113700") & (Inicio == dmy("13/03/2010")))) %>% 
  mutate(Fim = dmy("04/12/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 4113700") & (Inicio == dmy("13/03/2010")))) %>% 
  mutate(Inicio = dmy("06/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 4113700") & (Inicio == dmy("13/03/2010"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PRTB 3106200") & (Inicio == dmy("03/02/2012")))) %>% 
  mutate(Fim = dmy("30/05/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PRTB 3106200") & (Inicio == dmy("03/02/2012")))) %>% 
  mutate(Inicio = dmy("07/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PRTB 3106200") & (Inicio == dmy("03/02/2012") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

# Ajustes para 2012
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2800670") & (Inicio == dmy("28/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2800670") & (Inicio == dmy("21/08/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PDT 2414506") & (Inicio == dmy("28/03/2008") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PMN 2407807") & (Inicio == dmy("28/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PPS 2901205") & (Inicio == dmy("21/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PR 2601904") & (Inicio == dmy("20/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PR 2911709") & (Inicio == dmy("05/07/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PRB 2307502") & (Inicio == dmy("22/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PSB 2906501") & (Inicio == dmy("11/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSDC 2903201") & (Inicio == dmy("04/05/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PR 2407807") & (Inicio == dmy("28/03/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSDB 2910701") & (Inicio == dmy("28/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PTB 4105003") & (Inicio == dmy("03/09/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2926806") & (Inicio == dmy("22/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PCDOB 2930907") & (Inicio == dmy("11/07/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PCDOB 2930907") & (Inicio == dmy("18/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PHS 2100709") & (Inicio == dmy("25/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PHS 2100709") & (Inicio == dmy("01/09/2015") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PHS 3109402") & (Inicio == dmy("21/05/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PPS 2903904") & (Fim == dmy("29/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PRP 5005608") & (Inicio == dmy("14/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PRTB 3133808") & (Inicio == dmy("01/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 2307502") & (Inicio == dmy("27/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 2902005") & (Inicio == dmy("19/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 5001904") & (Inicio == dmy("26/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PSDC 5208905") & (Inicio == dmy("25/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>%
  filter(!((Chave == "PTN 3548500") & (Inicio == dmy("29/06/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PV 4115200") & (Inicio == dmy("11/06/2012") & (Tipo == "COMISSAO PROVISORIA"))))

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2930907") & (Inicio == dmy("28/09/2011")))) %>% 
  mutate(Fim = dmy("16/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2930907") & (Inicio == dmy("28/09/2011") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 5005608") & (Inicio == dmy("08/11/2011")))) %>% 
  mutate(Fim = dmy("28/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PDT 5005608") & (Inicio == dmy("08/11/2011")))) %>% 
  mutate(Inicio = dmy("01/10/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 5005608") & (Inicio == dmy("08/11/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 2311231") & (Inicio == dmy("23/05/2009")))) %>% 
  mutate(Fim = dmy("17/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PHS 2311231") & (Inicio == dmy("23/05/2009")))) %>% 
  mutate(Inicio = dmy("01/08/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 2311231") & (Inicio == dmy("23/05/2009") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 3548500") & (Inicio == dmy("18/05/2010")))) %>% 
  mutate(Fim = dmy("04/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PHS 3548500") & (Inicio == dmy("18/05/2010")))) %>% 
  mutate(Inicio = dmy("28/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 3548500") & (Inicio == dmy("18/05/2010") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600303") & (Inicio == dmy("26/11/2011")))) %>% 
  mutate(Fim = dmy("31/03/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1600303") & (Inicio == dmy("26/11/2011")))) %>% 
  mutate(Inicio = dmy("18/07/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600303") & (Inicio == dmy("26/11/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2108207") & (Inicio == dmy("09/10/2011")))) %>% 
  mutate(Fim = dmy("04/07/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 2108207") & (Inicio == dmy("09/10/2011")))) %>% 
  mutate(Inicio = dmy("07/07/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2108207") & (Inicio == dmy("09/10/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2100709") & (Inicio == dmy("23/10/2011")))) %>% 
  mutate(Fim = dmy("03/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PPS 2100709") & (Inicio == dmy("23/10/2011")))) %>% 
  mutate(Inicio = dmy("28/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2100709") & (Inicio == dmy("23/10/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PTDOB 2906501") & (Inicio == dmy("20/11/2011")))) %>% 
  mutate(Fim = dmy("17/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PTDOB 2906501") & (Inicio == dmy("20/11/2011")))) %>% 
  mutate(Inicio = dmy("22/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PTDOB 2906501") & (Inicio == dmy("20/11/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "DEM 2502805") & (Inicio == dmy("31/10/2011")))) %>% 
  mutate(Fim = dmy("23/05/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "DEM 2502805") & (Inicio == dmy("31/10/2011")))) %>% 
  mutate(Inicio = dmy("01/01/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "DEM 2502805") & (Inicio == dmy("31/10/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2504009") & (Inicio == dmy("23/10/2011")))) %>% 
  mutate(Fim = dmy("03/07/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PPS 2504009") & (Inicio == dmy("23/10/2011")))) %>% 
  mutate(Inicio = dmy("01/01/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2504009") & (Inicio == dmy("23/10/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PR 2407807") & (Inicio == dmy("12/09/2007")))) %>% 
  mutate(Fim = dmy("27/03/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PR 2407807") & (Inicio == dmy("12/09/2007") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2918407") & (Inicio == dmy("20/03/2011")))) %>% 
  mutate(Fim = dmy("16/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSDB 2918407") & (Inicio == dmy("20/03/2011")))) %>% 
  mutate(Inicio = dmy("01/01/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2918407") & (Inicio == dmy("20/03/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 2919207") & (Inicio == dmy("20/03/2011")))) %>% 
  mutate(Fim = dmy("19/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSDB 2919207") & (Inicio == dmy("20/03/2011")))) %>% 
  mutate(Inicio = dmy("01/01/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 2919207") & (Inicio == dmy("20/03/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDC 3541000") & (Inicio == dmy("24/07/2011")))) %>% 
  mutate(Fim = dmy("29/09/2011"))
org3_3 <- org3 %>% 
  filter(((Chave == "PSDC 3541000") & (Inicio == dmy("24/07/2011")))) %>% 
  mutate(Inicio = dmy("01/04/2012"),
         Fim = dmy("15/05/2012"))
org3_4 <- org3 %>% 
  filter(((Chave == "PSDC 3541000") & (Inicio == dmy("24/07/2011")))) %>% 
  mutate(Inicio = dmy("17/11/2012"),
         Fim = dmy("13/08/2013"))
org3_5 <- org3 %>% 
  filter(((Chave == "PSDC 3541000") & (Inicio == dmy("24/07/2011")))) %>% 
  mutate(Inicio = dmy("12/12/2014"),
         Fim = dmy("07/05/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDC 3541000") & (Inicio == dmy("24/07/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3, org3_4, org3_5)
rm(org3_2, org3_3, org3_4, org3_5)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDC 3541000") & (Inicio == dmy("08/05/2015")))) %>% 
  mutate(Fim = dmy("06/08/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDC 3541000") & (Inicio == dmy("08/05/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 1303304") & (Inicio == dmy("19/08/2011")))) %>% 
  mutate(Inicio = dmy("25/05/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 1303304") & (Inicio == dmy("19/08/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 1304005") & (Inicio == dmy("26/05/2012")))) %>% 
  mutate(Fim = dmy("25/05/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 1304005") & (Inicio == dmy("26/05/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 1501808") & (Inicio == dmy("06/09/2011")))) %>% 
  mutate(Fim = dmy("30/09/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 1501808") & (Inicio == dmy("06/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 3170107") & (Inicio == dmy("28/10/2009")))) %>% 
  mutate(Fim = dmy("23/05/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 3170107") & (Inicio == dmy("28/10/2009"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 5006002") & (Inicio == dmy("06/12/2012")))) %>% 
  mutate(Fim = dmy("14/12/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 5006002") & (Inicio == dmy("06/12/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 2513851") & (Inicio == dmy("10/07/2012")))) %>% 
  mutate(Fim = dmy("24/07/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 2513851") & (Inicio == dmy("10/07/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 3162658") & (Inicio == dmy("30/09/2011")))) %>% 
  mutate(Fim = dmy("02/01/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 3162658") & (Inicio == dmy("30/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 3169505") & (Inicio == dmy("31/08/2010")))) %>% 
  mutate(Fim = dmy("18/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 3169505") & (Inicio == dmy("19/06/2012")))) %>% 
  mutate(Fim = dmy("19/08/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 3169505") & (Inicio == dmy("31/08/2010") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PP 3169505") & (Inicio == dmy("19/06/2012") & (Tipo == "COMISSAO PROVISORIA"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2201101") & (Inicio == dmy("18/11/2011")))) %>% 
  mutate(Fim = dmy("28/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PPS 2201101") & (Inicio == dmy("18/11/2011")))) %>% 
  mutate(Inicio = dmy("19/11/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2201101") & (Inicio == dmy("18/11/2011") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PPS 2903904") & (Inicio == dmy("30/09/2011")))) %>% 
  mutate(Inicio = dmy("19/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PPS 2903904") & (Inicio == dmy("30/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PRTB 3109402") & (Inicio == dmy("19/08/2011")))) %>% 
  mutate(Inicio = dmy("30/05/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PRTB 3109402") & (Inicio == dmy("19/08/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PRTB 3133808") & (Inicio == dmy("23/09/2011")))) %>% 
  mutate(Inicio = dmy("11/03/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PRTB 3133808") & (Inicio == dmy("23/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 2903201") & (Inicio == dmy("03/10/2011")))) %>% 
  mutate(Fim = dmy("05/10/2011"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 2903201") & (Inicio == dmy("03/10/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 5001904") & (Inicio == dmy("27/09/2011")))) %>% 
  mutate(Inicio = dmy("25/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 5001904") & (Inicio == dmy("27/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 5008008") & (Inicio == dmy("14/09/2011")))) %>% 
  mutate(Fim = dmy("03/02/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PSB 5008008") & (Inicio == dmy("14/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSL 2906501") & (Inicio == dmy("08/09/2011")))) %>% 
  mutate(Inicio = dmy("26/06/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PSL 2906501") & (Inicio == dmy("08/09/2011"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PV 4115200") & (Inicio == dmy("09/01/2008")))) %>% 
  mutate(Inicio = dmy("23/03/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PV 4115200") & (Inicio == dmy("09/01/2008"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 3543402") & (Inicio == dmy("12/05/2012")))) %>% 
  mutate(Fim = dmy("04/06/2012"))
org3_3 <- org3 %>% 
  filter(((Chave == "PHS 3543402") & (Inicio == dmy("12/05/2012")))) %>% 
  mutate(Inicio = dmy("05/07/2012"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 3543402") & (Inicio == dmy("12/05/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

# Ajustes para 2013

org3 <- org3 %>% 
  filter(!((Chave == "PSOL 4307807") & (Inicio == dmy("18/12/2012") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "DEM 5001102") & (Inicio == dmy("31/12/2013") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PTN 3304557") & (Inicio == dmy("18/12/2013") & (Tipo == "COMISSAO PROVISORIA"))))

org3_2 <- org3 %>% 
  filter(((Chave == "NOVO 3515004") & (Inicio == dmy("07/06/2013")))) %>% 
  mutate(Inicio = dmy("15/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "NOVO 3515004") & (Inicio == dmy("07/06/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "NOVO 3515186") & (Inicio == dmy("07/06/2013")))) %>% 
  mutate(Inicio = dmy("15/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "NOVO 3515186") & (Inicio == dmy("07/06/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2304400") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2304400") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSB 4213203") & (Inicio == dmy("08/07/2013")))) %>% 
  mutate(Fim = NA)
org3 <- org3 %>% 
  filter(!((Chave == "PSB 4213203") & (Inicio == dmy("31/12/2013")))) %>% 
  filter(!((Chave == "PSB 4213203") & (Inicio == dmy("08/07/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2104107") & (Inicio == dmy("20/06/2013")))) %>% 
  mutate(Fim = dmy("30/08/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2104107") & (Inicio == dmy("20/06/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2107258") & (Inicio == dmy("02/11/2012")))) %>% 
  mutate(Fim = dmy("30/08/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2107258") & (Inicio == dmy("02/11/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600238") & (Inicio == dmy("28/01/2012")))) %>% 
  mutate(Fim = dmy("02/08/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600238") & (Inicio == dmy("28/01/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600550") & (Inicio == dmy("28/01/2012")))) %>% 
  mutate(Fim = dmy("04/08/2013"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600550") & (Inicio == dmy("28/01/2012"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "NOVO 3550308") & (Inicio == dmy("07/06/2013")))) %>% 
  mutate(Inicio = dmy("15/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "NOVO 3550308") & (Inicio == dmy("07/06/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2304202") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2304202") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2307304") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2307304") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2307650") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2307650") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2304202") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2304202") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2308906") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2308906") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2309607") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2309607") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 3144607") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 3144607") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PMB 2308302") & (Inicio == dmy("30/04/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMB 2308302") & (Inicio == dmy("30/04/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "REDE 3549805") & (Inicio == dmy("10/08/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "REDE 3549805") & (Inicio == dmy("10/08/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "REDE 3550308") & (Inicio == dmy("10/08/2013")))) %>% 
  mutate(Inicio = dmy("29/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "REDE 3550308") & (Inicio == dmy("10/08/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

# Ajustes para 2014
org3 <- org3 %>% 
  filter(!((Chave == "DEM 3510609") & (Inicio == dmy("13/10/2011") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2110807") & (Inicio == dmy("13/09/2013") & (Tipo == "DIRETORIO")))) %>% 
  filter(!((Chave == "PSDB 5222005") & (Inicio == dmy("24/04/2013") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PMN 3104007") & (Inicio == dmy("31/12/2014") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSDB 2310209") & (Inicio == dmy("31/12/2014") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PSB 2509909") & (Inicio == dmy("15/01/2014") & (membros == 7))))

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2110807") & (Inicio == dmy("31/08/2013")))) %>% 
  mutate(Fim = dmy("13/05/2013"))
org3_3 <- org3 %>% 
  filter(((Chave == "PDT 2110807") & (Inicio == dmy("31/08/2013")))) %>% 
  mutate(Inicio = dmy("15/07/2013"),
         Fim = dmy("11/01/2015"))
org3_4 <- org3 %>% 
  filter(((Chave == "PDT 2110807") & (Inicio == dmy("31/08/2013")))) %>% 
  mutate(Inicio = dmy("13/04/2015"),
         Fim = dmy("11/05/2015"))
org3_5 <- org3 %>% 
  filter(((Chave == "PDT 2110807") & (Inicio == dmy("31/08/2013")))) %>% 
  mutate(Inicio = dmy("24/07/2015"),
         Fim = dmy("09/09/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2110807") & (Inicio == dmy("31/08/2013") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3, org3_4, org3_5)
rm(org3_2, org3_3, org3_4, org3_5)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600212") & (Inicio == dmy("04/08/2013")))) %>% 
  mutate(Fim = dmy("23/03/2014"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1600212") & (Inicio == dmy("04/08/2013")))) %>% 
  mutate(Inicio = dmy("16/05/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600212") & (Inicio == dmy("04/08/2013") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600253") & (Inicio == dmy("04/08/2013")))) %>% 
  mutate(Fim = dmy("26/02/2014"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1600253") & (Inicio == dmy("04/08/2013")))) %>% 
  mutate(Inicio = dmy("16/05/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600253") & (Inicio == dmy("04/08/2013") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600402") & (Inicio == dmy("06/08/2013")))) %>% 
  mutate(Fim = dmy("26/02/2014"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1600402") & (Inicio == dmy("06/08/2013")))) %>% 
  mutate(Inicio = dmy("16/05/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600402") & (Inicio == dmy("06/08/2013") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600535") & (Inicio == dmy("03/08/2013")))) %>% 
  mutate(Fim = dmy("26/02/2014"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1600535") & (Inicio == dmy("03/08/2013")))) %>% 
  mutate(Inicio = dmy("16/05/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600535") & (Inicio == dmy("03/08/2013") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PP 1600709") & (Inicio == dmy("05/08/2013")))) %>% 
  mutate(Fim = dmy("26/02/2014"))
org3_3 <- org3 %>% 
  filter(((Chave == "PP 1600709") & (Inicio == dmy("05/08/2013")))) %>% 
  mutate(Inicio = dmy("16/05/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PP 1600709") & (Inicio == dmy("05/08/2013") & (Tipo == "DIRETORIO"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 4314902") & (Inicio == dmy("21/03/2014")))) %>% 
  mutate(Inicio = dmy("30/12/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PHS 4314902") & (Inicio == dmy("21/03/2014"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PHS 4314902") & (Inicio == dmy("21/03/2014")))) %>% 
  mutate(Fim = NA)
org3 <- org3 %>% 
  filter(!((Chave == "PHS 4314902") & (Inicio == dmy("31/12/2014")))) %>% 
  filter(!((Chave == "PHS 4314902") & (Inicio == dmy("21/03/2014"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTN 3300100") & (Inicio == dmy("21/01/2014")))) %>% 
  mutate(Fim = dmy("28/02/2014"))
org3 <- org3 %>% 
  filter(!((Chave == "PTN 3300100") & (Inicio == dmy("21/01/2014"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

# Ajustes para 2015

org3 <- org3 %>% 
  filter(!((Chave == "PDT 2104107") & (Inicio == dmy("12/01/2015") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2104107") & (Inicio == dmy("12/05/2015") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2104107") & (Inicio == dmy("12/08/2015") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PDT 2107258") & (Inicio == dmy("26/03/2015") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PPS 3504008") & (Inicio == dmy("19/08/2015") & (Tipo == "COMISSAO PROVISORIA")))) %>% 
  filter(!((Chave == "PT 2902500") & (Inicio == dmy("09/11/2015") & (Tipo == "DIRETORIO"))))

org3_2 <- org3 %>% 
  filter(((Chave == "PCDOB 2929206") & (Inicio == dmy("18/08/2013")))) %>% 
  mutate(Fim = dmy("16/01/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PCDOB 2929206") & (Inicio == dmy("18/08/2013"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2104073") & (Inicio == dmy("31/08/2013") & (Fim == dmy("31/08/2015"))))) %>% 
  mutate(Fim = dmy("11/05/2015"))
org3_3 <- org3 %>% 
  filter(((Chave == "PDT 2104073") & (Inicio == dmy("31/08/2013") & (Fim == dmy("31/08/2015"))))) %>% 
  mutate(Inicio = dmy("30/07/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2104073") & (Inicio == dmy("31/08/2013") & (Fim == dmy("31/08/2015")))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 2107258") & (Inicio == dmy("31/08/2013")))) %>% 
  mutate(Fim = dmy("25/03/2015"))
org3_3 <- org3 %>% 
  filter(((Chave == "PDT 2107258") & (Inicio == dmy("31/08/2013")))) %>% 
  mutate(Inicio = dmy("27/06/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 2107258") & (Inicio == dmy("31/08/2013"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 3109402") & (Inicio == dmy("16/08/2015")))) %>% 
  mutate(Fim = dmy("15/09/2015"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 3109402") & (Inicio == dmy("16/08/2015")))) %>% 
  mutate(Inicio = dmy("01/11/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 3109402") & (Inicio == dmy("16/08/2015"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 4106902") & (Inicio == dmy("23/02/2014")))) %>% 
  mutate(Fim = dmy("06/12/2015"))
org3_3 <- org3 %>% 
  filter(((Chave == "PMDB 4106902") & (Inicio == dmy("23/02/2014")))) %>% 
  mutate(Inicio = dmy("08/06/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 4106902") & (Inicio == dmy("23/02/2014"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 3157906") & (Inicio == dmy("10/05/2015")))) %>% 
  mutate(Fim = dmy("12/07/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 3157906") & (Inicio == dmy("10/05/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PR 4112306") & (Inicio == dmy("28/09/2007")))) %>% 
  mutate(Fim = dmy("29/07/2015"))
org3_3 <- org3 %>% 
  filter(((Chave == "PR 4112306") & (Inicio == dmy("28/09/2007")))) %>% 
  mutate(Inicio = dmy("13/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PR 4112306") & (Inicio == dmy("28/09/2007"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PMDB 4105805") & (Inicio == dmy("15/02/2014")))) %>% 
  mutate(Fim = dmy("30/08/2015"))
org3 <- org3 %>% 
  filter(!((Chave == "PMDB 4105805") & (Inicio == dmy("15/02/2014"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PROS 2903201") & (Inicio == dmy("30/10/2013")))) %>% 
  mutate(Fim = dmy("15/09/2015"))
org3_3 <- org3 %>% 
  filter(((Chave == "PROS 2903201") & (Inicio == dmy("30/10/2013")))) %>% 
  mutate(Inicio = dmy("18/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PROS 2903201") & (Inicio == dmy("30/10/2013"))))
org3 <- rbind(org3, org3_2, org3_3)
rm(org3_2, org3_3)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 4202073") & (Inicio == dmy("31/12/2015")))) %>% 
  mutate(Inicio = dmy("01/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 4202073") & (Inicio == dmy("31/12/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PDT 4217600") & (Inicio == dmy("31/12/2015")))) %>% 
  mutate(Inicio = dmy("01/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PDT 4217600") & (Inicio == dmy("31/12/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PSDB 5210208") & (Inicio == dmy("31/12/2015")))) %>% 
  mutate(Inicio = dmy("01/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PSDB 5210208") & (Inicio == dmy("31/12/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PT 5207501") & (Inicio == dmy("31/12/2015")))) %>% 
  mutate(Inicio = dmy("01/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PT 5207501") & (Inicio == dmy("31/12/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 4201406") & (Inicio == dmy("31/12/2015")))) %>% 
  mutate(Inicio = dmy("01/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 4201406") & (Inicio == dmy("31/12/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

org3_2 <- org3 %>% 
  filter(((Chave == "PTB 4203204") & (Inicio == dmy("31/12/2015")))) %>% 
  mutate(Inicio = dmy("01/01/2016"))
org3 <- org3 %>% 
  filter(!((Chave == "PTB 4203204") & (Inicio == dmy("31/12/2015"))))
org3 <- rbind(org3, org3_2)
rm(org3_2)

# refazer duracao dias  
org3 <- org3 %>% 
  mutate(Duracao_dias = difftime(Fim, Inicio, units = "days"))