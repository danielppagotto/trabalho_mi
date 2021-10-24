library(tidyverse)

sim19 <- sqlQuery(channel, "SELECT TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO,
                                CODMUNRES, CAUSABAS, uf, competencia  
                                FROM Dados.sim.DO
                                WHERE competencia = 2019")
