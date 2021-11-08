library(tidyverse)
library(readxl)
library(ggrepel)
library(patchwork)
library(factoextra)
library(cluster)
library(Rtsne)
library(ggridges)
library(webr)
library(RColorBrewer)
library(gridExtra)
library(dgof)

options(scipen = 999)

setwd("~/GitHub/trabalho_mi/bases")


# tratando dados de homicidios -------------------------------------------------

# extracao de dados usando codigo SQL do datalake que uso em projeto 

sim19 <- sqlQuery(channel, "SELECT TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO,
                                CODMUNRES, CAUSABAS, uf, competencia  
                                FROM Dados.sim.DO
                                WHERE competencia = 2019")


sim19_tratado <- sim19 %>% 
                    mutate(cat_causa = str_sub(CAUSABAS, end = 3)) %>% 
                    filter(str_detect(cat_causa, "^X8") |
                           str_detect(cat_causa, "^X9") |
                           cat_causa == "Y35" | cat_causa == "Y36") %>% 
                    group_by(CODMUNRES, uf) %>% 
                    summarise(total = n()) %>% 
                    janitor::clean_names()


# subindo bases -----------------------------------------------------------

munic <- read_excel("munic.xlsx", col_types = c("text", 
                                                "numeric", "text", "text", "text", "text", 
                                                "text", "numeric", "numeric", "numeric", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "numeric", "numeric"))

sim19_tratado <- read_csv("sim19.csv", col_types = cols(codmunres = col_character())) %>% 
                select(-X1) %>% 
                mutate(codmunres = as.character(codmunres))

pop_masculina <- read_delim("pop_masculina.csv", 
                            ";", escape_double = FALSE, col_types = cols(cod_ibge = col_character()), 
                            locale = locale(encoding = "ISO-8859-1", 
                                            asciify = TRUE), trim_ws = TRUE)

pop_residente <- read_delim("pop_residente.csv", 
                            ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1", 
                                                                        asciify = TRUE), trim_ws = TRUE) %>% 
                 mutate(cod_ibge = as.character(cod_ibge))

pib2018 <- read_delim("pib2018.csv", ";", 
                      escape_double = FALSE, locale = locale(encoding = "ISO-8859-1", 
                                                             asciify = TRUE), trim_ws = TRUE) %>% 
                      mutate(id_municipio = as.character(id_municipio))

ideb <- read_delim("ideb.csv", ";", escape_double = FALSE, 
                   trim_ws = TRUE)

ideb_tratado <- ideb %>% 
                  group_by(ano, sigla_uf, id_municipio, anos_escolares) %>% 
                  summarise(ideb_finais_fund = mean(ideb, na.rm = TRUE)) %>%
                  ungroup() %>% 
                  mutate(id_municipio = as.character(id_municipio)) %>% 
                  select(id_municipio, ideb_finais_fund) 


# Juntando bases ----------------------------------------------------------

base_tratada <- munic %>% 
                  mutate(ibge6 = str_sub(CodMun, end = 6)) %>% 
                  left_join(sim19_tratado, by = c("ibge6" = "codmunres")) %>% 
                  left_join(ideb_tratado, by = c("CodMun"="id_municipio")) %>% 
                  left_join(pib2018, by = c("CodMun"="id_municipio")) %>% 
                  left_join(pop_masculina, by = c("ibge6" = "cod_ibge")) %>% 
                  left_join(pop_residente, by = c("ibge6" = "cod_ibge")) %>% 
                  select(regiao, cod_uf, CodMun, uf.x, nome_munic, classe_pop, 
                         porte_municipio, porte_codigo, comarca, guarda_municipal, 
                         direitos_humanos,prot_violencia_domestica,
                         prot_vit_indiretas_violencia,prot_viol_jovens,enfrent_racismo, 
                         prot_conflitos_territoriais,prot_lgbtfobia, pro_intol_religiosa, 
                         contagem_politicas, ideb_finais_fund, pib, impostos_liquidos, 
                         pop_masc19, pop_2019, total) %>%  
                  rename(uf = uf.x, total_homicidios = total) %>%  
                  mutate(perc_pop_masc = (pop_masc19/pop_2019)*100,
                         taxa_homicid = (total_homicidios/pop_2019)*1000) %>% 
                  filter(ideb_finais_fund != "NaN") %>% 
                  mutate(pib_log = log(pib)) %>% 
                  mutate(homicid_log = log(taxa_homicid)) %>% 
                  filter(comarca != "Recusa") 
                  

base_tratada$taxa_homicid[is.na(base_tratada$taxa_homicid)] <- 0 
                  
# Analise exploratoria 

base_tratada %>%  
  select(pib_log, contagem_politicas, ideb_finais_fund, 
         perc_pop_masc, homicid_log) %>%
  GGally::ggpairs()


base_tratada %>% 
  ggplot(aes(x = comarca , y = taxa_homicid, fill = regiao)) + geom_boxplot() + 
  theme_minimal() + ggtitle("Taxa de homicídio por municípios com sede de comarca") + 
  xlab("Presença de comarca") + ylab("Taxa de homicídio")

base_tratada %>% 
  ggplot(aes(x = guarda_municipal , y = taxa_homicid, fill = regiao)) + geom_boxplot() + 
  theme_minimal() + ggtitle("Taxa de homicídio por municípios com guarda municipal") + 
  xlab("Presença de Guarda Municipal") + ylab("Taxa de homicídio") 


base_tratada %>% 
  ggplot(aes(x = porte_municipio , y = taxa_homicid, 
             fill = regiao)) + geom_boxplot() + 
  theme_minimal() + ggtitle("Taxa de homicídio por porte de município") + 
  xlab("Porte do município") + ylab("Taxa de homicídio") 


base_tratada %>% 
  ggplot(aes(x = ideb_finais_fund, y = taxa_homicid, 
             size = pop_2019, col = regiao)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + facet_grid(~regiao) + xlab("Nota IDEB") +
  ylab("Taxa de homicídio") + ggtitle("Taxas de homicídio e escolaridade por região") +
  geom_text_repel(aes(label = nome_munic), size = 3)  


base_tratada %>% 
  ggplot(aes(x = pib_log, y = taxa_homicid, 
             size = pop_2019, col = regiao)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + facet_grid(~regiao) + xlab("log(PIB)") +
  ylab("Taxa de homicídio") + ggtitle("Taxas de homicídio e log(PIB) por região") +
  geom_text_repel(aes(label = nome_munic), size = 3)  


base_tratada %>% 
  ggplot(aes(x = perc_pop_masc, y = taxa_homicid, 
             size = pop_2019, col = regiao)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + facet_grid(~regiao) + xlab("Percentual de população masculina") +
  ylab("Taxa de homicídio") + ggtitle("Taxas de homicídio e Percentual de população masculina por região") +
  geom_text_repel(aes(label = nome_munic), size = 3)  



# Clusterização -----------------------------------------------------------

set.seed(123)

base_cluster <- base_tratada %>% 
  select(CodMun, porte_codigo , pib_log, contagem_politicas, ideb_finais_fund, 
         perc_pop_masc, guarda_municipal, comarca, direitos_humanos, 
         taxa_homicid) %>% 
  mutate(guarda_municipal = as.factor(guarda_municipal),
         direitos_humanos = as.factor(direitos_humanos)) %>% 
  remove_rownames() %>%
  column_to_rownames(var = 'CodMun') 


gower_dist <- daisy(base_cluster,
                    metric = "gower",
                    type = list(logratio = 3))


summary(gower_dist)

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

numero_clusters <- data.frame(1:10, sil_width) %>%  
                        rename(n_clusters = X1.10,
                               silhouette = sil_width) %>% 
                    ggplot(aes(x = n_clusters, y = silhouette)) + 
                    geom_point() + geom_line() + theme_minimal() +
                    scale_x_continuous(breaks = seq(1,10,1)) + 
                    theme(axis.text = element_text(size = 14)) + 
                    xlab("Número de clusters") + ylab("Silhueta")

numero_clusters

# Plot sihouette width (higher is better)
numero_clusters <- plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit5 <- pam(gower_dist, diss = TRUE, k = 5)
pam_fit3 <- pam(gower_dist, diss = TRUE, k = 2)

pam_results <- base_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data3 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit3$clustering))

tsne_data5 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit5$clustering))

grafico3 <- ggplot(aes(x = X, y = Y), data = tsne_data3) +
  geom_point(aes(color = cluster)) + theme_minimal() + 
  ggtitle("Três clusters")

grafico5 <- ggplot(aes(x = X, y = Y), data = tsne_data5) +
  geom_point(aes(color = cluster)) + theme_minimal() + 
  ggtitle("Cinco clusters")

numero_clusters + (grafico3 / grafico5)


tsne_data

clusters <- as.data.frame(pam_fit5[["clustering"]])

base_com_clusters <- cbind(base_cluster,clusters) %>% 
                        rename(cluster = `pam_fit5[["clustering"]]`) 


base_com_clusters <- cbind(cod_ibge = rownames(base_com_clusters), base_com_clusters)


munic_red <- munic %>% 
                select(CodMun, regiao, nome_munic, )

base_tratada_red <- base_tratada %>% 
                      select(CodMun, pop_2019)


base_com_clusters <- base_com_clusters %>% 
  left_join(munic_red, by = c("cod_ibge"="CodMun")) %>% 
  left_join(base_tratada_red, by = c("cod_ibge"="CodMun"))

glimpse(base_com_clusters)

# Explorando clusters -----------------------------------------------------

descritiva_clusters <- base_com_clusters %>% 
  group_by(cluster) %>% 
  summarise(taxa_homici = round(mean(taxa_homicid),2),
            pib_log = round(mean(pib_log),2), 
            contagem_politicas = round(mean(contagem_politicas),2),
            ideb_finais_fund = round(mean(ideb_finais_fund),2)) %>% 
  rename(políticas = contagem_politicas, ideb = ideb_finais_fund, 
         homicídios = taxa_homici, `PIB(log)` = pib_log) 


descritiva_cluster_regiao <- base_com_clusters %>% 
  group_by(cluster, regiao) %>% 
  summarise(total = n()) %>% 
  mutate(freq = 100 * (total / sum(total)))

# base_com_clusters %>% 
#   group_by(cluster) %>% 
#   summarise(total = n()) %>% 
#   mutate(freq = 100 * (total / sum(total))) %>% 
#   PieDonut(aes(cluster, count = freq), color = "black",
#            title = "Clusters", showPieName = FALSE)
# 
# 
# base_com_clusters %>% 
#   mutate(porte_codigo = case_when(porte_codigo == "1" ~ "Até 20 mil hab.",
#                                   porte_codigo == "2" ~ "Entre 20 e 100 mil hab.",
#                                   porte_codigo == "3" ~ "Entre 100 e 500 mil hab.",
#                                   porte_codigo == "4" ~ "Acima de 500 mil hab.")) %>% 
#   group_by(cluster,porte_codigo) %>% 
#   summarise(total = n()) %>% 
#   mutate(freq = 100 * (total / sum(total))) %>% 
#   treemap::treemap(index = c("cluster","porte_codigo"),
#                    vSize = "freq", type = "index",
#                    align.labels=list(
#                      c("center", "center"), 
#                      c("right", "bottom")),
#                    title = "Distribuição por porte do município",
#                    fontsize.labels = 30)


imagem_clust_porte <- base_com_clusters %>% 
  mutate(porte_codigo = case_when(porte_codigo == "1" ~ "Até 20 mil hab.",
                                  porte_codigo == "2" ~ "Entre 20 e 100 mil hab.",
                                  porte_codigo == "3" ~ "Entre 100 e 500 mil hab.",
                                  porte_codigo == "4" ~ "Acima de 500 mil hab.")) %>% 
  group_by(cluster,porte_codigo) %>% 
  summarise(total = n()) %>% 
  mutate(freq = (total / sum(total))) %>% 
  rename(Porte = porte_codigo) %>% 
  ggplot(aes(x = cluster, y = freq, fill = Porte)) + 
  geom_col() + theme_minimal() + coord_flip() + ylab("Frequência") + 
  theme(text = element_text(size = 25)) + ggtitle("Porte do município")

imagem_clust_regiao <- base_com_clusters %>% 
  group_by(cluster, regiao) %>% 
  summarise(total = n()) %>% 
  mutate(freq =  (total / sum(total))) %>% 
  rename(Região = regiao) %>% 
  ggplot(aes(x = cluster, y = freq, fill = Região)) + 
  geom_col() + theme_minimal() + coord_flip() + ylab("Frequência")+ 
  theme(text = element_text(size = 25)) + ggtitle("Região do município")


img_gcm <- base_com_clusters %>% 
  group_by(cluster,guarda_municipal) %>% 
  count() %>% 
  mutate(`Guarda Municipal` = case_when(guarda_municipal == "0" ~ "Não",
                                        guarda_municipal == "1" ~ "Sim")) %>% 
  ggplot(aes(x = cluster, y = n, fill = `Guarda Municipal`)) + geom_col(position = "fill") + 
  theme_minimal() + coord_flip() + ggtitle("Guarda Municipal") + 
  theme(text = element_text(size = 20)) + ylab("Frequência")

img_direitos <- base_com_clusters %>% 
  group_by(cluster,direitos_humanos) %>% 
  count() %>% 
  mutate(`Direitos humanos` = case_when(direitos_humanos == "0" ~ "Não",
                                        direitos_humanos == "1" ~ "Sim")) %>% 
  ggplot(aes(x = cluster, y = n, fill = `Direitos humanos`)) + geom_col(position = "fill") + 
  theme_minimal() + coord_flip() + ggtitle("Direitos Humanos") + 
  theme(text = element_text(size = 20)) + ylab("Frequência")

img_comarca <- base_com_clusters %>% 
  group_by(cluster,comarca) %>% 
  count() %>% 
  mutate(Comarca = case_when(comarca == "0" ~ "Não",
                             comarca == "1" ~ "Sim")) %>% 
  ggplot(aes(x = cluster, y = n, fill = Comarca)) + geom_col(position = "fill") + 
  theme_minimal() + coord_flip() + ggtitle("Comarca") + 
  theme(text = element_text(size = 20)) + ylab("Frequência")


(imagem_clust_porte + imagem_clust_regiao) / (img_gcm + img_comarca + img_direitos)




# 
# base_com_clusters %>% 
#   group_by(cluster, regiao) %>% 
#   summarise(total = n()) %>% 
#   mutate(freq = 100 * (total / sum(total))) %>% 
#   treemap::treemap(index = c("cluster","regiao"),
#                    vSize = "freq")

img_homicid <- base_com_clusters %>% 
  ggplot(aes(x = taxa_homicid, y = factor(cluster), fill = cluster)) +
  geom_boxplot() + theme_minimal() + guides(fill = FALSE) +
  theme_ridges() + ylab("Cluster") + 
  ggtitle("Taxa de homicídios") + theme(axis.title.x = element_blank())

img_masc <- base_com_clusters %>% 
  ggplot(aes(x = perc_pop_masc, y = factor(cluster), fill = cluster)) +
  geom_boxplot() + theme_minimal() + guides(fill = FALSE) +
  theme_ridges() + ylab("Cluster") + ggtitle("Percentual masculino entre 18 e 30 anos") +
  theme(axis.title.x = element_blank())

img_ideb <- base_com_clusters %>% 
  ggplot(aes(x = ideb_finais_fund, y = factor(cluster), fill = cluster)) +
  geom_boxplot() + theme_minimal() + guides(fill = FALSE) +
  theme_ridges() + ylab("Cluster") + ggtitle("Nota do IDEB") +
  theme(axis.title.x = element_blank())

img_pib <- base_com_clusters %>% 
  ggplot(aes(x = pib_log, y = factor(cluster), fill = cluster)) +
  geom_boxplot() + theme_minimal() + guides(fill = FALSE) +
  theme_ridges() + ylab("Cluster") + ggtitle("PIB (log)") + 
  theme(axis.title.x = element_blank())

img_politicas <- base_com_clusters %>% 
  ggplot(aes(x = contagem_politicas, y = factor(cluster), fill = cluster)) +
  geom_boxplot() + theme_minimal() + guides(fill = FALSE) +
  theme_ridges() + ylab("Cluster") + ggtitle("Contagem políticas") + 
  theme(axis.title.x = element_blank())


(img_homicid + grid.arrange(top = "Média de variáveis",tableGrob(descritiva_clusters,rows=NULL))) / (img_ideb + img_pib) / (img_politicas + img_masc)




# Modelagem ---------------------------------------------------------------

cluster1 <- base_com_clusters %>% 
                filter(cluster == "1")

cluster1$comarca <- as.factor(cluster1$comarca)

modelo1 <- lm(taxa_homicid ~ pib_log + contagem_politicas + 
                            ideb_finais_fund + perc_pop_masc + pop_2019 +
                            #comarca + guarda_municipal + 
                direitos_humanos, cluster1)

summary(modelo1)

gvlma::gvlma(modelo1)


cluster2 <- base_com_clusters %>% 
  filter(cluster == "2")

modelo2 <- lm(taxa_homicid ~ pib_log + contagem_politicas + 
                ideb_finais_fund + perc_pop_masc + pop_2019 + 
                #comarca + guarda_municipal 
                + direitos_humanos, cluster2)

summary(modelo2)

gvlma::gvlma(modelo2)


cluster3 <- base_com_clusters %>% 
  filter(cluster == "3")

modelo3 <- lm(taxa_homicid ~ pib_log + contagem_politicas + 
                ideb_finais_fund + perc_pop_masc + pop_2019 +
                #comarca + guarda_municipal + 
                direitos_humanos, cluster3)

summary(modelo3)

gvlma::gvlma(modelo3)

cluster4 <- base_com_clusters %>% 
  filter(cluster == "4")

modelo4 <- lm(taxa_homicid ~ pib_log + contagem_politicas + 
                ideb_finais_fund + perc_pop_masc + pop_2019 +
                #comarca + guarda_municipal + 
                direitos_humanos, cluster4)

summary(modelo4)
gvlma::gvlma(modelo4)


cluster5 <- base_com_clusters %>% 
  filter(cluster == "5")

modelo5 <- lm(taxa_homicid ~ pib_log + contagem_politicas + 
                ideb_finais_fund + perc_pop_masc + pop_2019 + 
                #comarca + guarda_municipal + 
                direitos_humanos, cluster5)

summary(modelo5)
gvlma::gvlma(modelo5)


# Testando pressupostos  --------------------------------------------------

# normalidade

resultados_modelo1 <- broom::augment(modelo1)

dgof::ks.test(resultados_modelo1$.resid, "pnorm")

resultados_modelo2 <- broom::augment(modelo2)

dgof::ks.test(resultados_modelo2$.resid, "pnorm")

resultados_modelo3 <- broom::augment(modelo3)

dgof::ks.test(resultados_modelo3$.resid, "pnorm")

resultados_modelo4 <- broom::augment(modelo4)

dgof::ks.test(resultados_modelo4$.resid, "pnorm")

resultados_modelo5 <- broom::augment(modelo5)

dgof::ks.test(resultados_modelo5$.resid, "pnorm")


# homocedasticidade 

lmtest::bptest(modelo1)
lmtest::bptest(modelo2)
lmtest::bptest(modelo3)
lmtest::bptest(modelo4)
lmtest::bptest(modelo5)

# multicolinearidade

car::vif(modelo1)
car::vif(modelo2)
car::vif(modelo3)
car::vif(modelo4)
car::vif(modelo5)
