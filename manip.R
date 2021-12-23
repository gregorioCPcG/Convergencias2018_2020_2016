library(data.table)
marco <- fread("bweb_1t_SC_101020182028.csv")
library(tidyverse)

#### MANIPULACAO #####

z26 <- marco %>%
  filter(NM_MUNICIPIO == "RIO DO SUL")
table(z26$NM_MUNICIPIO)
table(z26$NR_LOCAL_VOTACAO)
#rm(marco)
str(z26)
table(z26$DS_CARGO_PERGUNTA)
table(z26$NR_TURNO)
z26 <- subset(z26, select = c(NR_LOCAL_VOTACAO,DS_CARGO_PERGUNTA,
                                        SG_PARTIDO, QT_VOTOS, NM_VOTAVEL)) %>% na.omit()
str(z26)
library(memisc)

z26$bairro <- recode(z26$NR_LOCAL_VOTACAO, "Centro" <- c(1015, 1023), "Sumare" <- c(1040),"Budag" <-c(1058,1279), "Santana" <-c(1066),
                     "Barra_Trombudo" <- c(1074), "Boa Vista" <- c(1082), "Laranjeiras"<-c(1090), "Progresso"<-c(1104,1287), "Bela Ali"
                     <- c(1112, 1120), "Barra_d_Itoupava" <-c(1139), "Fundo-Canoas" <-c(1147), "Canta_Galo"<-c(1155,1236), 
                     "Valada_Itoupava" <- c(1163), "Valada_Sao_Paulo" <- c(1171), "Taboao" <-c(1180), "Albertina" <-c(1198),
                     "Alto_Itoupava" <- c(1201), "Santa-Rita" <- c(1228), "Barra_Taboao" <-c(1244), "Barragem" <-c(1252),
                     "Serra_Taboao" <-c(1260),"Jardim_Alex" <-c(1295), "Navegantes" <-c(1309), "Rainha" <-c(1317))

detach("package:memisc", unload = TRUE)
table(z26$bairro)


presi <- z26%>%
  filter(DS_CARGO_PERGUNTA == "Presidente")
table(presi$bairro, presi$NM_VOTAVEL)



library(memisc)
table(presi$NM_VOTAVEL)
presi$Candidatos <- recode(presi$NM_VOTAVEL, "Bolsonaro" <-c("JAIR BOLSONARO"), "Haddad"<-c("FERNANDO HADDAD"),
                           "Amoedo" <-c("JOÃO AMOÊDO"),"Outros"<-c("ALVADO DIAS", "CABO DACIOLO", "EYMAEL",
                                                                   "GUILHERME BOULOS", "HENRIQUE MEIRELLES","MARINA SILVA",
                                                                   "JOÃO GOULART FILHO", "VERA"),
                           "Ciro" <-c("CIRO GOMES"), "Alckmin"<-c("GERALDO ALCKMIN"),"Branco_Nulo" <-c("Branco","Nulo"))
detach("package:memisc", unload = TRUE)

library(knitr)
library(kableExtra)
#colocar nome do bairro para geral lista para construção de arquivo de dados votaçao presidencial por bairro
tes <- presi%>%
  filter(bairro == "Rainha")

b5 <- tes %>% dplyr::select(QT_VOTOS, Candidatos) %>% arrange(desc(Candidatos))
b5 %>%kbl(caption = "") %>%kable_classic(full_width = F, html_font = "Garamond")

# anali #####
#
library(readxl)
dados <- read_excel("dados.xls")
dados$Candidato <-as.factor(dados$Candidato)
dados$Bairro <- as.factor(dados$Bairro)
aggregate(dados[, 5], list(dados$Candidato), mean)#USAR


bolsonaro <- dados%>%
  filter(Candidato == "Bolsonaro")
boxplot(bolsonaro$votos)


# boxplot com 7 fatores

ggplot(data = dados, mapping = aes(x = Candidato, y = votos)) +
  geom_boxplot(fill='#465549', color="black") + 
  xlab("Candidato") + ylab("distribuição de % votos nos bairros de Rio do Sul") 
# boplot com 2 fatores (bozo x outros, recod primeiro)


dados2 <- read_excel("dados2.xlsx")
ggplot(data = dados2, mapping = aes(x = Voto, y = perc)) +
  geom_boxplot(fill='#465549', color="black") + 
  xlab("Candidato") + ylab("distribuição de % votos nos bairros de Rio do Sul")


aggregate(dados2[, 3], list(dados2$Voto), mean)#USAR
summary(bolsonaro$votos)
antibolsonaro <- dados2%>%
  filter(Voto == "Outro_Branco_Nulo")
summary(antibolsonaro$perc)

bolsonaro$cont <- 1
g <- ggplot(bolsonaro, aes(votos,cont))
g + geom_boxplot(fill='#465549', color="black") + coord_flip() + geom_text(aes(label =Bairro))

#graficos arquivos excel ver

dados <- dados %>%
  mutate(Bolsonaro = case_when(Candidato == "Bolsonaro" ~ 1,
                             TRUE ~0)) %>%
  mutate(Amoedo = case_when(Candidato == "Amoedo" ~ 1,
                           TRUE ~0)) %>%
  mutate(Alckmin = case_when(Candidato == "Alckmin" ~ 1,
                           TRUE ~0)) %>%
  mutate(Haddad = case_when(Candidato == "Haddad" ~ 1,
                              TRUE ~0)) %>%
  mutate(Branco_Nulo = case_when(Candidato == "Branco_Nulo" ~ 1,
                            TRUE ~ 0)) %>%
  mutate(Ciro = case_when(Candidato == "Ciro" ~ 1,
                               TRUE ~0))


Amoedo <- dados%>%
  filter(Amoedo == "1")
Alckmin <- dados%>%
  filter(Alckmin == "1")
Hadddad <- dados%>%
  filter(Haddad == "1")
Ciro <- dados%>%
  filter(Ciro == "1")
Branco_Nulo <- dados%>%
  filter(Branco_Nulo =="1")
cor.test(Amoedo$votos, bolsonaro$votos)
cor.test(Alckmin$votos, bolsonaro$votos)
cor.test(Branco_Nulo$votos, bolsonaro$votos)
cor.test(Branco_Nulo$votos, Hadddad$votos)
cor.test(bolsonaro$votos, Hadddad$votos)
cor.test(Ciro$votos, Hadddad$votos)
cor.test(Ciro$votos, bolsonaro$votos)
cont <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
base <- data.frame(cont)
base$Ciro <- Ciro$votos
base$Branco_Nulo <- Branco_Nulo$votos
base$Bolsonaro <- bolsonaro$votos
base$Haddad <- Hadddad$votos
base$Amoedo <- Amoedo$votos
base$Alckmin <- Alckmin$votos
base$Bairro <-  Amoedo$Bairro

base2 <-  subset(base, select = c(Bairro, Branco_Nulo, Ciro, Bolsonaro, Haddad, Amoedo, Alckmin)) %>% na.omit()
base <- subset(base, select = c(Branco_Nulo, Ciro, Bolsonaro, Haddad, Amoedo, Alckmin)) %>% na.omit()

#iniciando a análise fatorial
matriz <- cor(base)

require(corrplot)
corrplot(matriz, method="shade", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)


base3 <- subset(base2, select = c(Bairro,Ciro, Bolsonaro, Haddad, Amoedo)) %>% na.omit()
library(writexl)
write_xlsx(base3, "D:/ATUALIZA_PASTA_d/Novo RSL/merge22.xlsx" )
# acrescendo prefeitura 2020 # novo file
