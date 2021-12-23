#
#
# Pergunta de pesquisa o que teve maior correlação 
# na eleição de 2020(prefeito) dessa cidade catarinense
# mais de direita ? Aquela mais próxima no tempo (2018 presidente) ou
# para o mesmo cargo mas mais distante no tempo(prefetura 2016)
#
#Rio do Sul 2018 - somente na cidade (os 4 mais votados)
# --  primeiro turno
# Jair Bolsonaro (PSL) - 73,57% (no Brasil - 46%) 
# F. Haddad (PT) - 10,08% - (no Brasil -  29%) # 17% no segundo turno
# Ciro Gomes (PDT) - 5,29% - (no Brasil - 12%)
# João Amôedo (Novo) - 5,15% (no Brasil - 2%)
#
#

# Rio do Sul 2020 -
# José Thomé (PSD) - 38,14%
# Jaime Pasqualini (PODEMOS) - 29,56%
# Jean de Liz (PDT) - 16,88%
# Coronel Tonet (PSL) - 13,16%
# Clóvis Hoffman (Cidadania) - 2,26%

library(readxl)
pref_pres <- read_excel("pref_pres.xlsx")

#Isso é um exercício, correlação não é causalidade, pode ser espúria.

#Clóvis Hoffman - Cidadania
min(pref_pres$C.Hoffman)
max(pref_pres$C.Hoffman)
cor.test(pref_pres$C.Hoffman, pref_pres$Bolsonaro)
cor.test(pref_pres$C.Hoffman, pref_pres$Ciro)
cor.test(pref_pres$C.Hoffman, pref_pres$Haddad)
cor.test(pref_pres$C.Hoffman, pref_pres$Amoedo)

#Coronel Tonet - PSL
min(pref_pres$C.Tonet)
max(pref_pres$C.Tonet)
cor.test(pref_pres$C.Tonet, pref_pres$Bolsonaro)
cor.test(pref_pres$C.Tonet, pref_pres$Ciro)
cor.test(pref_pres$C.Tonet, pref_pres$Haddad)
cor.test(pref_pres$C.Tonet, pref_pres$Amoedo)

library(ggplot2)

a <- ggplot(pref_pres, aes(C.Tonet, Ciro))
a + geom_text(aes(label = Bairro))


#Jean de Liz - PDT
min(pref_pres$J.deLiz)
max(pref_pres$J.deLiz)
cor.test(pref_pres$J.deLiz, pref_pres$Bolsonaro)
cor.test(pref_pres$J.deLiz, pref_pres$Ciro)
cor.test(pref_pres$J.deLiz, pref_pres$Haddad)
cor.test(pref_pres$J.deLiz, pref_pres$Amoedo)

library(tidyverse)
Jean <- subset(pref_pres, select = c(J.deLiz, Bolsonaro,
                                     Haddad)) %>% na.omit()
matriz <- cor(Jean)

require(corrplot)
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

b <- ggplot(pref_pres, aes(J.deLiz, Bolsonaro))
b + geom_text(aes(label = Bairro))
b <- b+ geom_point()

b + geom_smooth(model = lm)

c <- ggplot(pref_pres, aes(J.deLiz, Haddad))
c + geom_text(aes(label = Bairro))

#Jaime Pasqualini - PODEMOS
min(pref_pres$J.Pasqualini)
max(pref_pres$J.Pasqualini)
cor.test(pref_pres$J.Pasqualini, pref_pres$Bolsonaro)
cor.test(pref_pres$J.Pasqualini, pref_pres$Ciro)
cor.test(pref_pres$J.Pasqualini, pref_pres$Haddad)
cor.test(pref_pres$J.Pasqualini, pref_pres$Amoedo)


Pasqualini <- subset(pref_pres, select = c(J.Pasqualini, 
                                           Ciro, Amoedo)) %>% na.omit()
matriz <- cor(Pasqualini)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

d <- ggplot(pref_pres, aes(J.Pasqualini, Ciro))
d + geom_text(aes(label = Bairro))

e <- ggplot(pref_pres, aes(J.Pasqualini, Amoedo))
e + geom_text(aes(label = Bairro))

# José Thomé
min(pref_pres$J.Thome)
max(pref_pres$J.Thome)
cor.test(pref_pres$J.Thome, pref_pres$Bolsonaro)
cor.test(pref_pres$J.Thome, pref_pres$Ciro)
cor.test(pref_pres$J.Thome, pref_pres$Haddad)
cor.test(pref_pres$J.Thome, pref_pres$Amoedo)

Thome <- subset(pref_pres, select = c(J.Thome, 
                                           Ciro, Bolsonaro)) %>% na.omit()
matriz <- cor(Thome)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

f <- ggplot(pref_pres, aes(J.Thome, Ciro))
f + geom_text(aes(label = Bairro))

g <- ggplot(pref_pres, aes(J.Thome, Bolsonaro))
g + geom_text(aes(label = Bairro))


# Um se relaciona com o outro?
prefeitura<- subset(pref_pres, select = c(J.Thome, J.Pasqualini,
                                              J.deLiz, C.Tonet,
                                              C.Hoffman))
matriz <- cor(prefeitura)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

presidencia <- subset(pref_pres, select = c(Amoedo, Bolsonaro,
                                            Ciro, Haddad))
matriz <- cor(presidencia)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)
