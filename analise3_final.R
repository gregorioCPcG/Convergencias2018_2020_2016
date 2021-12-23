library(readxl)
final <- read_excel("total geral_analise3.xlsx")

# Clóvis Hoffman - quinto colocado
# sem significância

# Coronel Tonet - quarto colocado
Coronel <- lm(C.Tonet2020 ~ Bolsonaro2018 + Ciro2018 + J.Thome2016, 
              data = final)
summary(Coronel)
library(coefplot)
coefplot(Coronel, intercept=FALSE)


# Jean de Liz - terceiro colocado
cor(final$J.deLiz2016, final$Haddad2018)
cor(final$Bolsonaro2018, final$Haddad2018)
Jean <- lm(J.deLiz2020 ~ Haddad2018 + J.deLiz2016, data=final)
summary(Jean)
coefplot(Jean, intercept=FALSE)


#Jaime Pasqualini - segundo colocado
cor(final$Ciro2018, final$Amoedo2018)
Jaime <- lm(J.Pasqualini2020 ~ Ciro2018 + Amoedo2018, data=final)
summary(Jaime)
coefplot(Jaime, intercept = FALSE)


#J.Thomé - primeiro colocado
cor(final$J.Thome2016, final$Bolsonaro2018)
Thome <- lm(J.Thome2020 ~ Bolsonaro2018 + Ciro2018 + J.Thome2016,
            data = final)
summary(Thome)


library(huxtable)
huxreg(Thome, Jaime, Jean, Coronel, stars = c(`*` = 0.1, `**` = 0.05,
                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                       "AIC" = "AIC"))
