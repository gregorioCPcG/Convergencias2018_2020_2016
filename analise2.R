###Rio do Sul 2016 x Rio do Sul 2020

# Rio do Sul 2016 - 
# José Thomé (PSDB) - 47,67%
# Gariba (PMDB) - 39.95%
# Jean de Liz (PT) - 12.37%

# Rio do Sul 2020 -
# José Thomé (PSD) - 38,14%
# Jaime Pasqualini (PODEMOS) - 29,56%
# Jean de Liz (PDT) - 16,88%
# Coronel Tonet (PSL) - 13,16%
# Clóvis Hoffman (Cidadania) - 2,26%

# Clóvis Hofman # em 2018 não teve correlação
library(readxl)
prefa16_20 <- read_excel("prefa16_20.xlsx")
cor.test(prefa16_20$C.Hoffman2020, prefa16_20$Gariba2016)
cor.test(prefa16_20$C.Hoffman2020, prefa16_20$J.Thome2016)
cor.test(prefa16_20$C.Hoffman2020, prefa16_20$J.deLiz2016)

#Coronel Tonet # em 2018 - correlacionou com Ciro
cor.test(prefa16_20$C.Tonet2020, prefa16_20$Gariba2016)
cor.test(prefa16_20$C.Tonet2020, prefa16_20$J.Thome2016)
cor.test(prefa16_20$C.Tonet2020, prefa16_20$J.deLiz2016)

library(ggplot2)

a <- ggplot(prefa16_20, aes(C.Tonet2020, J.Thome2016))
a + geom_text(aes(label = Bairro))



#Jean de Liz # em 2018 correlacionou com Bolsonaro e Haddad
cor.test(prefa16_20$J.deLiz2020, prefa16_20$Gariba2016)
cor.test(prefa16_20$J.deLiz2020, prefa16_20$J.Thome2016)
cor.test(prefa16_20$J.deLiz2020, prefa16_20$J.deLiz2016)

b <- ggplot(prefa16_20, aes(J.deLiz2020, J.deLiz2016))
b + geom_text(aes(label = Bairro))


# Jaime Pasqualini # em 2018 correlacionou com Ciro e Amoedo
cor.test(prefa16_20$J.Pasqualini2020, prefa16_20$Gariba2016)
cor.test(prefa16_20$J.Pasqualini2020, prefa16_20$J.Thome2016)
cor.test(prefa16_20$J.Pasqualini2020, prefa16_20$J.deLiz2016)


#José Thomé # em 2018 correlacionou com Ciro e Bolsonaro
cor.test(prefa16_20$J.Thome2020, prefa16_20$Gariba2016)
cor.test(prefa16_20$J.Thome2020, prefa16_20$J.Thome2016)
cor.test(prefa16_20$J.Thome2020, prefa16_20$J.deLiz2016)

c <- ggplot(prefa16_20, aes(J.Thome2020, J.Thome2016))
c + geom_text(aes(label = Bairro))


d <- ggplot(prefa16_20, aes(J.Thome2020, J.Pasqualini2020))
d + geom_text(aes(label = Bairro))
