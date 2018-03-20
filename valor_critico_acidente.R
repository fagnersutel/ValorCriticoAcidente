#tipos de Acidentes
acidente <- c("Materiais", "Feridos", "Pedestres", "Fatais")
#peso dos acidentes
nivel_acidente <- c(1, 4, 6, 13)
#UPS ponderação conforme gravidade do acidente
ups <- cbind(nivel_acidente, acidente)
head(ups)
#criamos registros fake
registros <- rep(c(1,4,6,13), c(sample(1000:5000, 1), sample(500:1500, 1), sample(500:1000, 1), sample(50:200, 1))) 
registros <- as.data.frame(registros)
dim(registros)
conta <- c(sum(registros[,1] == 1), sum(registros[,1] == 4), sum(registros[,1] == 6), sum(registros[,1] == 13))
conta <- cbind(conta, ups)
conta
#setamos o numerio de cruzamentos
ncruz = 350
#obtemos o indicede acidente  medio ponderado
ip <- sum(registros) / ncruz
ip
#definimos K para um nível de confiança de 95%  = 1.645, 99,5% = 2.576 e para 90% = 1.282
k <- 1.645
exposicao <- c(1:5000)
exposicao <- as.data.frame(exposicao)
summary(exposicao)


exposicao$ic <- 0
exposicao$exposicao <- as.numeric(exposicao$exposicao)
exposicao$ic <- ip + k * ( sqrt(ip / exposicao$exposicao) - (0.5 / exposicao$exposicao) )
library(ggplot2)
ggplot(exposicao, aes(x=exposicao, y=ic)) +
  geom_line(colour="red", linetype="dashed", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")


# p = periodo
# a = numerode acidentes
# v = vol medio diario
# t = numero de acidentes por milhoes de veiculos
# t = ( a *10^6 ) / ( p * v )


#
#criamos registros fake
registros <- rep(c(1,4,6,13), c(sample(1:80, 1), sample(1:50, 1), sample(1:30, 1), sample(1:5, 1))) 
registros <- as.data.frame(registros)
dim(registros)
conta <- c(sum(registros[,1] == 1), sum(registros[,1] == 4), sum(registros[,1] == 6), sum(registros[,1] == 13))
conta <- cbind(conta, ups)
conta
#setamos o numerio de cruzamentos
ncruz = 1
#obtemos o indicede acidente  medio ponderado
ip <- sum(registros) / ncruz
ip
#definimos K para um nível de confiança de 95%  = 1.645, 99,5% = 2.576 e para 90% = 1.282
k <- 1.645
exposicao <- c(1:2000)
exposicao <- as.data.frame(exposicao)
summary(exposicao)


exposicao$ic <- 0
exposicao$exposicao <- as.numeric(exposicao$exposicao)
exposicao$ic <- ip + k * ( sqrt(ip / exposicao$exposicao) - (0.5 / exposicao$exposicao) )
library(ggplot2)
ggplot(exposicao, aes(x=exposicao, y=ic)) +
  geom_line(colour="red", linetype="dashed", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")

