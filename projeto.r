library("Rlab")
library(zoo)
library(dplyr)
setwd("/Users/feliperamos/Documents/pro3200/projeto/projEstat/")
dados <- read.csv(file="metais.csv")
dadosInt <- mutate(dados, Paladio = na.approx(Paladio))
dadosInt <- mutate(dadosInt, Platina = na.approx(Platina))

preGuerra <- dadosInt[1:115,]
posGuerra <- dadosInt[116:180,]

mediaPreGuerraPaladio = mean(preGuerra$Paladio)
mediaPreGuerraPlatina = mean(preGuerra$Platina)
mediaPosGuerraPaladio = mean(posGuerra$Paladio)
mediaPosGuerraPlatina = mean(posGuerra$Platina)

varPreGuerraPaladio = var(preGuerra$Paladio)
varPreGuerraPlatina = var(preGuerra$Platina)
varPosGuerraPaladio = var(posGuerra$Paladio)
varPosGuerraPlatina = var(posGuerra$Platina)

medianaPreGuerraPaladio = median(preGuerra$Paladio)
medianaPreGuerraPlatina = median(preGuerra$Platina)
medianaPosGuerraPaladio = median(posGuerra$Paladio)
medianaPosGuerraPlatina = median(posGuerra$Platina)

amplitudePreGuerraPaladio = max(preGuerra$Paladio) - min(preGuerra$Paladio)
amplitudePreGuerraPlatina = max(preGuerra$Platina) - min(preGuerra$Platina)
amplitudePosGuerraPaladio = max(posGuerra$Paladio) - min(posGuerra$Paladio)
amplitudePosGuerraPlatina = max(posGuerra$Platina) - min(posGuerra$Platina)

tPreGuerra = qt(0.975, dim(preGuerra)[1]-1)
tPosGuerra = qt(0.975, dim(posGuerra)[1]-1)

XlowPreGuerraPaladio = mediaPreGuerraPaladio - tPreGuerra*sqrt(varPreGuerraPaladio/dim(preGuerra)[1])
XhighPreGuerraPaladio = mediaPreGuerraPaladio + tPreGuerra*sqrt(varPreGuerraPaladio/dim(preGuerra)[1])

XlowPreGuerraPlatina = mediaPreGuerraPlatina - tPreGuerra*sqrt(varPreGuerraPlatina/dim(preGuerra)[1])
XhighPreGuerraPlatina = mediaPreGuerraPlatina + tPreGuerra*sqrt(varPreGuerraPlatina/dim(preGuerra)[1])

XlowPosGuerraPaladio = mediaPosGuerraPaladio - tPosGuerra*sqrt(varPosGuerraPaladio/dim(posGuerra)[1])
XhighPosGuerraPaladio = mediaPosGuerraPaladio + tPosGuerra*sqrt(varPosGuerraPaladio/dim(posGuerra)[1])

XlowPosGuerraPlatina = mediaPosGuerraPlatina - tPosGuerra*sqrt(varPosGuerraPlatina/dim(posGuerra)[1])
XhighPosGuerraPlatina = mediaPosGuerraPlatina + tPosGuerra*sqrt(varPosGuerraPlatina/dim(posGuerra)[1])



bPreGuerra = qchisq(0.025, dim(preGuerra)[1]-1)
aPreGuerra = qchisq(0.975, dim(preGuerra)[1]-1)
bPosGuerra = qchisq(0.025, dim(posGuerra)[1]-1)
aPosGuerra = qchisq(0.925, dim(posGuerra)[1]-1)

SlowPreGuerraPaladio = (dim(preGuerra)[1]-1)*varPreGuerraPaladio/bPreGuerra
ShighPreGuerraPaladio = (dim(preGuerra)[1]-1)*varPreGuerraPaladio/aPreGuerra
SlowPreGuerraPlatina = (dim(preGuerra)[1]-1)*varPreGuerraPlatina/bPreGuerra
ShighPreGuerraPlatina = (dim(preGuerra)[1]-1)*varPreGuerraPlatina/aPreGuerra

SlowPosGuerraPaladio = (dim(posGuerra)[1]-1)*varPosGuerraPaladio/bPosGuerra
ShighPosGuerraPaladio = (dim(posGuerra)[1]-1)*varPosGuerraPaladio/aPosGuerra
SlowPosGuerraPlatina = (dim(posGuerra)[1]-1)*varPosGuerraPlatina/bPosGuerra
ShighPosGuerraPlatina = (dim(posGuerra)[1]-1)*varPosGuerraPlatina/aPosGuerra


glPaladio = (((varPreGuerraPaladio/dim(preGuerra)[1]) + varPosGuerraPaladio/dim(posGuerra)[1])**2)/(((varPreGuerraPaladio/dim(preGuerra)[1])**2)/(dim(preGuerra)[1]-1) + ((varPosGuerraPaladio/dim(posGuerra)[1])**2)/(dim(posGuerra)[1]-1))

glPlatina = (((varPreGuerraPlatina/dim(preGuerra)[1]) + varPosGuerraPlatina/dim(posGuerra)[1])**2)/(((varPreGuerraPlatina/dim(preGuerra)[1])**2)/(dim(preGuerra)[1]-1) + ((varPosGuerraPlatina/dim(posGuerra)[1])**2)/(dim(posGuerra)[1]-1))

meanDiffPaladio = mediaPosGuerraPaladio-mediaPreGuerraPaladio
meanDiffPlatina = mediaPosGuerraPlatina-mediaPreGuerraPlatina

meanDiffLowPaladio = meanDiffPaladio - qt(0.975, glPaladio)*sqrt(varPreGuerraPaladio/dim(preGuerra)[1] + varPosGuerraPaladio/dim(posGuerra)[1])

meanDiffHighPaladio = meanDiffPaladio + qt(0.975, glPaladio)*sqrt(varPreGuerraPaladio/dim(preGuerra)[1] + varPosGuerraPaladio/dim(posGuerra)[1])

meanDiffLowPlatina = meanDiffPlatina - qt(0.975, glPlatina)*sqrt(varPreGuerraPlatina/dim(preGuerra)[1] + varPosGuerraPlatina/dim(posGuerra)[1])

meanDiffHighPlatina = meanDiffPlatina + qt(0.975, glPlatina)*sqrt(varPreGuerraPlatina/dim(preGuerra)[1] + varPosGuerraPlatina/dim(posGuerra)[1])

posGuerra$"RetornoPlatina" = NA
posGuerra$"RetornoPaladio" = NA
preGuerra$"RetornoPlatina" = NA
preGuerra$"RetornoPaladio" = NA
contRetornoPaladioPos = 0
contRetornoPaladioPre = 0
contRetornoPlatinaPos = 0
contRetornoPlatinaPre = 0

for(i in 2: dim(posGuerra)[1]){
	posGuerra$RetornoPaladio[i] <- log(posGuerra$Paladio[i]/posGuerra$Paladio[i-1])
	if(posGuerra$RetornoPaladio[i] > 0.025){
		contRetornoPaladioPos = contRetornoPaladioPos + 1
	}
}
for(i in 2: dim(posGuerra)[1]){
	posGuerra$RetornoPlatina[i] <- log(posGuerra$Platina[i]/posGuerra$Platina[i-1])
	if(posGuerra$RetornoPlatina[i] > 0.025){
		contRetornoPlatinaPos = contRetornoPlatinaPos + 1
	}
}
for(i in 2: dim(preGuerra)[1]){
	preGuerra$RetornoPlatina[i] <- log(preGuerra$Platina[i]/preGuerra$Platina[i-1])
	if(preGuerra$RetornoPlatina[i] > 0.025){
		contRetornoPlatinaPre = contRetornoPlatinaPre + 1
	}
}

for(i in 2: dim(preGuerra)[1]){
	preGuerra$RetornoPaladio[i] <- log(preGuerra$Paladio[i]/preGuerra$Paladio[i-1])
	if(preGuerra$RetornoPaladio[i] > 0.025){
		contRetornoPaladioPre = contRetornoPaladioPre + 1
	}
}

mediaPreRetornoPaladio <- mean(preGuerra$RetornoPaladio)
mediaPosRetornoPaladio <- mean(posGuerra$RetornoPaladio)
mediaPreRetornoPlatina <- mean(preGuerra$RetornoPlatina)
mediaPosRetornoPlatina <- mean(posGuerra$RetornoPlatina)

varPreRetornoPaladio <- var(preGuerra$RetornoPaladio, na.rm = TRUE)
varPosRetornoPaladio <- var(posGuerra$RetornoPaladio, na.rm = TRUE)
varPreRetornoPlatina <- var(preGuerra$RetornoPlatina, na.rm = TRUE)
varPosRetornoPlatina <- var(posGuerra$RetornoPlatina, na.rm = TRUE)



SlowRetornoPreGuerraPaladio = (dim(preGuerra)[1]-1)*varPreRetornoPaladio/bPreGuerra
ShighRetornoPreGuerraPaladio = (dim(preGuerra)[1]-1)*varPreRetornoPaladio/aPreGuerra
SlowRetornoPreGuerraPlatina = (dim(preGuerra)[1]-1)*varPreRetornoPlatina/bPreGuerra
ShighRetornoPreGuerraPlatina = (dim(preGuerra)[1]-1)*varPreRetornoPlatina/aPreGuerra

SlowRetornoPosGuerraPaladio = (dim(posGuerra)[1]-1)*varPosRetornoPaladio/bPosGuerra
ShighRetornoPosGuerraPaladio = (dim(posGuerra)[1]-1)*varPosRetornoPaladio/aPosGuerra
SlowRetornoPosGuerraPlatina = (dim(posGuerra)[1]-1)*varPosRetornoPlatina/bPosGuerra
ShighRetornoPosGuerraPlatina = (dim(posGuerra)[1]-1)*varPosRetornoPlatina/aPosGuerra

#SlowRetornoPreGuerraPaladio
#ShighRetornoPreGuerraPaladio
#SlowRetornoPreGuerraPlatina
#ShighRetornoPreGuerraPlatina
#
#SlowRetornoPosGuerraPaladio
#ShighRetornoPosGuerraPaladio
#SlowRetornoPosGuerraPlatina
#ShighRetornoPosGuerraPlatina
#
#XlowPreGuerraPaladio
#XhighPreGuerraPaladio
#
#XlowPreGuerraPlatina
#XhighPreGuerraPlatina
#
#XlowPosGuerraPaladio
#XhighPosGuerraPaladio
#
#XlowPosGuerraPlatina
#XhighPosGuerraPlatina
#
#meanDiffLowPaladio
#
#meanDiffHighPaladio
#
#meanDiffLowPlatina
#
#meanDiffHighPlatina

testeVarPaladio <- bartlett.test(list(preGuerra$Paladio, posGuerra$Paladio))
testeVarPlatina <- bartlett.test(list(preGuerra$Platina, posGuerra$Platina))

testeMediaPaladio <- t.test(preGuerra$Paladio, posGuerra$Paladio, "l", FALSE, FALSE, 0.95)
testeMediaPlatina <- t.test(preGuerra$Platina, posGuerra$Platina, "l", FALSE, FALSE, 0.95)

#testeVarPaladio
#testeVarPlatina
#
#testeMediaPaladio
#testeMediaPlatina
#
#varPreGuerraPaladio
#varPosGuerraPaladio
#
#varPreGuerraPlatina
#varPosGuerraPlatina
plot.new()
plot(preGuerra$Paladio, preGuerra$Platina, main="Gráfico de dispersão pré Guerra", xlab="Preço da platina", ylab="Preço do Paládio", col="red", cex=0.7, pch=0)
regressao1 <- lm(preGuerra$Paladio~preGuerra$Platina)
regressao1
#abline(a=-1597.755, b=3.6165)
abline(a=0.01, b=1.61)
summary(regressao1)

#plot(preGuerra$Paladio, preGuerra$Platina, main="Gráfico de dispersão pós Guerra", xlab="Preço da platina", ylab="Preço do Paládio", col="red", cex=0.7, pch=0)
#regressao2 <- lm(posGuerra$Paladio~posGuerra$Platina)
#regressao2
#abline(regressao2$coefficients[1], regressao2$coefficients[2])


