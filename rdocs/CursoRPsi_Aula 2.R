source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

# AULA 2 - R

#############################################################################

p_load(readxl,psych)

# An?lise Fatorial Explorat?ria e Alfa de Cronbach

# Importando arquivo exemploAF.xlsx

dados=read_excel("rdocs/dados/exemploAF.xlsx")



str(dados)

table(dados$X1)


# Correla??es

# Escala autoefic?cia

correlacoes=cor(dados, method="pearson")
correlacoes=cor(dados, method="spearman")
correlacoes=cor(dados, method="kendall")

# Gr?fico de correla??es
cor.plot(correlacoes,numbers=TRUE,main="Autoeficacia")


# Identifica??o do n?mero de fatores

# Extra??o por componentes principais

fit<-fa(dados,fm="pa")

summary(fit)

fit$e.values

# Scree plot 1
#windows()
plot(fit$e.values,type="o",  ylim = c(0, 6),
     xlab="Componente", ylab="Autovalor")

# An?lise paralela

paralela=fa.parallel(dados,nfactors=4, fa="pc", error.bars=TRUE,
                     main="An?lise Paralela", n.iter=20,
                     ylabel=NULL,show.legend=TRUE,
                     sim=TRUE,quant=.95,cor="poly", # ou cor = "cor"
                     use="pairwise",plot=TRUE,correct=.5)


#dados=conjunto1

# N?mero de vari?veis
nvar=dim(dados)[2]
x=c(1:nvar)

# Armazenando os resultados
observado=paralela$pc.values
reamostrado=paralela$pc.simr
simulado=paralela$pc.sim

resultado=cbind(x,observado,reamostrado, simulado)

resultado


# An?lise fatorial

AF1 <- fa(dados, nfactors=3, rotate="Promax", cor=TRUE)

summary(AF1)

print(AF1,sort=T)


# Extraindo as informa??es para c?lculo da % explicada
# N?mero de vari?veis 
nvar=dim(dados)[2]

# Autovalores
auto=AF1$e.values

# % da variabilidade explicada
var_explicada=auto/nvar
var_explicada

# % da variabilidade explicada acumulada
var_explicada_a=var_explicada
for(i in 2:nvar) {
  var_explicada_a[i] <- var_explicada_a[i-1]+var_explicada[i]
}

# Tabela com as informa??es
x=c(1:nvar)
tabela_var=data.frame(cbind(x,auto,var_explicada,var_explicada_a))
tabela_var


# Scree plot 2
#p_load(tidyverse)

ggplot(tabela_var, aes(x=x, y=auto)) +
  scale_x_continuous(n.breaks=nvar)+
  labs(x="Fator ou componente", y="Autovalor")+
  geom_line()+
  geom_point(aes(x=x, y=auto))



# Cargas fatoriais
AF1$loadings

# Comunalidade e especificidade
comunalidade=AF1$communalities
especificidade=1-comunalidade

resultado2=cbind(comunalidade, especificidade)

resultado2

# Gerando os escores dos fatores
escores=data.frame(AF1$scores)

summary(escores)

hist(escores$MR1)

# S?o propostos ent?o 3 fatores:

# Fator 1 (autoeficacia para escola): 1 a 9

# Fator 2 (autoeficacia autoassertiva): 10 a 13

# Fator 3 (autoeficacia para suporte): 14 a 16



# Medidas de fidedignidade

# Escala para fator 1

fat1=dados[,1:9]

a=psych::alpha(fat1,check.keys=T)

summary(a)
a

b=omega(fat1)
summary(b)
b

# Escala para fator 2

fat2=dados[,10:13]

a=alpha(fat2)
summary(a)
a

b=omega(fat2)
summary(b)
b


# Escala para fator 3

fat3=dados[,14:16]

a=alpha(fat3, check.keys = TRUE)
summary(a)
a

b=omega(fat3)
summary(b)
b

# Geral

a=alpha(dados, check.keys = TRUE)
summary(a)
a

b=omega(dados)
summary(b)
b

######################################################################

# An?lise Fatorial Confirmat?ria
p_load(semTools,lavaan,semPlot,corrplot)

# Covari?ncia

covariancia=cov(dados)

# Gr?fico de correla??es
cor.plot(covariancia,numbers=TRUE,main="Autoeficacia")

# Formula??o do modelo

I1=indProd(dados,var1=1:9,var2=14:15)

mod1 <- "Escola =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9
         Assertiva =~ X10 + X11 + X12 + X13
         Suporte =~ X14 + X15 + X16"

# Ajuste do modelo
fit1 <- cfa(mod1, data=dados, estimator = "WLSMV")

# Resultados
summary(fit1, fit.measures=TRUE, standardized = TRUE)


# Gr?fico
semPaths(fit1, what = 'par', layout = "tree2", sizeInt=0.5, std=F,
         edge.label.cex = 0.8, residuals = T, sizeLat=10,
         curve = 2.5, fade = F, rotation = 2, sizeMan=5,
         label.cex = 1.2, nCharNodes = 0, label.norm = "OOOOO",
         label.color = "gray10", border.color = "gray10",
         posCol = c("#024A5F"), negCol = c("#E46A4D"),
         edge.width=0.5, nCharEdges = 1)


# ?ndices de modifica??o

mi1 <- modificationIndices(fit1)
mi1


