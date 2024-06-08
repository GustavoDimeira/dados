# Se precisar carregar pacotes adicionais, siga os exemplos abaixo 
#install.packages("psych")

library("ggplot2")
#library("ggalt")
library("gridExtra")
library("plyr")
library("stringr")
library("forcats")
library("scales")
library("forcats")
library("ExpDes")
library("dplyr")
library("ExpDes.pt")
library(tidyr)
library("Metrics")
library(data.table)


options(scipen = 999)

# pega o parametro que diz a quantia de epocas
args <- commandArgs(trailingOnly = TRUE)

# Verificar se o argumento foi fornecido
if (length(args) == 0) {
  stop("A variavel epochs é obrigatoria")
} else {
  EPOCAS <- as.integer(sub("--epochs=", "", args[1])) - 1
}

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# BOXPLOT DO DESEMPENHO ENTRE TÉCNICAS
#
dados <- read.table('./results.csv',sep=',',header=TRUE)

metricas <- list("mAP50","mAP75","mAP","precision","recall","fscore")
graficos <- list()
i <- 1

for (metrica in metricas) {
  TITULO = sprintf("Boxplot for %s", metrica)
   
   g <- ggplot(dados, aes_string(x = "ml", y = metrica, fill = "ml")) + 
     geom_boxplot() +
     scale_fill_brewer(palette = "Purples") +
     labs(title = TITULO, x = "", y = "") +
     theme(
       legend.position = "none", 
       axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
       axis.text.y = element_text(angle = 0, hjust = 0.5, size = 14),
       plot.title = element_text(size = 16)
     )
   
   graficos[[i]] <- g
   i = i + 1
}


g <- grid.arrange(grobs=graficos, ncol = 3)
ggsave(paste("./boxplot.png", sep=""),g, width = 14, height = 8)
print(g)

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# XY CONTAGEM MANUAL X AUTOMÁTICA - JUNTANDO TODAS AS DOBRAS
#
dadosContagem <- read.table('./counting.csv',sep=',',header=TRUE)
nets <- levels(as.factor(dados$ml))

graficos <- list()
i <- 1

print(nets)
for (net in nets) {

   filtrado <- dadosContagem[dadosContagem$ml == net, ]

   RMSE = rmse(filtrado$groundtruth,filtrado$predicted)
   MAE = mae(filtrado$groundtruth,filtrado$predicted)
   MAPE = mape(filtrado$groundtruth,filtrado$predicted)
   R = cor(filtrado$groundtruth,filtrado$predicted,method = "pearson")
   TITULO = sprintf("%s RMSE=%.3f MAE=%.3f MAPE=%.3f r = %.3f",net,RMSE,MAE,MAPE,R)
   MAX <- max(filtrado$groundtruth, filtrado$predicted)
   
   g <- ggplot(filtrado, aes(x=groundtruth, y=predicted)) + 
        geom_point()+
        geom_smooth(method='lm')+
        labs(title=TITULO ,x="Measured", y = "Predicted")+ theme(plot.title = element_text(size = 10))+
        xlim(0,MAX)+
        ylim(0,MAX)

   print(g)
   graficos[[i]] <- g
   i = i + 1
}

g <- grid.arrange(grobs=graficos, ncol = 2)
ggsave(paste("./counting.png", sep=""),g, width = 10, height = 12)
print(g)


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# XY CONTAGEM MANUAL X AUTOMÁTICA - APENAS PARA A PRIMEIRA DOBRA
#
dadosContagem <- read.table('./counting.csv',sep=',',header=TRUE)

graficos <- list()
i <- 1

dadosContagem <- subset(dadosContagem,fold == 'fold_1')
print(nets)
for (net in nets) {
  
  filtrado <- dadosContagem[dadosContagem$ml == net, ]
  
  RMSE = rmse(filtrado$groundtruth,filtrado$predicted)
  MAE = mae(filtrado$groundtruth,filtrado$predicted)
  MAPE = mape(filtrado$groundtruth,filtrado$predicted)
  R = cor(filtrado$groundtruth,filtrado$predicted,method = "pearson")
  TITULO = sprintf("%s RMSE=%.3f MAE=%.3f MAPE=%.3f r = %.3f",net,RMSE,MAE,MAPE,R)
  MAX <- max(filtrado$groundtruth, filtrado$predicted)
  
  g <- ggplot(filtrado, aes(x=groundtruth, y=predicted)) + 
    geom_point()+
    geom_smooth(method='lm')+
    labs(title=TITULO ,x="Measured", y = "Predicted")+ theme(plot.title = element_text(size = 10))+
    xlim(0,MAX)+
    ylim(0,MAX)
  
  print(g)
  graficos[[i]] <- g
  i = i + 1
}

g <- grid.arrange(grobs=graficos, ncol = 2)
ggsave(paste("./counting_FOLD_1.png", sep=""),g, width = 10, height = 12)
print(g)


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# HISTOGRAMA DA DISTRIBUIÇÃO DOS DADOS DO CONJUNTO DE TESTE
# (CONTAGENS MANUAIS)

g <- ggplot(filtrado, aes(x=groundtruth))+
   geom_histogram(color="darkblue", fill="lightblue")+
   xlab("Objects Countings")+
   ylab("Density")+
   ggtitle("Histogram for Ground Truth Countings (Test Set)")

ggsave(paste("./histogram.png", sep=""),g)
print(g)

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# GERA ESTATÍSTICAS PARA mAP, fscore e R
# Formatado para tabela em Latex


sink('./statistics.txt')


metricas <- list("mAP50","mAP75","mAP","precision","recall","fscore")


dt <- data.table(dados)
cat("\n[ Estatísticas para mAP50]-----------------------------\n")
dt[,list(median=median(mAP50),IQR=IQR(mAP50),mean=mean(mAP50),sd=sd(mAP50)),by=ml]
cat("\n[ Estatísticas para mAP75]-----------------------------\n")
dt[,list(median=median(mAP75),IQR=IQR(mAP75),mean=mean(mAP75),sd=sd(mAP75)),by=ml]
cat("\n[ Estatísticas para mAP]-----------------------------\n")
dt[,list(median=median(mAP),IQR=IQR(mAP),mean=mean(mAP),sd=sd(mAP)),by=ml]
cat("\n[ Estatísticas para precision]-----------------------------\n")
dt[,list(median=median(precision),IQR=IQR(precision),mean=mean(precision),sd=sd(precision)),by=ml]
cat("\n[ Estatísticas para recall]-----------------------------\n")
dt[,list(median=median(recall),IQR=IQR(recall),mean=mean(recall),sd=sd(recall)),by=ml]
cat("\n[ Estatísticas para fscore]-----------------------------\n")
dt[,list(median=median(fscore),IQR=IQR(fscore),mean=mean(fscore),sd=sd(fscore)),by=ml]
# cat("\n[ Estatísticas para MAE]-----------------------------\n")
# dt[,list(median=median(MAE),IQR=IQR(MAE),mean=mean(MAE),sd=sd(MAE)),by=ml]
# cat("\n[ Estatísticas para RMSE]-----------------------------\n")
# dt[,list(median=median(RMSE),IQR=IQR(RMSE),mean=mean(RMSE),sd=sd(RMSE)),by=ml]
# cat("\n[ Estatísticas para r]-----------------------------\n")
# try(dt[,list(median=median(r),IQR=IQR(r),mean=mean(r),sd=sd(r)),by=ml])

sink()


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# APLICA TESTES DE HIPÓTESE E PÓS-TESTE
# Anova e Tukey para mAP, fscore e r


sink('./anova.txt')


cat("[ Teste para mAP50]-----------------------------","\n")
dados.anova <- aov(dados$mAP50 ~ dados$ml)
summary(dados.anova)
tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
tukey

cat("[ Teste para mAP75]-----------------------------","\n")
dados.anova <- aov(dados$mAP75 ~ dados$ml)
summary(dados.anova)
tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
tukey

cat("[ Teste para mAP]-----------------------------","\n")
dados.anova <- aov(dados$mAP ~ dados$ml)
summary(dados.anova)
tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
tukey



cat("[ Teste para precision]-----------------------------","\n")
dados.anova <- aov(dados$precision ~ dados$ml)
summary(dados.anova)
tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
tukey

cat("[ Teste para recall]-----------------------------","\n")
dados.anova <- aov(dados$recall ~ dados$ml)
summary(dados.anova)
tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
tukey

cat("[ Teste para fscore]-----------------------------","\n")
dados.anova <- aov(dados$fscore ~ dados$ml)
summary(dados.anova)
tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
tukey

# cat("[ Teste para MAE]-----------------------------","\n")
# dados.anova <- aov(dados$MAE ~ dados$ml)
# summary(dados.anova)
# tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
# tukey

# cat("[ Teste para RMSE]-----------------------------","\n")
# dados.anova <- aov(dados$RMSE ~ dados$ml)
# summary(dados.anova)
# tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
# tukey

# cat("[ Teste para r]-----------------------------","\n")
# dados.anova <- aov(dados$r ~ dados$ml)
# summary(dados.anova)
# tukey <- TukeyHSD(dados.anova,'dados$ml',conf.level=0.95)
# tukey

sink()

