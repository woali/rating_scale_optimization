#####code by Alicja Wolny-Dominiak
#####woali@ue.katowice.pl
####Date: may 2018

library(pROC)
library(DEoptim)
library(neuralnet)
library(sfsmisc)
# mat2tex(as.matrix(........), stdout()) 

set.seed(1234)

#Main function
## nsi - the objective function
## x - weights

rsDE <- function(D, attribute, iter){

  #objective function
  nsi <- function(x){
    
    D.predict <- rowSums(x*attribute)
    -1*roc(D, D.predict, plotROC = FALSE)$auc
  }
  
  
    #classifier Sum
  S <- rowSums(attribute)
  
  auc.sum <- roc(D, S, plot = TRUE)$auc
  
  #classifier DEvol
  lower_nsi <- c(rep(0.1, ncol(attribute)))
  upper_nsi <- c(rep(3, ncol(attribute)))
  output <- DEoptim(nsi, lower_nsi, upper_nsi, DEoptim.control(itermax = iter))
  opt.out <- output$optim
  opt.weight <- output$optim$bestmem
  aucResult <- -1*output$optim$bestval
  D.pred <- rowSums(opt.weight*attribute)  
  out <- list(auc.sum = auc.sum, opt.weight = opt.weight, auc.DE = aucResult, D.pred = D.pred, S = S)
  return(out)
  
}

plot3 <- function(D, S, DE){
  p.ex <-roc(D, S, plotROC = FALSE)
  x=1-p.ex$specificities
  y=p.ex$sensitivities
  
  p1.ex <-roc(D, DE)
  x1=1-p1.ex$specificities
  y1=p1.ex$sensitivities
  
  
  par(mfrow = c(1,3))
  #1
  plot(x1, y1, type = 'l', xlab="1-specificities", 
       ylab = 'sensitivities', main = 'Classifier DE')
  polygon(x1, y1, col = 'black')
  abline(0, 1) 
  
  #2
  plot(x, y, type = 'l', xlab="1-specificities", 
       ylab = 'sensitivities', main = 'Classifier sum')
  polygon(x, y, col = 'lightgray')
  abline(0, 1) 
  
  #3
  plot(x1, y1, type = 'l', xlab="1-specificities", 
       ylab = 'sensitivities', main = 'Comparison')
  lines(x,y)
  
  polygon(x1, y1, col = 'black')
  polygon(x, y, col = 'lightgray')
  
  abline(0, 1) 
  legend("bottomright", legend = list("Classifier - DE", "Classifier - Sum"), col = c('black', 'lightgray'),  lty = 1)
}


#########################################
##Example 2.1
data21 <- read.table('ROC_data.txt')
data21 <- data21[ ,c(-1, -9)]

D <- data21$V2
attribute <- data21[ ,2:ncol(data21)]

example <- rsDE(D, attribute, 200)

#classifier Sum
S <- example$S

#classifier DEvol
DE <- example$D.pred

example$auc.DE
example$auc.sum
mat2tex(data.frame(weight.opt = round(example$opt.weight,2)), stdout()) 

##Fig file fig90.tiff
p.ex <-roc(D, S, plotROC = FALSE)
x=1-p.ex$specificities
y=p.ex$sensitivities
plot(x, y, type = 'l', xlab="1-specificities", 
     ylab = 'sensitivities', main = 'Classifier sum')
polygon(x, y, col = 'lightgray')
abline(0, 1)

##Fig file fig100.tiff
plot3(D, S, DE)


#####################################
######## SHS data
#setwd()
mydata <- read.csv2(file="SHS_D8.csv")

attribute1 = mydata[ ,1:6]
D1 = mydata[ ,7]

example1 <- rsDE(D1, attribute1, 200)

#classifier Sum
S1 <- example1$S

#classifier DEvol
DE1 <- example1$D.pred

example1$auc.DE
example1$auc.sum
example1$opt.weight

##Fig file fig120.png
plot3(D1, S1, DE1)
