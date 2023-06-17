cat("\014");rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); getwd() # Set and print working directory from source file.

# Load packages ----
library(corrplot)
library(olsrr)
library(ggplot2)
library(dplyr)
library(openxlsx)

# Data pre-processing ----
boston <- read.csv("data/housing-data.csv")
boston <- boston %>% na.omit() # Datos crudos
boston_norm <- boston %>% mutate(CHAS = as.numeric(CHAS)) # Normalize data
for( i in 1:dim(boston_norm)[2] ){
  boston_norm[i] <- ( boston_norm[i] -  min(boston_norm[i]) )/( max(boston_norm[i]) - min(boston_norm[i]) )
}
boston_corr <- cor(boston_norm) # Correlation matrix

cat("\014")
# Step-forward selection ----
fit_all <- lm(MEDV ~.,data=boston_norm) # Fit all variables
fit_start <- lm(MEDV~1, data=boston_norm)

# ------------------------------------------------------------------------------------
# Run all possible fits (really long processing time - ~5min). Be sure to run it once.
mlcompare <- ols_step_all_possible(fit_all)
#-------------------------------------------------------------------------------------
mlcompare <- data.frame(mlcompare)
dim(mlcompare) # Check dimensions of step-forward data frame. 

lmcompare_top <- mlcompare %>% 
  #select(n,predrsq,rsquare,predictors) %>%
  group_by(n) %>%
  slice_max(order_by = rsquare,n=4)
lmcompare_maxmin <- mlcompare %>% 
  #select(n,predrsq,rsquare,predictors) %>%
  filter(n==1 | n==13)
lmcompare_max <- mlcompare %>%
  group_by(n) %>%
  summarise(rsquare = max(rsquare))
lmcompare_min <- mlcompare %>%
  group_by(n) %>%
  summarise(rsquare = min(rsquare))

lista_df <- list("top"=lmcompare_top,"maxmin"=lmcompare_maxmin)
write.xlsx(lista_df, file = 'resultados.xlsx') 

theme_set(theme_bw())

png("latex/figuras/best subset selection.png",width=25,height=12.5,units="cm",res=300)
mlcompare %>%
  ggplot(aes(x=n,y=rsquare))+
  geom_point(alpha=0.1)+
  geom_point(data=lmcompare_max,aes(x=n,y=rsquare),color="red")+
  geom_line(data=lmcompare_max,aes(x=n,y=rsquare),color="red")+
  geom_text(data=lmcompare_max,aes(x=n,y=rsquare+0.05,
                                   label=format(round(rsquare,4),nsmall=4)),color="red",
            size=3)+
  geom_point(data=lmcompare_min,aes(x=n,y=rsquare),color="blue")+
  geom_line(data=lmcompare_min,aes(x=n,y=rsquare),color="blue")+
  geom_text(data=lmcompare_min,aes(x=n,y=rsquare-0.025,
                                   label=format(round(rsquare,4),nsmall=4)),color="blue",
            size=3)+
  scale_x_continuous(breaks=seq(0,15,1))+
  scale_y_continuous(breaks=seq(0,1,0.25),lim=c(0,0.85))+
  labs(y=expression(R^2),
       x="Number of variables")
dev.off()
