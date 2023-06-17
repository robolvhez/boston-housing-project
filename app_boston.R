cat("\014");rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); getwd() # Set and print working directory from source file.

library(ggplot2)
theme_set(theme_minimal())
library(ggridges)
library(MASS)
library(forcats)
library(ggpubr)
library(dplyr)
library(corrplot)
library(reshape2)
library(RColorBrewer)
library(GGally)
library(rgl)

predictgrid <- function(model, xvar, yvar, zvar, res = 16, type = NULL) {
  # Find the range of the predictor variable. This works for lm and glm
  # and some others, but may require customization for others.
  xrange <- range(model$model[[xvar]])
  yrange <- range(model$model[[yvar]])
  
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res))
  names(newdata) <- c(xvar, yvar)
  newdata[[zvar]] <- predict(model, newdata = newdata, type = type)
  newdata
}
df2mat <- function(p, xvar = NULL, yvar = NULL, zvar = NULL) {
  if (is.null(xvar)) xvar <- names(p)[1]
  if (is.null(yvar)) yvar <- names(p)[2]
  if (is.null(zvar)) zvar <- names(p)[3]
  
  x <- unique(p[[xvar]])
  y <- unique(p[[yvar]])
  z <- matrix(p[[zvar]], nrow = length(y), ncol = length(x))
  
  m <- list(x, y, z)
  names(m) <- c(xvar, yvar, zvar)
  m
}
interleave <- function(v1, v2)  as.vector(rbind(v1,v2))
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

boston <- read.csv("data/housing-data.csv")
boston <- boston %>%mutate(CHAS = as.factor(CHAS))

# Histogram with RM Counts ----
png("latex/figuras/RM-histogram.png",width=20,height=10,units="cm",res=300)
boston %>% na.omit() %>%
  mutate(CHAS = fct_recode(CHAS, "No limita"="0", "Limita"="1"))%>%
  ggplot(aes(x=RM))+
  geom_histogram(bins=30, binwidth=0.15, position="identity", alpha=0.75)+
  stat_bin(binwidth=0.15, geom="text", colour="black", size=3.5,
           aes(label=..count.., y=(..count..)+5)) +
  labs(x="Promedio de habitaciones por hogar",
       y="Cuenta",
       fill="R?o Charles",
       color="R?o Charles")+
  theme_bw()+
  theme(legend.position="top")+
  scale_x_continuous(breaks=seq(0,10,0.5))+
  scale_y_continuous(limits=c(0,60),
                     breaks=seq(0,60,10))
dev.off()

# Predictor density ----
boxplot <- boston %>% melt() %>%
  ggplot(aes(y=value))+
  geom_boxplot(aes(fill=variable))+
  facet_wrap(.~variable, scales="free")+
  theme_minimal()+
  labs(y="")+
  theme(legend.position="none")+
  scale_fill_viridis_d()+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

# Data normalization ----
boston <- boston %>% na.omit()
boston <- boston %>%mutate(CHAS = as.numeric(CHAS))
for( i in 1:dim(boston)[2] ){
  boston[i] <- ( boston[i] -  min(boston[i]) )/( max(boston[i]) - min(boston[i]) )
}
boston <- boston %>%mutate(CHAS = as.factor(CHAS))

# Density Ridges - Plot
density <- boston %>% melt() %>%
  ggplot(aes(x=value,y=variable,fill=stat(x)))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  labs(x="Valor normalizado",y="Variable")+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks=seq(0,1.2,0.1))
  

png("latex/figuras/boxplot-density.png",width=25,height=9,units="cm",res=300)
ggarrange(boxplot,density,labels=c("A","B"),nrow=1,ncol=2,
          widths=c(1.15,1.75))
dev.off()


# Matriz de correlación ----
boston <- boston %>%mutate(CHAS = as.numeric(CHAS))
boston.corr <- cor(boston)
boston <- boston %>%mutate(CHAS = as.factor(CHAS))
png("latex/figuras/corrplot.png",width=25,height=25,units="cm",res=300)
corrplot(boston.corr,
         #type="lower",
         order="FPC",
         method="color",
         addCoef.col = "black",
         tl.col="black", tl.srt=90,
  )
dev.off()

# Pairwaise scatter plots ----
png("latex/figuras/scatter.png",width=30,height=15,units="cm",res=300)
boston %>% 
  melt(id.vars=c("MEDV","CHAS")) %>%
  mutate(
    density = get_density(MEDV,value,n=100)
  ) %>%
  ggplot(aes(x=value,y=MEDV,color=density))+
  geom_point(alpha=0.5, size=1)+
  geom_smooth(color="black",method="lm",formula=y~x, alpha=0.5, se=F)+
  stat_cor(aes(label = ..r.label..),size=2.5,color="red")+
  facet_wrap(.~variable, scales="free")+
  scale_color_viridis_c()+
  theme(legend.position = "none")+
  labs(x="Valor",
       y="Valor medio de vivienda ($USD mil)")
dev.off()

# LSTAT exponential decay ----
lstat.data <- boston %>%
  melt(id.vars=c("MEDV","CHAS")) %>% 
  mutate(density = get_density(MEDV,value,n=100)) %>%
  filter(variable=="LSTAT")

lstat_exp.reg <- summary(
    boston %>% lm(formula=log(MEDV) ~ LSTAT)
)

png("latex/figuras/scatter_lstat.png",width=12,height=8,units="cm",res=300)
boston %>% 
  melt(id.vars=c("MEDV","CHAS")) %>%
  mutate(
    density = get_density(MEDV,value,n=100)
  ) %>%
  filter(variable=="LSTAT") %>%
  # Grafico
  ggplot(aes(x=value,y=MEDV,color=density))+
  geom_point(alpha=0.5, size=2)+
  geom_smooth(
    color="black",method="glm",formula=y~exp(-0.09*x), 
    alpha=0.5,se=F
  )+
  facet_wrap(.~variable, scales="free")+
  scale_color_viridis_c()+
  theme(legend.position = "none")+
  labs(y="Valor medio de propiedad ($USD mil)",
       x="Poblaci?nn de clase baja (%)")+
  annotate(geom="text",x=20, y=45,
           label= paste(
             "R2 =",format(lstat_exp.reg$r.squared,digits=4), sep=" "),
           color="red")
dev.off()

# ----
boston %>% 
  #select(CRIM, ) %>%
  melt(id.vars=c("CRIM","CHAS")) %>%
  mutate(
    density = get_density(CRIM,value,n=100)
  ) %>%
  ggplot(aes(x=CRIM,y=value,color=density))+
  geom_point(alpha=0.5, size=0.6)+
  geom_smooth(color="black",method="lm",formula=y~x, alpha=0.5, se=F)+
  stat_cor(aes(label = ..r.label..),size=2.5,color="red")+
  facet_wrap(.~variable, scales="free")+
  scale_color_viridis_c()+
  theme(legend.position = "none")+
  labs(x="Valor",
       y="Tasa de criminalidad (%)")

# Relacion entre RM y LSTAT ----
png("latex/figuras/rm_lstat.png",width=12,height=6,units="cm",res=300)
boston %>%
  ggplot(aes(x=RM, y=LSTAT, color=MEDV))+
  geom_point(alpha=0.8)+
  geom_smooth(method="lm", color="red")+
  scale_color_viridis_c()
dev.off()

# CHAS - RM - LSTAT - MEDV ----
png("latex/figuras/chas.png",width=12,height=8,units="cm",res=300)
boston %>%
  select(MEDV,CHAS,LSTAT,RM)%>%
  melt(id.vars=c("MEDV","CHAS"))%>%
  mutate(CHAS = fct_recode(CHAS, "No limita"="0", "Limita"="1"))%>%
  ggplot(aes(x=value,y=MEDV))+
  geom_point(aes(color=CHAS))+
  geom_smooth(method="lm",color="black",formula=y~x)+
  facet_wrap(CHAS~variable)+
  theme_bw()+
  theme(legend.position="none")+
  labs(x="Valor",y="Valor medio de vivienda ($USD mil)")
dev.off()
