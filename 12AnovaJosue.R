library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

file.choose()
frog<- read.csv("/Volumes/macs/documentos mac 5marzo2022/Clase Rstudio/frogs_messy_data.csv")
View(frog)
str(frog)

frog_long <- gather(frog,Temperature, Hatching_time,
                          c(Temperature13,Temperature18,Temperature25))

frog_long
frog_long2<- drop_na(frog_long)
View(frog_long2)
class(frog_long2$Temperature)
class(frog_long2$Hatching_time)

frog_long2$Hatching_time<- as.numeric(frog_long2$Hatching_time)

Frogs<- frog_long2 %>% 
              mutate(Temperature=case_when(
              grepl("Temperature13",Temperature) ~ 13,
              grepl("Temperature18",Temperature) ~ 18,
              grepl("Temperature25",Temperature) ~ 25))


View(Frogs)

class(Frogs$Temperature)

Frogs$Temperature<- as.factor(Frogs$Temperature)

Frogs_hist<- ggplot(Frogs, aes(x=Hatching_time))+
              geom_histogram()

Frogs_hist

Frogs_box<- ggplot(Frogs, aes(x= Temperature, y= Hatching_time))+
              geom_boxplot()
            
Frogs_box

ranas_anova <- aov(Hatching_time ~ Temperature, data = Frogs)
ranas_anova

summary(ranas_anova)

par(mfrow = c(1,2)) #No me funcionó, me marca un error consecuente en los plots "figure margins too large"
hist(ranas_anova$residuals) # histograma de residuoss 
plot(ranas_anova, which = 2) # hace la grafica Q-Q

#Pude graficarlas omitiendo el código par()


# checando homoscedasticidad (Homogeneidad de varianzas) 
plot(ranas_anova, which = 1) # residuos VS datos

dev.off()


resumen_stats <- Frogs%>%
                  group_by(Temperature) %>%
                  summarise(n = n(), # calcular el tamaño de muestra (n)
                  average_hatch = mean(Hatching_time), # calcular la media de tiempo de eclosión
                  SD = sd(Hatching_time))%>% # calcular la desviación estándar
                  mutate(SE = SD / sqrt(n)) # Calcular el error estándar
resumen_stats

barresumen<- ggplot(data=resumen_stats,
                    mapping = aes(x=Temperature, stat= average_hatch))+
              geom_bar()
barresumen


ggplot(data = Frogs, mapping = aes(x=Temperature, 
                                   y= Hatching_time))+
        stat_summary(fun = mean, geom = "bar", fill= "pink", color= "black")

#Cuendo intenté correr el geom_bar desde los datos de resume_stats me marcaba error, por lo que realicé el average directamente en el barplot
#para esto generé lo siguiente...

library(Hmisc)

ggplot(data = Frogs, mapping = aes(x=Temperature, 
                                   y= Hatching_time))+
  stat_summary(fun = mean, geom = "bar", fill= "pink", color= "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width=.2, position = "dodge")
