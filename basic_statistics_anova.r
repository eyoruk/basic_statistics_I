Tek_Deney_Normalite_den_PCA_ya

***  excel'de wst-1 ya da wst1 gibi bir isim yerine wst gibi isim yaz
***  excel i as.data.frame e çevir

> setwd('C:/Users/emre.yoruk/Desktop/pca1/')
> dir()
[1] "Normalite_ve_diger_testler.r" "nüm_kar_PCA"                 
[3] "PCA_1.xlsx"                   "pca1"                        
[5] "tek_test.xlsx" 
> library(readxl)
Warning message:
package ‘readxl’ was built under R version 4.2.3 
> tek_test <- read_excel("tek_test.xlsx")
> View(tek_test)
> colnames(tek_test)[1]='Group'
> tektest=as.data.frame(tek_test)
> View(tektest)
> library(ggplot2)
> library(ggpubr)
> library(tidyverse)
> library(rstatix)
> library(devtools)
> devtools::install_github("vqv/ggbiplot")
> library(ggbiplot)

                                                        ##VERİLERE GENEL BAKIŞ###
> summary(tektest)
> mean(tektest$wst)
> sd(tektest$wst)
                                                        ###NORMAL DAĞILIM###
> pdf('density.pdf')
> par(mfrow=c(1,1))
> n=colnames(tektest)
> hist(tektest$wst)
> x= density(tektest$wst)
> lines(x, col='red')
> dev.off()

                                                        ###Shapiro-Wilk###
> shapiro.test(tektest$wst)
> print(shapiro.test(tektest$wst)$p.value)

                                                        ###Multiple Regression###
> model <- lm(wst ~. -Group ,data = tektest)
> model
> summary(model)

                                                        ###ANOVA TEST###
library(devtools)
devtools::install_github("vqv/ggbiplot")
library(ggbiplot)
library(reshape)
ggqqplot(tektest,'wst', facet.by = 'Group')
library(rstatix)
res.aov <- tektest %>% welch_anova_test(wst ~ Group)
pwc <- tektest %>% games_howell_test(wst ~ Group)
pwc <- pwc %>% add_xy_position(x = "Group", step.increase = 1)
ggboxplot(tektest, x = "Group", y = "wst") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
    )

summary(res.aov)
res.aov
wst=aov(wst ~ Group,data=tektest)
wst
summary(wst)

                                                        ###Tukey-LSD###  
> wst=aov(wst ~ Group,data=tektest)
> TukeyHSD(wst, conf.level = .95)
> summary(wst)

                                                        ###Dunnet### 
> library(DescTools)
> DunnettTest(tektest$wst, tektest$Group)
> mean(tektest$wst)
> sd(tektest$wst)

                                                        ###Duncan_GRUPLAMA###
> library(agricolae)
> duncan.test(wst,"Group",alpha=0.05,console=TRUE)
    wst=aov seklinde bir tanimlama yapmak gerekiyor !!!

                                                        ###PCA###
> pcax<-prcomp(tektest$wst,center = T, scale. = T)
> library(ggbiplot)
> ggbiplot(pcax,ellipse=TRUE,   groups=tektest$wst)                                                ***hata veriyor***
> ggbiplot(pcax,ellipse=TRUE,   groups=tektest[,1])                                                ***hata veriyor***
> ggbiplot(pcax,ellipse=TRUE,   groups=tektest)                                                    ***hata veriyor***
            Error in names(df.v) <- names(df.u) : 
                'names' attribute [2] must be the same length as the vector [1]






















