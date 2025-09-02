library(foreign)
library(robCompositions) 
library(nlme)
library(boot)

#In the current script we will be referring to Von Rosens article Analysing 
#time-use composition as dependent variables in physical activity and 
#sedentary behaviour research: different compositional data analysis approaches
#JASSB 2, 23 (2023). https://doi.org/10.1186/s44167-023-00033-5
#The CoDA in the current script corresponds to approach 3 in Von Rosen

# getting data file
ya <- read.spss("YH4AllCohorts_clean_use_paper_1.sav", to.data.frame=TRUE) 

# Including all individuals with valid accelerometer data and background data
vt <- ya[ya$Sample_paper1_mainanalysis_SleepSedLIPAMVPA=="Included"&
           is.na(ya$mean_PubDevScore_hr)==0&is.na(ya$CIT_Sev_index_010)==0,]

# converting variable names
vt$id <- vt$PID.114875
vt$alder <- vt$Alder_deltakelse_YH4
vt$cit <- vt$CIT_Sev_index_010
vt$pub <- vt$mean_PubDevScore_hr


# Looking at the different variables
summary(vt[,c("sleepMean3", "hvileMean3", "lettMean3", "hardMean3",
              "Sex","alder","CIT_Sev_index_010","pub")])

# without missing, looking at duration
summary(apply(vt[,c("sleepMean3","hvileMean3","lettMean3","hardMean3")],
              1,sum)) # sum 86400, ok

# making duration variables as shares 0:1
vt$sleep <- vt$sleepMean3/86400
vt$hvile<- vt$hvileMean3/86400
vt$lett <- vt$lettMean3/86400
vt$hard <- vt$hardMean3/86400
summary(apply(vt[,c("sleep","hvile","lett","hard")],1,sum)) # sum 1, ok


# dataframe with variables needed to conduct the CoDA
vc <- vt[,c("id","sleep","hvile","lett","hard","Sex","alder","cit","pub")]
summary(vc) # ingen missing

#pivot-transformations with sleep, sedentary (hvile), LIPA (lett) and MVPA (hard)
zsl <- pivotCoord(vc[,c("sleep","hvile","lett","hard")], pivotvar=1)
zhv <- pivotCoord(vc[,c("sleep","hvile","lett","hard")], pivotvar=2)
zle <- pivotCoord(vc[,c("sleep","hvile","lett","hard")], pivotvar=3)
zha <- pivotCoord(vc[,c("sleep","hvile","lett","hard")], pivotvar=4)

# checking
summary(zsl)
summary(zhv)
summary(zle)
summary(zha)

# adding the pivot coordinates to dataframe
# according to Von Rosen script line 32-34
vc <- data.frame(cbind(vc, zsl,zhv,zle,zha))
names(vc) # changing z variable names, according to Von Rosen script line 99-100
names(vc)[10:21] <- paste(rep(c("zsl","zhv","zle","zha"), rep(3,4)),
                          names(vc)[10:21], sep="..")
names(vc) # removing uneccesary repetition mark
names(vc)[15] <- "zhv..lett_ha"

# Regression models similar to Von Rosen approach 3,4
# Regression models for absolute time (not CoDA)
lm0.sleep <- lm(sleep~cit+Sex+alder+pub, data=vc)
lm0.hvile <- lm(hvile~cit+Sex+alder+pub, data=vc)
lm0.lett <- lm(lett~cit+Sex+alder+pub, data=vc)
lm0.hard <- lm(hard~cit+Sex+alder+pub, data=vc)

# vR approach 3, with alle three pivot coordinates for sleep modelled
# vR script line 250-252
lma3.sleep.kontrast1 <- lm(zsl..sleep_hv.le.ha~cit+Sex+alder+pub, data=vc)
lma3.sleep.kontrast2 <- lm(zsl..hvile_le.ha~cit+Sex+alder+pub, data=vc)
lma3.sleep.kontrast3 <- lm(zsl..lett_ha~cit+Sex+alder+pub, data=vc)

# Looking at the contrast models for sleep
summary(lma3.sleep.kontrast1)$coef
summary(lma3.sleep.kontrast2)$coef
summary(lma3.sleep.kontrast3)$coef

# summary of the models of absolute time and approach 3, 
#including confidence intervals
sum.lm0.sleep <- summary(lm0.sleep)
conf.lm0.sleep <- confint(lm0.sleep)
sum.lm0.hvile <- summary(lm0.hvile)
conf.lm0.hvile <- confint(lm0.hvile)
sum.lm0.lett <- summary(lm0.lett)
conf.lm0.lett <- confint(lm0.lett)
sum.lm0.hard <- summary(lm0.hard)
conf.lm0.hard <- confint(lm0.hard)
sum.lma3.sleep.kontrast1 <- summary(lma3.sleep.kontrast1)
conf.lma3.sleep.kontrast1 <- confint(lma3.sleep.kontrast1)
sum.lma3.sleep.kontrast2 <- summary(lma3.sleep.kontrast2)
conf.lma3.sleep.kontrast2 <- confint(lma3.sleep.kontrast2)
sum.lma3.sleep.kontrast3 <- summary(lma3.sleep.kontrast3)
conf.lma3.sleep.kontrast3 <- confint(lma3.sleep.kontrast3)

res0.3 <- data.frame(
  coef=c(sum.lm0.sleep$coef[2,1],sum.lm0.hvile$coef[2,1],
         sum.lm0.lett$coef[2,1],sum.lm0.hard$coef[2,1],
         sum.lma3.sleep.kontrast1$coef[2,1],sum.lma3.sleep.kontrast2$coef[2,1],
         sum.lma3.sleep.kontrast3$coef[2,1]),
  lower=c(conf.lm0.sleep[2,1],conf.lm0.hvile[2,1],
          conf.lm0.lett[2,1],conf.lm0.hard[2,1],
          conf.lma3.sleep.kontrast1[2,1],conf.lma3.sleep.kontrast2[2,1],
          conf.lma3.sleep.kontrast3[2,1]),
  upper=c(conf.lm0.sleep[2,2],conf.lm0.hvile[2,2],
          conf.lm0.lett[2,2],conf.lm0.hard[2,2],
          conf.lma3.sleep.kontrast1[2,2],conf.lma3.sleep.kontrast2[2,2],
          conf.lma3.sleep.kontrast3[2,2]),
  p.value=c(sum.lm0.sleep$coef[2,4],sum.lm0.hvile$coef[2,4],
            sum.lm0.lett$coef[2,4],sum.lm0.hard$coef[2,4],
            sum.lma3.sleep.kontrast1$coef[2,4],sum.lma3.sleep.kontrast2$coef[2,4],
            sum.lma3.sleep.kontrast3$coef[2,4]),
  r.sq=c(sum.lm0.sleep$r.squared,sum.lm0.hvile$r.squared,
          sum.lm0.lett$r.squared,sum.lm0.hard$r.squared,
          sum.lma3.sleep.kontrast1$r.squared,sum.lma3.sleep.kontrast2$r.squared,
          sum.lma3.sleep.kontrast3$r.squared),
  row.names=c("lm0.sleep","lm0.hvile","lm0.lett","lm0.hard",
              "lma3.sleep1","lma3.sleep2","lma3.sleep3"))

res0.3
#writing table to file
write.table(round(res0.3,5), file="res0_3.txt", quote=FALSE, sep=";")

# Predicitons
# Looking at age first
summary(vc$alder)
hist(vc$alder, col="lightgreen")

#Prediction data
names(vc)
summary(vc[,c("cit","Sex","alder","pub")]) # no missing
table(vc$cit) # whole numbers 0 til 10
table(vc$pub) # not whole numbers
hist(vc$pub,col="lightgreen")

#Making prediction dataframe
predframe <- 
  data.frame(cit=rep(0:10,6), 
             Sex=rep(c("Kvinne","Mann"),rep(33,2)),
             alder=rep(rep(c(14,16,17), rep(11,3)),2), 
             pub=rep(3,66))
predframe

# writing table to file
write.table(predframe, file="predframe66.txt", quote=FALSE,
            sep=";", row.names=1:66)

#Predframe mean age = 16
predframe_age16 <- data.frame(
  cit = rep(0:10, 2),
  Sex = rep(c("Kvinne", "Mann"), each = 11),
  alder = 16,
  pub = 3
)

predframe_age16

# Pred models with absolute time, not CoDA, age 14,16,17
pred.lm0sleep <- predict(lm0.sleep, newdata=predframe)
pred.lm0hvile <- predict(lm0.hvile, newdata=predframe)
pred.lm0lett <- predict(lm0.lett, newdata=predframe)
pred.lm0hard <- predict(lm0.hard, newdata=predframe)
pred.lm0 <- data.frame(predframe, psleep=pred.lm0sleep, phvile=pred.lm0hvile,
                       plett=pred.lm0lett, phard=pred.lm0hard)
names(pred.lm0)[c(1,3,4)] <- c("cit","alder","pub")
pred.lm0

#Pred models absolute time, not CoDA, age 16
pred.lm0sleep_age16 <- predict(lm0.sleep, newdata=predframe_age16)
pred.lm0hvile_age16 <- predict(lm0.hvile, newdata=predframe_age16)
pred.lm0lett_age16 <- predict(lm0.lett, newdata=predframe_age16)
pred.lm0hard_age16 <- predict(lm0.hard, newdata=predframe_age16)
pred.lm0_age16 <- data.frame(predframe_age16, psleep=pred.lm0sleep_age16, phvile=pred.lm0hvile_age16,
                       plett=pred.lm0lett_age16, phard=pred.lm0hard_age16)
names(pred.lm0_age16)[c(1,3,4)] <- c("cit","alder","pub")
pred.lm0_age16

# diagram for lm0 (not CoDA), seperate for female and mail, age 14,16,17
lty.f <- "dotted"
lty.m <- "solid"
lwd.f <- 3
lwd.m <- 2
colsleep <- "#A59A6E"
colhvile <- "#C5D1E9"
collett <- "#7BAADF"
colhard <- "#11317E"
pch.a14 <- c(17,rep(NA,9),17) # age 14
pch.a16 <- c(3,rep(NA,9),3)   # age 16
pch.a17 <- c(15,rep(NA,9),15) # age 17
par(mar=c(bottom=3,left=4,top=2,right=0)+0.1)
plot(x=0:10, y=rep(.5,11), col="white", axes=FALSE, xlab="CIT score (0-10)",
     ylab="Predicted time in activity (absolute time)", ylim=c(0,1))
axis(1)
axis(2, las=1, at=c(0:5)/5, labels=(0:5)*20)
box()
points(x=0:10, y=pred.lm0$psleep[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==14], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$psleep[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$psleep[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$phvile[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==14], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$phvile[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$phvile[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f,  col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$plett[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==14], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$plett[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$plett[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$phard[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==14], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$phard[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$phard[pred.lm0$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$psleep[pred.lm0$Sex=="Mann"&pred.lm0$alder==14], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$psleep[pred.lm0$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$psleep[pred.lm0$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$phvile[pred.lm0$Sex=="Mann"&pred.lm0$alder==14], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$phvile[pred.lm0$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$phvile[pred.lm0$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m,  col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$plett[pred.lm0$Sex=="Mann"&pred.lm0$alder==14], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$plett[pred.lm0$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$plett[pred.lm0$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=pred.lm0$phard[pred.lm0$Sex=="Mann"&pred.lm0$alder==14], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=pred.lm0$phard[pred.lm0$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=pred.lm0$phard[pred.lm0$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a17)
par(mar=c(bottom=5,left=4,top=4,right=2)+0.1)

# Filtr??r females and males for age = 16
filt_f <- pred.lm0_age16$Sex == "Kvinne"
filt_m <- pred.lm0_age16$Sex == "Mann"

# Diagram with age = 16 for lm0 (not CoDA)
par(mar=c(5, 4, 2, 8) + 0.1)
plot(x = 0:10, y = rep(.5, 11), col = "white", axes = FALSE,
     xlab = "CIT severity score (0-10)", xlim = c(0,10),
     ylab = "Predicted absolute time in activity", ylim = c(0, 1))
axis(1)
axis(2, las=1, at=c(0:5)/5, labels=(0:5)*20)
box()
points(x = pred.lm0_age16$cit[filt_f], y = pred.lm0_age16$psleep[filt_f],
       lty = lty.f, lwd = lwd.f, col = colsleep, type = "l")
points(x = pred.lm0_age16$cit[filt_f], y = pred.lm0_age16$phvile[filt_f],
       lty = lty.f, lwd = lwd.f, col = colhvile, type = "l")
points(x = pred.lm0_age16$cit[filt_f], y = pred.lm0_age16$plett[filt_f],
       lty = lty.f, lwd = lwd.f, col = collett, type = "l")
points(x = pred.lm0_age16$cit[filt_f], y = pred.lm0_age16$phard[filt_f],
       lty = lty.f, lwd = lwd.f, col = colhard, type = "l")
points(x = pred.lm0_age16$cit[filt_m], y = pred.lm0_age16$psleep[filt_m],
       lty = lty.m, lwd = lwd.m, col = colsleep, type = "l")
points(x = pred.lm0_age16$cit[filt_m], y = pred.lm0_age16$phvile[filt_m],
       lty = lty.m, lwd = lwd.m, col = colhvile, type = "l")
points(x = pred.lm0_age16$cit[filt_m], y = pred.lm0_age16$plett[filt_m],
       lty = lty.m, lwd = lwd.m, col = collett, type = "l")
points(x = pred.lm0_age16$cit[filt_m], y = pred.lm0_age16$phard[filt_m],
       lty = lty.m, lwd = lwd.m, col = colhard, type = "l")
legend("right",
       inset = c(-0.22, 0),
       xpd = TRUE,
       legend = c("Sedentary", "Sleep", "LIPA", "MVPA"),
       col = c("#C5D1E9", "#A59A6E", "#7BAADF", "#11317E"),
       lwd = 2,
       bty = "n")
par(mar=c(5, 4, 4, 8)+ 0.1)

#May be added if axes and not box is preferable in the diagram
abline (h=0, col = "black")
abline(v = 0, col = "black")
axis (1, at = seq(0,10,2), labels = seq(0,10,2), line = 0, lwd = 0, lwd.ticks =1)
axis (2, las = 1, at = seq(0,1, 0.2), labels = seq (0, 100, 20), line = 0, lwd = 0, lwd.ticks = 1)

# approach 3 starting on line 245 in the Von Rosen script, with the models
# at line 250-252.

predframe # 66 combinations of sex, age, CIT and pub score
# see line 260-262 in Von Rosens script
pred.lma3.sleep.kontrast1 <- predict(lma3.sleep.kontrast1, newdata=predframe)
pred.lma3.sleep.kontrast2 <- predict(lma3.sleep.kontrast2, newdata=predframe)
pred.lma3.sleep.kontrast3 <- predict(lma3.sleep.kontrast3, newdata=predframe)
pred.lma3.sleep.kontrasts <- 
  data.frame(cbind(pred.lma3.sleep.kontrast1,pred.lma3.sleep.kontrast2,
                   pred.lma3.sleep.kontrast3))

#Predictions with preddata age=16
pred.lma3.sleep.kontrast1_age16 <- predict(lma3.sleep.kontrast1, newdata=predframe_age16)
pred.lma3.sleep.kontrast2_age16 <- predict(lma3.sleep.kontrast2, newdata=predframe_age16)
pred.lma3.sleep.kontrast3_age16 <- predict(lma3.sleep.kontrast3, newdata=predframe_age16)
pred.lma3.sleep.kontrasts_age16 <- 
  data.frame(cbind(pred.lma3.sleep.kontrast1_age16,pred.lma3.sleep.kontrast2_age16,
                   pred.lma3.sleep.kontrast3_age16))

# Back-transforming
pred.lma3.sleep.kontrasts.inv <- pivotCoordInv(pred.lma3.sleep.kontrasts)
names(pred.lma3.sleep.kontrasts.inv) <- c("psleep","phvile","plett","phard")

# checking if looks okey
summary(vc[,c("sleep","hvile","lett","hard")])
summary(pred.lma3.sleep.kontrasts.inv)
# Not obviously out off course, make data.frame with predframe
pred.lma3 <- data.frame(cbind(predframe, pred.lma3.sleep.kontrasts.inv))
names(pred.lma3)[c(1,3,4)] <- c("cit","alder","pub")


# back-transformation age 16
pred.lma3.sleep.kontrasts.inv_age16 <- pivotCoordInv(pred.lma3.sleep.kontrasts_age16)
names(pred.lma3.sleep.kontrasts.inv_age16) <- c("psleep","phvile","plett","phard")

# checking if looks okey
summary(vc[,c("sleep","hvile","lett","hard")])
summary(pred.lma3.sleep.kontrasts.inv_age16)
# Not obviously out off course, make data.frame with predframe
pred.lma3_age16 <- data.frame(cbind(predframe_age16, pred.lma3.sleep.kontrasts.inv_age16))
names(pred.lma3_age16)[c(1,3,4)] <- c("cit","alder","pub")

# Filters man and women age=16 
filt_f <- pred.lma3_age16$Sex == "Kvinne"
filt_m <- pred.lma3_age16$Sex == "Mann"

# Diagram with age=16
par(mar=c(5, 4, 2, 8) + 0.1)
plot(x = 0:10, y = rep(.5, 11), col = "white", axes = FALSE,
     xlab = "CIT score (0-10)",
     ylab = "Predicted relative time in activity (CoDA)", ylim = c(0, 1))
axis(1)
axis(2, las = 1, at = seq(0, 1, 0.2), labels = seq(0, 100, 20))
box()
points(x = pred.lma3_age16$cit[filt_f], y = pred.lma3_age16$psleep[filt_f],
       lty = lty.f, lwd = lwd.f, col = colsleep, type = "l")
points(x = pred.lma3_age16$cit[filt_f], y = pred.lma3_age16$phvile[filt_f],
       lty = lty.f, lwd = lwd.f, col = colhvile, type = "l")
points(x = pred.lma3_age16$cit[filt_f], y = pred.lma3_age16$plett[filt_f],
       lty = lty.f, lwd = lwd.f, col = collett, type = "l")
points(x = pred.lma3_age16$cit[filt_f], y = pred.lma3_age16$phard[filt_f],
       lty = lty.f, lwd = lwd.f, col = colhard, type = "l")
points(x = pred.lma3_age16$cit[filt_m], y = pred.lma3_age16$psleep[filt_m],
       lty = lty.m, lwd = lwd.m, col = colsleep, type = "l")
points(x = pred.lma3_age16$cit[filt_m], y = pred.lma3_age16$phvile[filt_m],
       lty = lty.m, lwd = lwd.m, col = colhvile, type = "l")
points(x = pred.lma3_age16$cit[filt_m], y = pred.lma3_age16$plett[filt_m],
       lty = lty.m, lwd = lwd.m, col = collett, type = "l")
points(x = pred.lma3_age16$cit[filt_m], y = pred.lma3_age16$phard[filt_m],
       lty = lty.m, lwd = lwd.m, col = colhard, type = "l")
legend("right",
       inset = c(-0.22, 0),
       xpd = TRUE,
       legend = c("Sedentary", "Sleep", "LIPA", "MVPA"),
       col = c("#C5D1E9", "#A59A6E", "#7BAADF", "#11317E"),
       lwd = 2,
       bty = "n")
par(mar=c(5, 4, 4, 8) + 0.1)


# diagram with age 14,16,17
lty.f <- "dotted"
lty.m <- "solid"
lwd.f <- 3
lwd.m <- 2
colsleep <- "#A59A6E"
colhvile <- "#C5D1E9"
collett <- "#7BAADF"
colhard <- "#11317E"
pch.a14 <- c(17,rep(NA,9),17) # age 14
pch.a16 <- c(3,rep(NA,9),3)   # age 16
pch.a17 <- c(15,rep(NA,9),15) # age 17
par(mar=c(bottom=3,left=4,top=2,right=0)+0.1)
plot(x=0:10, y=rep(.5,11), col="white", axes=FALSE, xlab="CIT score (0-10)",
     ylab="predicted activity, approach 3", ylim=c(0,1))
axis(1)
axis(2, las=1, at=c(0:5)/5, labels=(0:5)*20)
box()
points(x=0:10, y=pred.lma3$psleep[pred.lma3$Sex=="Kvinne"&pred.lma3$alder==14], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$psleep[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$psleep[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$phvile[pred.lma3$Sex=="Kvinne"&pred.lma3$alder==14], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$phvile[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$phvile[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$plett[pred.lma3$Sex=="Kvinne"&pred.lma3$alder==14], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$plett[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$plett[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$phard[pred.lma3$Sex=="Kvinne"&pred.lma3$alder==14], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$phard[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==16], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$phard[pred.lma3$Sex=="Kvinne"&pred.lm0$alder==17], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$psleep[pred.lma3$Sex=="Mann"&pred.lma3$alder==14], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$psleep[pred.lma3$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$psleep[pred.lma3$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$phvile[pred.lma3$Sex=="Mann"&pred.lma3$alder==14], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$phvile[pred.lma3$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$phvile[pred.lma3$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$plett[pred.lma3$Sex=="Mann"&pred.lma3$alder==14], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$plett[pred.lma3$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$plett[pred.lma3$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=pred.lma3$phard[pred.lma3$Sex=="Mann"&pred.lma3$alder==14], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=pred.lma3$phard[pred.lma3$Sex=="Mann"&pred.lm0$alder==16], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=pred.lma3$phard[pred.lma3$Sex=="Mann"&pred.lm0$alder==17], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a17)
par(mar=c(bottom=4,left=4,top=0,right=2.5)+0.1)


# Looking at the distribution of the pivot coordinates in approach 3
hist(vc$zsl..sleep_hv.le.ha, col="lightgreen") 
hist(vc$zsl..hvile_le.ha, col="lightgreen") 
hist(vc$zsl..lett_ha, col="lightgreen") 

# saving coefficients and predictions in approach 3 together
samlet.a3 <- c(lma3.sleep.kontrast1$coef[-1],lma3.sleep.kontrast2$coef[-1],
               lma3.sleep.kontrast3$coef[-1],
               pred.lma3.sleep.kontrasts.inv[,1],
               pred.lma3.sleep.kontrasts.inv[,2],
               pred.lma3.sleep.kontrasts.inv[,3],
               pred.lma3.sleep.kontrasts.inv[,4])
names(samlet.a3) <-
  c("cit1","mann1","alder1","pub1","cit2","mann2","alder2","pub2",
    "cit3","mann3","alder3","pub3",
    paste0("cit",predframe[,1],predframe[,2],"a",predframe[,3],".sl"),
    paste0("cit",predframe[,1],predframe[,2],"a",predframe[,3],".hv"),
    paste0("cit",predframe[,1],predframe[,2],"a",predframe[,3],".le"),
    paste0("cit",predframe[,1],predframe[,2],"a",predframe[,3],".ha"))
samlet.a3

# procedure for regression, transformation and prediction 
# back transformation added as a bootstrap function
approach3.bootf <- function(frame1, linjer, predframeuse=predframe) {
  repl <- frame1[linjer,]
  lma3.sleep.kontrast1.b <- 
    lm(zsl..sleep_hv.le.ha~cit+Sex+ alder+
         pub, data=repl)
  lma3.sleep.kontrast2.b <- 
    lm(zsl..hvile_le.ha~cit+Sex+ alder+
         pub, data=repl)
  lma3.sleep.kontrast3.b <- 
    lm(zsl..lett_ha~cit+Sex+ alder+
         pub, data=repl)
  coef.b <- c(lma3.sleep.kontrast1.b$coef[-1],lma3.sleep.kontrast2.b$coef[-1],
            lma3.sleep.kontrast3.b$coef[-1])
  pred.lma3.sleep.kontrast1.b <- 
    predict(lma3.sleep.kontrast1.b, newdata=predframeuse)
  pred.lma3.sleep.kontrast2.b <- 
    predict(lma3.sleep.kontrast2.b, newdata=predframeuse)
  pred.lma3.sleep.kontrast3.b <- 
    predict(lma3.sleep.kontrast3.b, newdata=predframeuse)
  pred.lma3.sleep.kontrasts.b <- 
    data.frame(cbind(pred.lma3.sleep.kontrast1.b,pred.lma3.sleep.kontrast2.b,
                     pred.lma3.sleep.kontrast3.b))
  pred.lma3.sleep.kontrasts.inv.b <- pivotCoordInv(pred.lma3.sleep.kontrasts.b)
  res <- c(coef.b,pred.lma3.sleep.kontrasts.inv.b[,1],
           pred.lma3.sleep.kontrasts.inv.b[,2],
           pred.lma3.sleep.kontrasts.inv.b[,3],
           pred.lma3.sleep.kontrasts.inv.b[,4])
  return(res)
} # end bootstrap function approach3.bootf

# testing on small scale
nbootsjekk <- 100
approach3.boot.sjekk <- boot(data=vc, statistic=approach3.bootf, R=nbootsjekk)
summary(samlet.a3 - approach3.boot.sjekk$t0) # all the same, ok

# bootstrapping in full scale
nboot <- 10000
before.approach3.boot <- Sys.time()
approach3.boot <- boot(data=vc, statistic=approach3.bootf, R=nboot)
after.approach3.boot <- Sys.time()
before.approach3.boot
after.approach3.boot
after.approach3.boot - before.approach3.boot # approx 3 minutes
summary(samlet.a3 - approach3.boot$t0) # similar, ok
approach3.boot # very low standard errors, but similar to the
# standard errors in the normal linear regression (checked)

# visual check
plot(approach3.boot, index=1)
plot(approach3.boot, index=2)
plot(approach3.boot, index=3)
plot(approach3.boot, index=4)
plot(approach3.boot, index=5)
plot(approach3.boot, index=6)
plot(approach3.boot, index=7)
plot(approach3.boot, index=8)
plot(approach3.boot, index=9)
plot(approach3.boot, index=10)
plot(approach3.boot, index=11)
plot(approach3.boot, index=12)
plot(approach3.boot, index=13)
plot(approach3.boot, index=14)
plot(approach3.boot, index=15)
plot(approach3.boot, index=16)
plot(approach3.boot, index=17)
plot(approach3.boot, index=18)
plot(approach3.boot, index=19)
plot(approach3.boot, index=20)
plot(approach3.boot, index=21)
plot(approach3.boot, index=22)
plot(approach3.boot, index=23)
plot(approach3.boot, index=24)
plot(approach3.boot, index=25)
plot(approach3.boot, index=26)
plot(approach3.boot, index=27)
plot(approach3.boot, index=28)
plot(approach3.boot, index=29)
plot(approach3.boot, index=30)
plot(approach3.boot, index=31)
plot(approach3.boot, index=32)
plot(approach3.boot, index=33)
plot(approach3.boot, index=34)
plot(approach3.boot, index=35)
plot(approach3.boot, index=36)
plot(approach3.boot, index=37)
plot(approach3.boot, index=38)
plot(approach3.boot, index=39)
plot(approach3.boot, index=40)
plot(approach3.boot, index=41)
plot(approach3.boot, index=42)
plot(approach3.boot, index=43)
plot(approach3.boot, index=44)
plot(approach3.boot, index=45)
plot(approach3.boot, index=46)
plot(approach3.boot, index=47)
plot(approach3.boot, index=48)
plot(approach3.boot, index=49)
plot(approach3.boot, index=50)
plot(approach3.boot, index=51)
plot(approach3.boot, index=52)
plot(approach3.boot, index=53)
plot(approach3.boot, index=54)
plot(approach3.boot, index=55)
plot(approach3.boot, index=56)
plot(approach3.boot, index=57)
plot(approach3.boot, index=58)
plot(approach3.boot, index=59)
plot(approach3.boot, index=60)
plot(approach3.boot, index=61)
plot(approach3.boot, index=62)
plot(approach3.boot, index=63)
plot(approach3.boot, index=64)
plot(approach3.boot, index=65)
plot(approach3.boot, index=66)
plot(approach3.boot, index=67)
plot(approach3.boot, index=68)
plot(approach3.boot, index=69)
plot(approach3.boot, index=70)
plot(approach3.boot, index=71)
plot(approach3.boot, index=72)
plot(approach3.boot, index=73)
plot(approach3.boot, index=74)
plot(approach3.boot, index=75)
plot(approach3.boot, index=76)
plot(approach3.boot, index=77)
plot(approach3.boot, index=78)
plot(approach3.boot, index=79)
plot(approach3.boot, index=80)
plot(approach3.boot, index=81)
plot(approach3.boot, index=82)
plot(approach3.boot, index=83)
plot(approach3.boot, index=84)
plot(approach3.boot, index=85)
plot(approach3.boot, index=86)
plot(approach3.boot, index=87)
plot(approach3.boot, index=88)
plot(approach3.boot, index=89)
plot(approach3.boot, index=90)
plot(approach3.boot, index=91)
plot(approach3.boot, index=92)
plot(approach3.boot, index=93)
plot(approach3.boot, index=94)
plot(approach3.boot, index=95)
plot(approach3.boot, index=96)
plot(approach3.boot, index=97)
plot(approach3.boot, index=98)
plot(approach3.boot, index=99)
plot(approach3.boot, index=100)
plot(approach3.boot, index=101)
plot(approach3.boot, index=102)
plot(approach3.boot, index=103)
plot(approach3.boot, index=104)
plot(approach3.boot, index=105)
plot(approach3.boot, index=106)
plot(approach3.boot, index=107)
plot(approach3.boot, index=108)
plot(approach3.boot, index=109)
plot(approach3.boot, index=110)
plot(approach3.boot, index=111)
plot(approach3.boot, index=112)
plot(approach3.boot, index=13)
plot(approach3.boot, index=114)
plot(approach3.boot, index=115)
plot(approach3.boot, index=116)
plot(approach3.boot, index=117)
plot(approach3.boot, index=118)
plot(approach3.boot, index=119)
plot(approach3.boot, index=120)
plot(approach3.boot, index=121)
plot(approach3.boot, index=122)
plot(approach3.boot, index=123)
plot(approach3.boot, index=124)
plot(approach3.boot, index=125)
plot(approach3.boot, index=126)
plot(approach3.boot, index=127)
plot(approach3.boot, index=128)
plot(approach3.boot, index=129)
plot(approach3.boot, index=130)
plot(approach3.boot, index=131)
plot(approach3.boot, index=132)
plot(approach3.boot, index=133)
plot(approach3.boot, index=134)
plot(approach3.boot, index=135)
plot(approach3.boot, index=136)
plot(approach3.boot, index=137)
plot(approach3.boot, index=138)
plot(approach3.boot, index=139)
plot(approach3.boot, index=140)
plot(approach3.boot, index=141)
plot(approach3.boot, index=142)
plot(approach3.boot, index=143)
plot(approach3.boot, index=144)
plot(approach3.boot, index=145)
plot(approach3.boot, index=146)
plot(approach3.boot, index=147)
plot(approach3.boot, index=148)
plot(approach3.boot, index=149)
plot(approach3.boot, index=150)
plot(approach3.boot, index=151)
plot(approach3.boot, index=152)
plot(approach3.boot, index=153)
plot(approach3.boot, index=154)
plot(approach3.boot, index=155)
plot(approach3.boot, index=156)
plot(approach3.boot, index=157)
plot(approach3.boot, index=158)
plot(approach3.boot, index=159)
plot(approach3.boot, index=160)
plot(approach3.boot, index=161)
plot(approach3.boot, index=162)
plot(approach3.boot, index=163)
plot(approach3.boot, index=164)
plot(approach3.boot, index=165)
plot(approach3.boot, index=166)
plot(approach3.boot, index=167)
plot(approach3.boot, index=168)
plot(approach3.boot, index=169)
plot(approach3.boot, index=170)
plot(approach3.boot, index=171)
plot(approach3.boot, index=172)
plot(approach3.boot, index=173)
plot(approach3.boot, index=174)
plot(approach3.boot, index=175)
plot(approach3.boot, index=176)
plot(approach3.boot, index=177)
plot(approach3.boot, index=178)
plot(approach3.boot, index=179)
plot(approach3.boot, index=180)
plot(approach3.boot, index=181)
plot(approach3.boot, index=182)
plot(approach3.boot, index=183)
plot(approach3.boot, index=184)
plot(approach3.boot, index=185)
plot(approach3.boot, index=186)
plot(approach3.boot, index=187)
plot(approach3.boot, index=188)
plot(approach3.boot, index=189)
plot(approach3.boot, index=190)
plot(approach3.boot, index=191)
plot(approach3.boot, index=192)
plot(approach3.boot, index=193)
plot(approach3.boot, index=194)
plot(approach3.boot, index=195)
plot(approach3.boot, index=196)
plot(approach3.boot, index=197)
plot(approach3.boot, index=198)
plot(approach3.boot, index=199)
plot(approach3.boot, index=200)
plot(approach3.boot, index=201)
plot(approach3.boot, index=202)
plot(approach3.boot, index=203)
plot(approach3.boot, index=204)
plot(approach3.boot, index=205)
plot(approach3.boot, index=206)
plot(approach3.boot, index=207)
plot(approach3.boot, index=208)
plot(approach3.boot, index=209)
plot(approach3.boot, index=210)
plot(approach3.boot, index=211)
plot(approach3.boot, index=212)
plot(approach3.boot, index=213)
plot(approach3.boot, index=214)
plot(approach3.boot, index=215)
plot(approach3.boot, index=216)
plot(approach3.boot, index=217)
plot(approach3.boot, index=218)
plot(approach3.boot, index=219)
plot(approach3.boot, index=220)
plot(approach3.boot, index=221)
plot(approach3.boot, index=222)
plot(approach3.boot, index=223)
plot(approach3.boot, index=224)
plot(approach3.boot, index=225)
plot(approach3.boot, index=226)
plot(approach3.boot, index=227)
plot(approach3.boot, index=228)
plot(approach3.boot, index=229)
plot(approach3.boot, index=230)
plot(approach3.boot, index=231)
plot(approach3.boot, index=232)
plot(approach3.boot, index=233)
plot(approach3.boot, index=234)
plot(approach3.boot, index=235)
plot(approach3.boot, index=236)
plot(approach3.boot, index=237)
plot(approach3.boot, index=238)
plot(approach3.boot, index=239)
plot(approach3.boot, index=240)
plot(approach3.boot, index=241)
plot(approach3.boot, index=242)
plot(approach3.boot, index=243)
plot(approach3.boot, index=244)
plot(approach3.boot, index=245)
plot(approach3.boot, index=246)
plot(approach3.boot, index=247)
plot(approach3.boot, index=248)
plot(approach3.boot, index=249)
plot(approach3.boot, index=250)
plot(approach3.boot, index=251)
plot(approach3.boot, index=252)
plot(approach3.boot, index=253)
plot(approach3.boot, index=254)
plot(approach3.boot, index=255)
plot(approach3.boot, index=256)
plot(approach3.boot, index=257)
plot(approach3.boot, index=258)
plot(approach3.boot, index=259)
plot(approach3.boot, index=260)
plot(approach3.boot, index=261)
plot(approach3.boot, index=262)
plot(approach3.boot, index=263)
plot(approach3.boot, index=264)
plot(approach3.boot, index=265)
plot(approach3.boot, index=266)
plot(approach3.boot, index=267)
plot(approach3.boot, index=268)
plot(approach3.boot, index=269)
plot(approach3.boot, index=270)
plot(approach3.boot, index=271)
plot(approach3.boot, index=272)
plot(approach3.boot, index=273)
plot(approach3.boot, index=274)
plot(approach3.boot, index=275)
plot(approach3.boot, index=276)
# no problems detected with the visual check
# mostly very close to normal distribution

# confidence intervals, percentiles due to large data material
approach3.boot.ci1 <- boot.ci(approach3.boot, type="perc", conf=.95, index=1)
approach3.boot.ci2 <- boot.ci(approach3.boot, type="perc", conf=.95, index=2)
approach3.boot.ci3 <- boot.ci(approach3.boot, type="perc", conf=.95, index=3)
approach3.boot.ci4 <- boot.ci(approach3.boot, type="perc", conf=.95, index=4)
approach3.boot.ci5 <- boot.ci(approach3.boot, type="perc", conf=.95, index=5)
approach3.boot.ci6 <- boot.ci(approach3.boot, type="perc", conf=.95, index=6)
approach3.boot.ci7 <- boot.ci(approach3.boot, type="perc", conf=.95, index=7)
approach3.boot.ci8 <- boot.ci(approach3.boot, type="perc", conf=.95, index=8)
approach3.boot.ci9 <- boot.ci(approach3.boot, type="perc", conf=.95, index=9)
approach3.boot.ci10 <- boot.ci(approach3.boot, type="perc", conf=.95, index=10)
approach3.boot.ci11 <- boot.ci(approach3.boot, type="perc", conf=.95, index=11)
approach3.boot.ci12 <- boot.ci(approach3.boot, type="perc", conf=.95, index=12)
approach3.boot.ci13 <- boot.ci(approach3.boot, type="perc", conf=.95, index=13)
approach3.boot.ci14 <- boot.ci(approach3.boot, type="perc", conf=.95, index=14)
approach3.boot.ci15 <- boot.ci(approach3.boot, type="perc", conf=.95, index=15)
approach3.boot.ci16 <- boot.ci(approach3.boot, type="perc", conf=.95, index=16)
approach3.boot.ci17 <- boot.ci(approach3.boot, type="perc", conf=.95, index=17)
approach3.boot.ci18 <- boot.ci(approach3.boot, type="perc", conf=.95, index=18)
approach3.boot.ci19 <- boot.ci(approach3.boot, type="perc", conf=.95, index=19)
approach3.boot.ci20 <- boot.ci(approach3.boot, type="perc", conf=.95, index=20)
approach3.boot.ci21 <- boot.ci(approach3.boot, type="perc", conf=.95, index=21)
approach3.boot.ci22 <- boot.ci(approach3.boot, type="perc", conf=.95, index=22)
approach3.boot.ci23 <- boot.ci(approach3.boot, type="perc", conf=.95, index=23)
approach3.boot.ci24 <- boot.ci(approach3.boot, type="perc", conf=.95, index=24)
approach3.boot.ci25 <- boot.ci(approach3.boot, type="perc", conf=.95, index=25)
approach3.boot.ci26 <- boot.ci(approach3.boot, type="perc", conf=.95, index=26)
approach3.boot.ci27 <- boot.ci(approach3.boot, type="perc", conf=.95, index=27)
approach3.boot.ci28 <- boot.ci(approach3.boot, type="perc", conf=.95, index=28)
approach3.boot.ci29 <- boot.ci(approach3.boot, type="perc", conf=.95, index=29)
approach3.boot.ci30 <- boot.ci(approach3.boot, type="perc", conf=.95, index=30)
approach3.boot.ci31 <- boot.ci(approach3.boot, type="perc", conf=.95, index=31)
approach3.boot.ci32 <- boot.ci(approach3.boot, type="perc", conf=.95, index=32)
approach3.boot.ci33 <- boot.ci(approach3.boot, type="perc", conf=.95, index=33)
approach3.boot.ci34 <- boot.ci(approach3.boot, type="perc", conf=.95, index=34)
approach3.boot.ci35 <- boot.ci(approach3.boot, type="perc", conf=.95, index=35)
approach3.boot.ci36 <- boot.ci(approach3.boot, type="perc", conf=.95, index=36)
approach3.boot.ci37 <- boot.ci(approach3.boot, type="perc", conf=.95, index=37)
approach3.boot.ci38 <- boot.ci(approach3.boot, type="perc", conf=.95, index=38)
approach3.boot.ci39 <- boot.ci(approach3.boot, type="perc", conf=.95, index=39)
approach3.boot.ci40 <- boot.ci(approach3.boot, type="perc", conf=.95, index=40)
approach3.boot.ci41 <- boot.ci(approach3.boot, type="perc", conf=.95, index=41)
approach3.boot.ci42 <- boot.ci(approach3.boot, type="perc", conf=.95, index=42)
approach3.boot.ci43 <- boot.ci(approach3.boot, type="perc", conf=.95, index=43)
approach3.boot.ci44 <- boot.ci(approach3.boot, type="perc", conf=.95, index=44)
approach3.boot.ci45 <- boot.ci(approach3.boot, type="perc", conf=.95, index=45)
approach3.boot.ci46 <- boot.ci(approach3.boot, type="perc", conf=.95, index=46)
approach3.boot.ci47 <- boot.ci(approach3.boot, type="perc", conf=.95, index=47)
approach3.boot.ci48 <- boot.ci(approach3.boot, type="perc", conf=.95, index=48)
approach3.boot.ci49 <- boot.ci(approach3.boot, type="perc", conf=.95, index=49)
approach3.boot.ci50 <- boot.ci(approach3.boot, type="perc", conf=.95, index=50)
approach3.boot.ci51 <- boot.ci(approach3.boot, type="perc", conf=.95, index=51)
approach3.boot.ci52 <- boot.ci(approach3.boot, type="perc", conf=.95, index=52)
approach3.boot.ci53 <- boot.ci(approach3.boot, type="perc", conf=.95, index=53)
approach3.boot.ci54 <- boot.ci(approach3.boot, type="perc", conf=.95, index=54)
approach3.boot.ci55 <- boot.ci(approach3.boot, type="perc", conf=.95, index=55)
approach3.boot.ci56 <- boot.ci(approach3.boot, type="perc", conf=.95, index=56)
approach3.boot.ci57 <- boot.ci(approach3.boot, type="perc", conf=.95, index=57)
approach3.boot.ci58 <- boot.ci(approach3.boot, type="perc", conf=.95, index=58)
approach3.boot.ci59 <- boot.ci(approach3.boot, type="perc", conf=.95, index=59)
approach3.boot.ci60 <- boot.ci(approach3.boot, type="perc", conf=.95, index=60)
approach3.boot.ci61 <- boot.ci(approach3.boot, type="perc", conf=.95, index=61)
approach3.boot.ci62 <- boot.ci(approach3.boot, type="perc", conf=.95, index=62)
approach3.boot.ci63 <- boot.ci(approach3.boot, type="perc", conf=.95, index=63)
approach3.boot.ci64 <- boot.ci(approach3.boot, type="perc", conf=.95, index=64)
approach3.boot.ci65 <- boot.ci(approach3.boot, type="perc", conf=.95, index=65)
approach3.boot.ci66 <- boot.ci(approach3.boot, type="perc", conf=.95, index=66)
approach3.boot.ci67 <- boot.ci(approach3.boot, type="perc", conf=.95, index=67)
approach3.boot.ci68 <- boot.ci(approach3.boot, type="perc", conf=.95, index=68)
approach3.boot.ci69 <- boot.ci(approach3.boot, type="perc", conf=.95, index=69)
approach3.boot.ci70 <- boot.ci(approach3.boot, type="perc", conf=.95, index=70)
approach3.boot.ci71 <- boot.ci(approach3.boot, type="perc", conf=.95, index=71)
approach3.boot.ci72 <- boot.ci(approach3.boot, type="perc", conf=.95, index=72)
approach3.boot.ci73 <- boot.ci(approach3.boot, type="perc", conf=.95, index=73)
approach3.boot.ci74 <- boot.ci(approach3.boot, type="perc", conf=.95, index=74)
approach3.boot.ci75 <- boot.ci(approach3.boot, type="perc", conf=.95, index=75)
approach3.boot.ci76 <- boot.ci(approach3.boot, type="perc", conf=.95, index=76)
approach3.boot.ci77 <- boot.ci(approach3.boot, type="perc", conf=.95, index=77)
approach3.boot.ci78 <- boot.ci(approach3.boot, type="perc", conf=.95, index=78)
approach3.boot.ci79 <- boot.ci(approach3.boot, type="perc", conf=.95, index=79)
approach3.boot.ci80 <- boot.ci(approach3.boot, type="perc", conf=.95, index=80)
approach3.boot.ci81 <- boot.ci(approach3.boot, type="perc", conf=.95, index=81)
approach3.boot.ci82 <- boot.ci(approach3.boot, type="perc", conf=.95, index=82)
approach3.boot.ci83 <- boot.ci(approach3.boot, type="perc", conf=.95, index=83)
approach3.boot.ci84 <- boot.ci(approach3.boot, type="perc", conf=.95, index=84)
approach3.boot.ci85 <- boot.ci(approach3.boot, type="perc", conf=.95, index=85)
approach3.boot.ci86 <- boot.ci(approach3.boot, type="perc", conf=.95, index=86)
approach3.boot.ci87 <- boot.ci(approach3.boot, type="perc", conf=.95, index=87)
approach3.boot.ci88 <- boot.ci(approach3.boot, type="perc", conf=.95, index=88)
approach3.boot.ci89 <- boot.ci(approach3.boot, type="perc", conf=.95, index=89)
approach3.boot.ci90 <- boot.ci(approach3.boot, type="perc", conf=.95, index=90)
approach3.boot.ci91 <- boot.ci(approach3.boot, type="perc", conf=.95, index=91)
approach3.boot.ci92 <- boot.ci(approach3.boot, type="perc", conf=.95, index=92)
approach3.boot.ci93 <- boot.ci(approach3.boot, type="perc", conf=.95, index=93)
approach3.boot.ci94 <- boot.ci(approach3.boot, type="perc", conf=.95, index=94)
approach3.boot.ci95 <- boot.ci(approach3.boot, type="perc", conf=.95, index=95)
approach3.boot.ci96 <- boot.ci(approach3.boot, type="perc", conf=.95, index=96)
approach3.boot.ci97 <- boot.ci(approach3.boot, type="perc", conf=.95, index=97)
approach3.boot.ci98 <- boot.ci(approach3.boot, type="perc", conf=.95, index=98)
approach3.boot.ci99 <- boot.ci(approach3.boot, type="perc", conf=.95, index=99)
approach3.boot.ci100 <- boot.ci(approach3.boot, type="perc", conf=.95, index=100)
approach3.boot.ci101 <- boot.ci(approach3.boot, type="perc", conf=.95, index=101)
approach3.boot.ci102 <- boot.ci(approach3.boot, type="perc", conf=.95, index=102)
approach3.boot.ci103 <- boot.ci(approach3.boot, type="perc", conf=.95, index=103)
approach3.boot.ci104 <- boot.ci(approach3.boot, type="perc", conf=.95, index=104)
approach3.boot.ci105 <- boot.ci(approach3.boot, type="perc", conf=.95, index=105)
approach3.boot.ci106 <- boot.ci(approach3.boot, type="perc", conf=.95, index=106)
approach3.boot.ci107 <- boot.ci(approach3.boot, type="perc", conf=.95, index=107)
approach3.boot.ci108 <- boot.ci(approach3.boot, type="perc", conf=.95, index=108)
approach3.boot.ci109 <- boot.ci(approach3.boot, type="perc", conf=.95, index=109)
approach3.boot.ci110 <- boot.ci(approach3.boot, type="perc", conf=.95, index=110)
approach3.boot.ci111 <- boot.ci(approach3.boot, type="perc", conf=.95, index=111)
approach3.boot.ci112 <- boot.ci(approach3.boot, type="perc", conf=.95, index=112)
approach3.boot.ci113 <- boot.ci(approach3.boot, type="perc", conf=.95, index=113)
approach3.boot.ci114 <- boot.ci(approach3.boot, type="perc", conf=.95, index=114)
approach3.boot.ci115 <- boot.ci(approach3.boot, type="perc", conf=.95, index=115)
approach3.boot.ci116 <- boot.ci(approach3.boot, type="perc", conf=.95, index=116)
approach3.boot.ci117 <- boot.ci(approach3.boot, type="perc", conf=.95, index=117)
approach3.boot.ci118 <- boot.ci(approach3.boot, type="perc", conf=.95, index=118)
approach3.boot.ci119 <- boot.ci(approach3.boot, type="perc", conf=.95, index=119)
approach3.boot.ci120 <- boot.ci(approach3.boot, type="perc", conf=.95, index=120)
approach3.boot.ci121 <- boot.ci(approach3.boot, type="perc", conf=.95, index=121)
approach3.boot.ci122 <- boot.ci(approach3.boot, type="perc", conf=.95, index=122)
approach3.boot.ci123 <- boot.ci(approach3.boot, type="perc", conf=.95, index=123)
approach3.boot.ci124 <- boot.ci(approach3.boot, type="perc", conf=.95, index=124)
approach3.boot.ci125 <- boot.ci(approach3.boot, type="perc", conf=.95, index=125)
approach3.boot.ci126 <- boot.ci(approach3.boot, type="perc", conf=.95, index=126)
approach3.boot.ci127 <- boot.ci(approach3.boot, type="perc", conf=.95, index=127)
approach3.boot.ci128 <- boot.ci(approach3.boot, type="perc", conf=.95, index=128)
approach3.boot.ci129 <- boot.ci(approach3.boot, type="perc", conf=.95, index=129)
approach3.boot.ci130 <- boot.ci(approach3.boot, type="perc", conf=.95, index=130)
approach3.boot.ci131 <- boot.ci(approach3.boot, type="perc", conf=.95, index=131)
approach3.boot.ci132 <- boot.ci(approach3.boot, type="perc", conf=.95, index=132)
approach3.boot.ci133 <- boot.ci(approach3.boot, type="perc", conf=.95, index=133)
approach3.boot.ci134 <- boot.ci(approach3.boot, type="perc", conf=.95, index=134)
approach3.boot.ci135 <- boot.ci(approach3.boot, type="perc", conf=.95, index=135)
approach3.boot.ci136 <- boot.ci(approach3.boot, type="perc", conf=.95, index=136)
approach3.boot.ci137 <- boot.ci(approach3.boot, type="perc", conf=.95, index=137)
approach3.boot.ci138 <- boot.ci(approach3.boot, type="perc", conf=.95, index=138)
approach3.boot.ci139 <- boot.ci(approach3.boot, type="perc", conf=.95, index=139)
approach3.boot.ci140 <- boot.ci(approach3.boot, type="perc", conf=.95, index=140)
approach3.boot.ci141 <- boot.ci(approach3.boot, type="perc", conf=.95, index=141)
approach3.boot.ci142 <- boot.ci(approach3.boot, type="perc", conf=.95, index=142)
approach3.boot.ci143 <- boot.ci(approach3.boot, type="perc", conf=.95, index=143)
approach3.boot.ci144 <- boot.ci(approach3.boot, type="perc", conf=.95, index=144)
approach3.boot.ci145 <- boot.ci(approach3.boot, type="perc", conf=.95, index=145)
approach3.boot.ci146 <- boot.ci(approach3.boot, type="perc", conf=.95, index=146)
approach3.boot.ci147 <- boot.ci(approach3.boot, type="perc", conf=.95, index=147)
approach3.boot.ci148 <- boot.ci(approach3.boot, type="perc", conf=.95, index=148)
approach3.boot.ci149 <- boot.ci(approach3.boot, type="perc", conf=.95, index=149)
approach3.boot.ci150 <- boot.ci(approach3.boot, type="perc", conf=.95, index=150)
approach3.boot.ci151 <- boot.ci(approach3.boot, type="perc", conf=.95, index=151)
approach3.boot.ci152 <- boot.ci(approach3.boot, type="perc", conf=.95, index=152)
approach3.boot.ci153 <- boot.ci(approach3.boot, type="perc", conf=.95, index=153)
approach3.boot.ci154 <- boot.ci(approach3.boot, type="perc", conf=.95, index=154)
approach3.boot.ci155 <- boot.ci(approach3.boot, type="perc", conf=.95, index=155)
approach3.boot.ci156 <- boot.ci(approach3.boot, type="perc", conf=.95, index=156)
approach3.boot.ci157 <- boot.ci(approach3.boot, type="perc", conf=.95, index=157)
approach3.boot.ci158 <- boot.ci(approach3.boot, type="perc", conf=.95, index=158)
approach3.boot.ci159 <- boot.ci(approach3.boot, type="perc", conf=.95, index=159)
approach3.boot.ci160 <- boot.ci(approach3.boot, type="perc", conf=.95, index=160)
approach3.boot.ci161 <- boot.ci(approach3.boot, type="perc", conf=.95, index=161)
approach3.boot.ci162 <- boot.ci(approach3.boot, type="perc", conf=.95, index=162)
approach3.boot.ci163 <- boot.ci(approach3.boot, type="perc", conf=.95, index=163)
approach3.boot.ci164 <- boot.ci(approach3.boot, type="perc", conf=.95, index=164)
approach3.boot.ci165 <- boot.ci(approach3.boot, type="perc", conf=.95, index=165)
approach3.boot.ci166 <- boot.ci(approach3.boot, type="perc", conf=.95, index=166)
approach3.boot.ci167 <- boot.ci(approach3.boot, type="perc", conf=.95, index=167)
approach3.boot.ci168 <- boot.ci(approach3.boot, type="perc", conf=.95, index=168)
approach3.boot.ci169 <- boot.ci(approach3.boot, type="perc", conf=.95, index=169)
approach3.boot.ci170 <- boot.ci(approach3.boot, type="perc", conf=.95, index=170)
approach3.boot.ci171 <- boot.ci(approach3.boot, type="perc", conf=.95, index=171)
approach3.boot.ci172 <- boot.ci(approach3.boot, type="perc", conf=.95, index=172)
approach3.boot.ci173 <- boot.ci(approach3.boot, type="perc", conf=.95, index=173)
approach3.boot.ci174 <- boot.ci(approach3.boot, type="perc", conf=.95, index=174)
approach3.boot.ci175 <- boot.ci(approach3.boot, type="perc", conf=.95, index=175)
approach3.boot.ci176 <- boot.ci(approach3.boot, type="perc", conf=.95, index=176)
approach3.boot.ci177 <- boot.ci(approach3.boot, type="perc", conf=.95, index=177)
approach3.boot.ci178 <- boot.ci(approach3.boot, type="perc", conf=.95, index=178)
approach3.boot.ci179 <- boot.ci(approach3.boot, type="perc", conf=.95, index=179)
approach3.boot.ci180 <- boot.ci(approach3.boot, type="perc", conf=.95, index=180)
approach3.boot.ci181 <- boot.ci(approach3.boot, type="perc", conf=.95, index=181)
approach3.boot.ci182 <- boot.ci(approach3.boot, type="perc", conf=.95, index=182)
approach3.boot.ci183 <- boot.ci(approach3.boot, type="perc", conf=.95, index=183)
approach3.boot.ci184 <- boot.ci(approach3.boot, type="perc", conf=.95, index=184)
approach3.boot.ci185 <- boot.ci(approach3.boot, type="perc", conf=.95, index=185)
approach3.boot.ci186 <- boot.ci(approach3.boot, type="perc", conf=.95, index=186)
approach3.boot.ci187 <- boot.ci(approach3.boot, type="perc", conf=.95, index=187)
approach3.boot.ci188 <- boot.ci(approach3.boot, type="perc", conf=.95, index=188)
approach3.boot.ci189 <- boot.ci(approach3.boot, type="perc", conf=.95, index=189)
approach3.boot.ci190 <- boot.ci(approach3.boot, type="perc", conf=.95, index=190)
approach3.boot.ci191 <- boot.ci(approach3.boot, type="perc", conf=.95, index=191)
approach3.boot.ci192 <- boot.ci(approach3.boot, type="perc", conf=.95, index=192)
approach3.boot.ci193 <- boot.ci(approach3.boot, type="perc", conf=.95, index=193)
approach3.boot.ci194 <- boot.ci(approach3.boot, type="perc", conf=.95, index=194)
approach3.boot.ci195 <- boot.ci(approach3.boot, type="perc", conf=.95, index=195)
approach3.boot.ci196 <- boot.ci(approach3.boot, type="perc", conf=.95, index=196)
approach3.boot.ci197 <- boot.ci(approach3.boot, type="perc", conf=.95, index=197)
approach3.boot.ci198 <- boot.ci(approach3.boot, type="perc", conf=.95, index=198)
approach3.boot.ci199 <- boot.ci(approach3.boot, type="perc", conf=.95, index=199)
approach3.boot.ci200 <- boot.ci(approach3.boot, type="perc", conf=.95, index=200)
approach3.boot.ci201 <- boot.ci(approach3.boot, type="perc", conf=.95, index=201)
approach3.boot.ci202 <- boot.ci(approach3.boot, type="perc", conf=.95, index=202)
approach3.boot.ci203 <- boot.ci(approach3.boot, type="perc", conf=.95, index=203)
approach3.boot.ci204 <- boot.ci(approach3.boot, type="perc", conf=.95, index=204)
approach3.boot.ci205 <- boot.ci(approach3.boot, type="perc", conf=.95, index=205)
approach3.boot.ci206 <- boot.ci(approach3.boot, type="perc", conf=.95, index=206)
approach3.boot.ci207 <- boot.ci(approach3.boot, type="perc", conf=.95, index=207)
approach3.boot.ci208 <- boot.ci(approach3.boot, type="perc", conf=.95, index=208)
approach3.boot.ci209 <- boot.ci(approach3.boot, type="perc", conf=.95, index=209)
approach3.boot.ci210 <- boot.ci(approach3.boot, type="perc", conf=.95, index=210)
approach3.boot.ci211 <- boot.ci(approach3.boot, type="perc", conf=.95, index=211)
approach3.boot.ci212 <- boot.ci(approach3.boot, type="perc", conf=.95, index=212)
approach3.boot.ci213 <- boot.ci(approach3.boot, type="perc", conf=.95, index=213)
approach3.boot.ci214 <- boot.ci(approach3.boot, type="perc", conf=.95, index=214)
approach3.boot.ci215 <- boot.ci(approach3.boot, type="perc", conf=.95, index=215)
approach3.boot.ci216 <- boot.ci(approach3.boot, type="perc", conf=.95, index=216)
approach3.boot.ci217 <- boot.ci(approach3.boot, type="perc", conf=.95, index=217)
approach3.boot.ci218 <- boot.ci(approach3.boot, type="perc", conf=.95, index=218)
approach3.boot.ci219 <- boot.ci(approach3.boot, type="perc", conf=.95, index=219)
approach3.boot.ci220 <- boot.ci(approach3.boot, type="perc", conf=.95, index=220)
approach3.boot.ci221 <- boot.ci(approach3.boot, type="perc", conf=.95, index=221)
approach3.boot.ci222 <- boot.ci(approach3.boot, type="perc", conf=.95, index=222)
approach3.boot.ci223 <- boot.ci(approach3.boot, type="perc", conf=.95, index=223)
approach3.boot.ci224 <- boot.ci(approach3.boot, type="perc", conf=.95, index=224)
approach3.boot.ci225 <- boot.ci(approach3.boot, type="perc", conf=.95, index=225)
approach3.boot.ci226 <- boot.ci(approach3.boot, type="perc", conf=.95, index=226)
approach3.boot.ci227 <- boot.ci(approach3.boot, type="perc", conf=.95, index=227)
approach3.boot.ci228 <- boot.ci(approach3.boot, type="perc", conf=.95, index=228)
approach3.boot.ci229 <- boot.ci(approach3.boot, type="perc", conf=.95, index=229)
approach3.boot.ci230 <- boot.ci(approach3.boot, type="perc", conf=.95, index=230)
approach3.boot.ci231 <- boot.ci(approach3.boot, type="perc", conf=.95, index=231)
approach3.boot.ci232 <- boot.ci(approach3.boot, type="perc", conf=.95, index=232)
approach3.boot.ci233 <- boot.ci(approach3.boot, type="perc", conf=.95, index=233)
approach3.boot.ci234 <- boot.ci(approach3.boot, type="perc", conf=.95, index=234)
approach3.boot.ci235 <- boot.ci(approach3.boot, type="perc", conf=.95, index=235)
approach3.boot.ci236 <- boot.ci(approach3.boot, type="perc", conf=.95, index=236)
approach3.boot.ci237 <- boot.ci(approach3.boot, type="perc", conf=.95, index=237)
approach3.boot.ci238 <- boot.ci(approach3.boot, type="perc", conf=.95, index=238)
approach3.boot.ci239 <- boot.ci(approach3.boot, type="perc", conf=.95, index=239)
approach3.boot.ci240 <- boot.ci(approach3.boot, type="perc", conf=.95, index=240)
approach3.boot.ci241 <- boot.ci(approach3.boot, type="perc", conf=.95, index=241)
approach3.boot.ci242 <- boot.ci(approach3.boot, type="perc", conf=.95, index=242)
approach3.boot.ci243 <- boot.ci(approach3.boot, type="perc", conf=.95, index=243)
approach3.boot.ci244 <- boot.ci(approach3.boot, type="perc", conf=.95, index=244)
approach3.boot.ci245 <- boot.ci(approach3.boot, type="perc", conf=.95, index=245)
approach3.boot.ci246 <- boot.ci(approach3.boot, type="perc", conf=.95, index=246)
approach3.boot.ci247 <- boot.ci(approach3.boot, type="perc", conf=.95, index=247)
approach3.boot.ci248 <- boot.ci(approach3.boot, type="perc", conf=.95, index=248)
approach3.boot.ci249 <- boot.ci(approach3.boot, type="perc", conf=.95, index=249)
approach3.boot.ci250 <- boot.ci(approach3.boot, type="perc", conf=.95, index=250)
approach3.boot.ci251 <- boot.ci(approach3.boot, type="perc", conf=.95, index=251)
approach3.boot.ci252 <- boot.ci(approach3.boot, type="perc", conf=.95, index=252)
approach3.boot.ci253 <- boot.ci(approach3.boot, type="perc", conf=.95, index=253)
approach3.boot.ci254 <- boot.ci(approach3.boot, type="perc", conf=.95, index=254)
approach3.boot.ci255 <- boot.ci(approach3.boot, type="perc", conf=.95, index=255)
approach3.boot.ci256 <- boot.ci(approach3.boot, type="perc", conf=.95, index=256)
approach3.boot.ci257 <- boot.ci(approach3.boot, type="perc", conf=.95, index=257)
approach3.boot.ci258 <- boot.ci(approach3.boot, type="perc", conf=.95, index=258)
approach3.boot.ci259 <- boot.ci(approach3.boot, type="perc", conf=.95, index=259)
approach3.boot.ci260 <- boot.ci(approach3.boot, type="perc", conf=.95, index=260)
approach3.boot.ci261 <- boot.ci(approach3.boot, type="perc", conf=.95, index=261)
approach3.boot.ci262 <- boot.ci(approach3.boot, type="perc", conf=.95, index=262)
approach3.boot.ci263 <- boot.ci(approach3.boot, type="perc", conf=.95, index=263)
approach3.boot.ci264 <- boot.ci(approach3.boot, type="perc", conf=.95, index=264)
approach3.boot.ci265 <- boot.ci(approach3.boot, type="perc", conf=.95, index=265)
approach3.boot.ci266 <- boot.ci(approach3.boot, type="perc", conf=.95, index=266)
approach3.boot.ci267 <- boot.ci(approach3.boot, type="perc", conf=.95, index=267)
approach3.boot.ci268 <- boot.ci(approach3.boot, type="perc", conf=.95, index=268)
approach3.boot.ci269 <- boot.ci(approach3.boot, type="perc", conf=.95, index=269)
approach3.boot.ci270 <- boot.ci(approach3.boot, type="perc", conf=.95, index=270)
approach3.boot.ci271 <- boot.ci(approach3.boot, type="perc", conf=.95, index=271)
approach3.boot.ci272 <- boot.ci(approach3.boot, type="perc", conf=.95, index=272)
approach3.boot.ci273 <- boot.ci(approach3.boot, type="perc", conf=.95, index=273)
approach3.boot.ci274 <- boot.ci(approach3.boot, type="perc", conf=.95, index=274)
approach3.boot.ci275 <- boot.ci(approach3.boot, type="perc", conf=.95, index=275)
approach3.boot.ci276 <- boot.ci(approach3.boot, type="perc", conf=.95, index=276)

# saving together with estimates/predictions
samlet.a3
samlet.a3.est.ci <- data.frame(estpred=samlet.a3)
samlet.a3.est.ci$lower <- 
  c(approach3.boot.ci1$percent[1,4],approach3.boot.ci2$percent[1,4],
    approach3.boot.ci3$percent[1,4],approach3.boot.ci4$percent[1,4],
    approach3.boot.ci5$percent[1,4],approach3.boot.ci6$percent[1,4],
    approach3.boot.ci7$percent[1,4],approach3.boot.ci8$percent[1,4],
    approach3.boot.ci9$percent[1,4],approach3.boot.ci10$percent[1,4],
    approach3.boot.ci11$percent[1,4],approach3.boot.ci12$percent[1,4],
    approach3.boot.ci13$percent[1,4],approach3.boot.ci14$percent[1,4],
    approach3.boot.ci15$percent[1,4],approach3.boot.ci16$percent[1,4],
    approach3.boot.ci17$percent[1,4],approach3.boot.ci18$percent[1,4],
    approach3.boot.ci19$percent[1,4],approach3.boot.ci20$percent[1,4],
    approach3.boot.ci21$percent[1,4],approach3.boot.ci22$percent[1,4],
    approach3.boot.ci23$percent[1,4],approach3.boot.ci24$percent[1,4],
    approach3.boot.ci25$percent[1,4],approach3.boot.ci26$percent[1,4],
    approach3.boot.ci27$percent[1,4],approach3.boot.ci28$percent[1,4],
    approach3.boot.ci29$percent[1,4],approach3.boot.ci30$percent[1,4],
    approach3.boot.ci31$percent[1,4],approach3.boot.ci32$percent[1,4],
    approach3.boot.ci33$percent[1,4],approach3.boot.ci34$percent[1,4],
    approach3.boot.ci35$percent[1,4],approach3.boot.ci36$percent[1,4],
    approach3.boot.ci37$percent[1,4],approach3.boot.ci38$percent[1,4],
    approach3.boot.ci39$percent[1,4],approach3.boot.ci40$percent[1,4],
    approach3.boot.ci41$percent[1,4],approach3.boot.ci42$percent[1,4],
    approach3.boot.ci43$percent[1,4],approach3.boot.ci44$percent[1,4],
    approach3.boot.ci45$percent[1,4],approach3.boot.ci46$percent[1,4],
    approach3.boot.ci47$percent[1,4],approach3.boot.ci48$percent[1,4],
    approach3.boot.ci49$percent[1,4],approach3.boot.ci50$percent[1,4],
    approach3.boot.ci51$percent[1,4],approach3.boot.ci52$percent[1,4],
    approach3.boot.ci53$percent[1,4],approach3.boot.ci54$percent[1,4],
    approach3.boot.ci55$percent[1,4],approach3.boot.ci56$percent[1,4],
    approach3.boot.ci57$percent[1,4],approach3.boot.ci58$percent[1,4],
    approach3.boot.ci59$percent[1,4],approach3.boot.ci60$percent[1,4],
    approach3.boot.ci61$percent[1,4],approach3.boot.ci62$percent[1,4],
    approach3.boot.ci63$percent[1,4],approach3.boot.ci64$percent[1,4],
    approach3.boot.ci65$percent[1,4],approach3.boot.ci66$percent[1,4],
    approach3.boot.ci67$percent[1,4],approach3.boot.ci68$percent[1,4],
    approach3.boot.ci69$percent[1,4],approach3.boot.ci70$percent[1,4],
    approach3.boot.ci71$percent[1,4],approach3.boot.ci72$percent[1,4],
    approach3.boot.ci73$percent[1,4],approach3.boot.ci74$percent[1,4],
    approach3.boot.ci75$percent[1,4],approach3.boot.ci76$percent[1,4],
    approach3.boot.ci77$percent[1,4],approach3.boot.ci78$percent[1,4],
    approach3.boot.ci79$percent[1,4],approach3.boot.ci80$percent[1,4],
    approach3.boot.ci81$percent[1,4],approach3.boot.ci82$percent[1,4],
    approach3.boot.ci83$percent[1,4],approach3.boot.ci84$percent[1,4],
    approach3.boot.ci85$percent[1,4],approach3.boot.ci86$percent[1,4],
    approach3.boot.ci87$percent[1,4],approach3.boot.ci88$percent[1,4],
    approach3.boot.ci89$percent[1,4],approach3.boot.ci90$percent[1,4],
    approach3.boot.ci91$percent[1,4],approach3.boot.ci92$percent[1,4],
    approach3.boot.ci93$percent[1,4],approach3.boot.ci94$percent[1,4],
    approach3.boot.ci95$percent[1,4],approach3.boot.ci96$percent[1,4],
    approach3.boot.ci97$percent[1,4],approach3.boot.ci98$percent[1,4],
    approach3.boot.ci99$percent[1,4],approach3.boot.ci100$percent[1,4],
    approach3.boot.ci101$percent[1,4],approach3.boot.ci102$percent[1,4],
    approach3.boot.ci103$percent[1,4],approach3.boot.ci104$percent[1,4],
    approach3.boot.ci105$percent[1,4],approach3.boot.ci106$percent[1,4],
    approach3.boot.ci107$percent[1,4],approach3.boot.ci108$percent[1,4],
    approach3.boot.ci109$percent[1,4],approach3.boot.ci110$percent[1,4],
    approach3.boot.ci111$percent[1,4],approach3.boot.ci112$percent[1,4],
    approach3.boot.ci113$percent[1,4],approach3.boot.ci114$percent[1,4],
    approach3.boot.ci115$percent[1,4],approach3.boot.ci116$percent[1,4],
    approach3.boot.ci117$percent[1,4],approach3.boot.ci118$percent[1,4],
    approach3.boot.ci119$percent[1,4],approach3.boot.ci120$percent[1,4],
    approach3.boot.ci121$percent[1,4],approach3.boot.ci122$percent[1,4],
    approach3.boot.ci123$percent[1,4],approach3.boot.ci124$percent[1,4],
    approach3.boot.ci125$percent[1,4],approach3.boot.ci126$percent[1,4],
    approach3.boot.ci127$percent[1,4],approach3.boot.ci128$percent[1,4],
    approach3.boot.ci129$percent[1,4],approach3.boot.ci130$percent[1,4],
    approach3.boot.ci131$percent[1,4],approach3.boot.ci132$percent[1,4],
    approach3.boot.ci133$percent[1,4],approach3.boot.ci134$percent[1,4],
    approach3.boot.ci135$percent[1,4],approach3.boot.ci136$percent[1,4],
    approach3.boot.ci137$percent[1,4],approach3.boot.ci138$percent[1,4],
    approach3.boot.ci139$percent[1,4],approach3.boot.ci140$percent[1,4],
    approach3.boot.ci141$percent[1,4],approach3.boot.ci142$percent[1,4],
    approach3.boot.ci143$percent[1,4],approach3.boot.ci144$percent[1,4],
    approach3.boot.ci145$percent[1,4],approach3.boot.ci146$percent[1,4],
    approach3.boot.ci147$percent[1,4],approach3.boot.ci148$percent[1,4],
    approach3.boot.ci149$percent[1,4],approach3.boot.ci150$percent[1,4],
    approach3.boot.ci151$percent[1,4],approach3.boot.ci152$percent[1,4],
    approach3.boot.ci153$percent[1,4],approach3.boot.ci154$percent[1,4],
    approach3.boot.ci155$percent[1,4],approach3.boot.ci156$percent[1,4],
    approach3.boot.ci157$percent[1,4],approach3.boot.ci158$percent[1,4],
    approach3.boot.ci159$percent[1,4],approach3.boot.ci160$percent[1,4],
    approach3.boot.ci161$percent[1,4],approach3.boot.ci162$percent[1,4],
    approach3.boot.ci163$percent[1,4],approach3.boot.ci164$percent[1,4],
    approach3.boot.ci165$percent[1,4],approach3.boot.ci166$percent[1,4],
    approach3.boot.ci167$percent[1,4],approach3.boot.ci168$percent[1,4],
    approach3.boot.ci169$percent[1,4],approach3.boot.ci170$percent[1,4],
    approach3.boot.ci171$percent[1,4],approach3.boot.ci172$percent[1,4],
    approach3.boot.ci173$percent[1,4],approach3.boot.ci174$percent[1,4],
    approach3.boot.ci175$percent[1,4],approach3.boot.ci176$percent[1,4],
    approach3.boot.ci177$percent[1,4],approach3.boot.ci178$percent[1,4],
    approach3.boot.ci179$percent[1,4],approach3.boot.ci180$percent[1,4],
    approach3.boot.ci181$percent[1,4],approach3.boot.ci182$percent[1,4],
    approach3.boot.ci183$percent[1,4],approach3.boot.ci184$percent[1,4],
    approach3.boot.ci185$percent[1,4],approach3.boot.ci186$percent[1,4],
    approach3.boot.ci187$percent[1,4],approach3.boot.ci188$percent[1,4],
    approach3.boot.ci189$percent[1,4],approach3.boot.ci190$percent[1,4],
    approach3.boot.ci191$percent[1,4],approach3.boot.ci192$percent[1,4],
    approach3.boot.ci193$percent[1,4],approach3.boot.ci194$percent[1,4],
    approach3.boot.ci195$percent[1,4],approach3.boot.ci196$percent[1,4],
    approach3.boot.ci197$percent[1,4],approach3.boot.ci198$percent[1,4],
    approach3.boot.ci199$percent[1,4],approach3.boot.ci200$percent[1,4],
    approach3.boot.ci201$percent[1,4],approach3.boot.ci202$percent[1,4],
    approach3.boot.ci203$percent[1,4],approach3.boot.ci204$percent[1,4],
    approach3.boot.ci205$percent[1,4],approach3.boot.ci206$percent[1,4],
    approach3.boot.ci207$percent[1,4],approach3.boot.ci208$percent[1,4],
    approach3.boot.ci209$percent[1,4],approach3.boot.ci210$percent[1,4],
    approach3.boot.ci211$percent[1,4],approach3.boot.ci212$percent[1,4],
    approach3.boot.ci213$percent[1,4],approach3.boot.ci214$percent[1,4],
    approach3.boot.ci215$percent[1,4],approach3.boot.ci216$percent[1,4],
    approach3.boot.ci217$percent[1,4],approach3.boot.ci218$percent[1,4],
    approach3.boot.ci219$percent[1,4],approach3.boot.ci220$percent[1,4],
    approach3.boot.ci221$percent[1,4],approach3.boot.ci222$percent[1,4],
    approach3.boot.ci223$percent[1,4],approach3.boot.ci224$percent[1,4],
    approach3.boot.ci225$percent[1,4],approach3.boot.ci226$percent[1,4],
    approach3.boot.ci227$percent[1,4],approach3.boot.ci228$percent[1,4],
    approach3.boot.ci229$percent[1,4],approach3.boot.ci230$percent[1,4],
    approach3.boot.ci231$percent[1,4],approach3.boot.ci232$percent[1,4],
    approach3.boot.ci233$percent[1,4],approach3.boot.ci234$percent[1,4],
    approach3.boot.ci235$percent[1,4],approach3.boot.ci236$percent[1,4],
    approach3.boot.ci237$percent[1,4],approach3.boot.ci238$percent[1,4],
    approach3.boot.ci239$percent[1,4],approach3.boot.ci240$percent[1,4],
    approach3.boot.ci241$percent[1,4],approach3.boot.ci242$percent[1,4],
    approach3.boot.ci243$percent[1,4],approach3.boot.ci244$percent[1,4],
    approach3.boot.ci245$percent[1,4],approach3.boot.ci246$percent[1,4],
    approach3.boot.ci247$percent[1,4],approach3.boot.ci248$percent[1,4],
    approach3.boot.ci249$percent[1,4],approach3.boot.ci250$percent[1,4],
    approach3.boot.ci251$percent[1,4],approach3.boot.ci252$percent[1,4],
    approach3.boot.ci253$percent[1,4],approach3.boot.ci254$percent[1,4],
    approach3.boot.ci255$percent[1,4],approach3.boot.ci256$percent[1,4],
    approach3.boot.ci257$percent[1,4],approach3.boot.ci258$percent[1,4],
    approach3.boot.ci259$percent[1,4],approach3.boot.ci260$percent[1,4],
    approach3.boot.ci261$percent[1,4],approach3.boot.ci262$percent[1,4],
    approach3.boot.ci263$percent[1,4],approach3.boot.ci264$percent[1,4],
    approach3.boot.ci265$percent[1,4],approach3.boot.ci266$percent[1,4],
    approach3.boot.ci267$percent[1,4],approach3.boot.ci268$percent[1,4],
    approach3.boot.ci269$percent[1,4],approach3.boot.ci270$percent[1,4],
    approach3.boot.ci271$percent[1,4],approach3.boot.ci272$percent[1,4],
    approach3.boot.ci273$percent[1,4],approach3.boot.ci274$percent[1,4],
    approach3.boot.ci275$percent[1,4],approach3.boot.ci276$percent[1,4])
samlet.a3.est.ci$upper <- 
  c(approach3.boot.ci1$percent[1,5],approach3.boot.ci2$percent[1,5],
    approach3.boot.ci3$percent[1,5],approach3.boot.ci4$percent[1,5],
    approach3.boot.ci5$percent[1,5],approach3.boot.ci6$percent[1,5],
    approach3.boot.ci7$percent[1,5],approach3.boot.ci8$percent[1,5],
    approach3.boot.ci9$percent[1,5],approach3.boot.ci10$percent[1,5],
    approach3.boot.ci11$percent[1,5],approach3.boot.ci12$percent[1,5],
    approach3.boot.ci13$percent[1,5],approach3.boot.ci14$percent[1,5],
    approach3.boot.ci15$percent[1,5],approach3.boot.ci16$percent[1,5],
    approach3.boot.ci17$percent[1,5],approach3.boot.ci18$percent[1,5],
    approach3.boot.ci19$percent[1,5],approach3.boot.ci20$percent[1,5],
    approach3.boot.ci21$percent[1,5],approach3.boot.ci22$percent[1,5],
    approach3.boot.ci23$percent[1,5],approach3.boot.ci24$percent[1,5],
    approach3.boot.ci25$percent[1,5],approach3.boot.ci26$percent[1,5],
    approach3.boot.ci27$percent[1,5],approach3.boot.ci28$percent[1,5],
    approach3.boot.ci29$percent[1,5],approach3.boot.ci30$percent[1,5],
    approach3.boot.ci31$percent[1,5],approach3.boot.ci32$percent[1,5],
    approach3.boot.ci33$percent[1,5],approach3.boot.ci34$percent[1,5],
    approach3.boot.ci35$percent[1,5],approach3.boot.ci36$percent[1,5],
    approach3.boot.ci37$percent[1,5],approach3.boot.ci38$percent[1,5],
    approach3.boot.ci39$percent[1,5],approach3.boot.ci40$percent[1,5],
    approach3.boot.ci41$percent[1,5],approach3.boot.ci42$percent[1,5],
    approach3.boot.ci43$percent[1,5],approach3.boot.ci44$percent[1,5],
    approach3.boot.ci45$percent[1,5],approach3.boot.ci46$percent[1,5],
    approach3.boot.ci47$percent[1,5],approach3.boot.ci48$percent[1,5],
    approach3.boot.ci49$percent[1,5],approach3.boot.ci50$percent[1,5],
    approach3.boot.ci51$percent[1,5],approach3.boot.ci52$percent[1,5],
    approach3.boot.ci53$percent[1,5],approach3.boot.ci54$percent[1,5],
    approach3.boot.ci55$percent[1,5],approach3.boot.ci56$percent[1,5],
    approach3.boot.ci57$percent[1,5],approach3.boot.ci58$percent[1,5],
    approach3.boot.ci59$percent[1,5],approach3.boot.ci60$percent[1,5],
    approach3.boot.ci61$percent[1,5],approach3.boot.ci62$percent[1,5],
    approach3.boot.ci63$percent[1,5],approach3.boot.ci64$percent[1,5],
    approach3.boot.ci65$percent[1,5],approach3.boot.ci66$percent[1,5],
    approach3.boot.ci67$percent[1,5],approach3.boot.ci68$percent[1,5],
    approach3.boot.ci69$percent[1,5],approach3.boot.ci70$percent[1,5],
    approach3.boot.ci71$percent[1,5],approach3.boot.ci72$percent[1,5],
    approach3.boot.ci73$percent[1,5],approach3.boot.ci74$percent[1,5],
    approach3.boot.ci75$percent[1,5],approach3.boot.ci76$percent[1,5],
    approach3.boot.ci77$percent[1,5],approach3.boot.ci78$percent[1,5],
    approach3.boot.ci79$percent[1,5],approach3.boot.ci80$percent[1,5],
    approach3.boot.ci81$percent[1,5],approach3.boot.ci82$percent[1,5],
    approach3.boot.ci83$percent[1,5],approach3.boot.ci84$percent[1,5],
    approach3.boot.ci85$percent[1,5],approach3.boot.ci86$percent[1,5],
    approach3.boot.ci87$percent[1,5],approach3.boot.ci88$percent[1,5],
    approach3.boot.ci89$percent[1,5],approach3.boot.ci90$percent[1,5],
    approach3.boot.ci91$percent[1,5],approach3.boot.ci92$percent[1,5],
    approach3.boot.ci93$percent[1,5],approach3.boot.ci94$percent[1,5],
    approach3.boot.ci95$percent[1,5],approach3.boot.ci96$percent[1,5],
    approach3.boot.ci97$percent[1,5],approach3.boot.ci98$percent[1,5],
    approach3.boot.ci99$percent[1,5],approach3.boot.ci100$percent[1,5],
    approach3.boot.ci101$percent[1,5],approach3.boot.ci102$percent[1,5],
    approach3.boot.ci103$percent[1,5],approach3.boot.ci104$percent[1,5],
    approach3.boot.ci105$percent[1,5],approach3.boot.ci106$percent[1,5],
    approach3.boot.ci107$percent[1,5],approach3.boot.ci108$percent[1,5],
    approach3.boot.ci109$percent[1,5],approach3.boot.ci110$percent[1,5],
    approach3.boot.ci111$percent[1,5],approach3.boot.ci112$percent[1,5],
    approach3.boot.ci113$percent[1,5],approach3.boot.ci114$percent[1,5],
    approach3.boot.ci115$percent[1,5],approach3.boot.ci116$percent[1,5],
    approach3.boot.ci117$percent[1,5],approach3.boot.ci118$percent[1,5],
    approach3.boot.ci119$percent[1,5],approach3.boot.ci120$percent[1,5],
    approach3.boot.ci121$percent[1,5],approach3.boot.ci122$percent[1,5],
    approach3.boot.ci123$percent[1,5],approach3.boot.ci124$percent[1,5],
    approach3.boot.ci125$percent[1,5],approach3.boot.ci126$percent[1,5],
    approach3.boot.ci127$percent[1,5],approach3.boot.ci128$percent[1,5],
    approach3.boot.ci129$percent[1,5],approach3.boot.ci130$percent[1,5],
    approach3.boot.ci131$percent[1,5],approach3.boot.ci132$percent[1,5],
    approach3.boot.ci133$percent[1,5],approach3.boot.ci134$percent[1,5],
    approach3.boot.ci135$percent[1,5],approach3.boot.ci136$percent[1,5],
    approach3.boot.ci137$percent[1,5],approach3.boot.ci138$percent[1,5],
    approach3.boot.ci139$percent[1,5],approach3.boot.ci140$percent[1,5],
    approach3.boot.ci141$percent[1,5],approach3.boot.ci142$percent[1,5],
    approach3.boot.ci143$percent[1,5],approach3.boot.ci144$percent[1,5],
    approach3.boot.ci145$percent[1,5],approach3.boot.ci146$percent[1,5],
    approach3.boot.ci147$percent[1,5],approach3.boot.ci148$percent[1,5],
    approach3.boot.ci149$percent[1,5],approach3.boot.ci150$percent[1,5],
    approach3.boot.ci151$percent[1,5],approach3.boot.ci152$percent[1,5],
    approach3.boot.ci153$percent[1,5],approach3.boot.ci154$percent[1,5],
    approach3.boot.ci155$percent[1,5],approach3.boot.ci156$percent[1,5],
    approach3.boot.ci157$percent[1,5],approach3.boot.ci158$percent[1,5],
    approach3.boot.ci159$percent[1,5],approach3.boot.ci160$percent[1,5],
    approach3.boot.ci161$percent[1,5],approach3.boot.ci162$percent[1,5],
    approach3.boot.ci163$percent[1,5],approach3.boot.ci164$percent[1,5],
    approach3.boot.ci165$percent[1,5],approach3.boot.ci166$percent[1,5],
    approach3.boot.ci167$percent[1,5],approach3.boot.ci168$percent[1,5],
    approach3.boot.ci169$percent[1,5],approach3.boot.ci170$percent[1,5],
    approach3.boot.ci171$percent[1,5],approach3.boot.ci172$percent[1,5],
    approach3.boot.ci173$percent[1,5],approach3.boot.ci174$percent[1,5],
    approach3.boot.ci175$percent[1,5],approach3.boot.ci176$percent[1,5],
    approach3.boot.ci177$percent[1,5],approach3.boot.ci178$percent[1,5],
    approach3.boot.ci179$percent[1,5],approach3.boot.ci180$percent[1,5],
    approach3.boot.ci181$percent[1,5],approach3.boot.ci182$percent[1,5],
    approach3.boot.ci183$percent[1,5],approach3.boot.ci184$percent[1,5],
    approach3.boot.ci185$percent[1,5],approach3.boot.ci186$percent[1,5],
    approach3.boot.ci187$percent[1,5],approach3.boot.ci188$percent[1,5],
    approach3.boot.ci189$percent[1,5],approach3.boot.ci190$percent[1,5],
    approach3.boot.ci191$percent[1,5],approach3.boot.ci192$percent[1,5],
    approach3.boot.ci193$percent[1,5],approach3.boot.ci194$percent[1,5],
    approach3.boot.ci195$percent[1,5],approach3.boot.ci196$percent[1,5],
    approach3.boot.ci197$percent[1,5],approach3.boot.ci198$percent[1,5],
    approach3.boot.ci199$percent[1,5],approach3.boot.ci200$percent[1,5],
    approach3.boot.ci201$percent[1,5],approach3.boot.ci202$percent[1,5],
    approach3.boot.ci203$percent[1,5],approach3.boot.ci204$percent[1,5],
    approach3.boot.ci205$percent[1,5],approach3.boot.ci206$percent[1,5],
    approach3.boot.ci207$percent[1,5],approach3.boot.ci208$percent[1,5],
    approach3.boot.ci209$percent[1,5],approach3.boot.ci210$percent[1,5],
    approach3.boot.ci211$percent[1,5],approach3.boot.ci212$percent[1,5],
    approach3.boot.ci213$percent[1,5],approach3.boot.ci214$percent[1,5],
    approach3.boot.ci215$percent[1,5],approach3.boot.ci216$percent[1,5],
    approach3.boot.ci217$percent[1,5],approach3.boot.ci218$percent[1,5],
    approach3.boot.ci219$percent[1,5],approach3.boot.ci220$percent[1,5],
    approach3.boot.ci221$percent[1,5],approach3.boot.ci222$percent[1,5],
    approach3.boot.ci223$percent[1,5],approach3.boot.ci224$percent[1,5],
    approach3.boot.ci225$percent[1,5],approach3.boot.ci226$percent[1,5],
    approach3.boot.ci227$percent[1,5],approach3.boot.ci228$percent[1,5],
    approach3.boot.ci229$percent[1,5],approach3.boot.ci230$percent[1,5],
    approach3.boot.ci231$percent[1,5],approach3.boot.ci232$percent[1,5],
    approach3.boot.ci233$percent[1,5],approach3.boot.ci234$percent[1,5],
    approach3.boot.ci235$percent[1,5],approach3.boot.ci236$percent[1,5],
    approach3.boot.ci237$percent[1,5],approach3.boot.ci238$percent[1,5],
    approach3.boot.ci239$percent[1,5],approach3.boot.ci240$percent[1,5],
    approach3.boot.ci241$percent[1,5],approach3.boot.ci242$percent[1,5],
    approach3.boot.ci243$percent[1,5],approach3.boot.ci244$percent[1,5],
    approach3.boot.ci245$percent[1,5],approach3.boot.ci246$percent[1,5],
    approach3.boot.ci247$percent[1,5],approach3.boot.ci248$percent[1,5],
    approach3.boot.ci249$percent[1,5],approach3.boot.ci250$percent[1,5],
    approach3.boot.ci251$percent[1,5],approach3.boot.ci252$percent[1,5],
    approach3.boot.ci253$percent[1,5],approach3.boot.ci254$percent[1,5],
    approach3.boot.ci255$percent[1,5],approach3.boot.ci256$percent[1,5],
    approach3.boot.ci257$percent[1,5],approach3.boot.ci258$percent[1,5],
    approach3.boot.ci259$percent[1,5],approach3.boot.ci260$percent[1,5],
    approach3.boot.ci261$percent[1,5],approach3.boot.ci262$percent[1,5],
    approach3.boot.ci263$percent[1,5],approach3.boot.ci264$percent[1,5],
    approach3.boot.ci265$percent[1,5],approach3.boot.ci266$percent[1,5],
    approach3.boot.ci267$percent[1,5],approach3.boot.ci268$percent[1,5],
    approach3.boot.ci269$percent[1,5],approach3.boot.ci270$percent[1,5],
    approach3.boot.ci271$percent[1,5],approach3.boot.ci272$percent[1,5],
    approach3.boot.ci273$percent[1,5],approach3.boot.ci274$percent[1,5],
    approach3.boot.ci275$percent[1,5],approach3.boot.ci276$percent[1,5])
samlet.a3.est.ci
summary(samlet.a3.est.ci$lower < samlet.a3.est.ci$estpred) # always, ok
summary(samlet.a3.est.ci$estpred < samlet.a3.est.ci$upper) # always, ok
summary(samlet.a3.est.ci$estpred - 
          (samlet.a3.est.ci$lower+samlet.a3.est.ci$upper)/2)
# differing in forth desimal or lower, ok
# predictions normally has thin confidence intervals, interesting to know

# coefficietnts with confidence intervals in noraml regression and bootstrapping
samlet.a3.est.ci.coef <- samlet.a3.est.ci[1:12,]
names(samlet.a3.est.ci.coef)[2:3] <- c("lowerb","upperb")
samlet.a3.est.ci.coef$lower <- 
  c(confint(lma3.sleep.kontrast1)[-1,1],
    confint(lma3.sleep.kontrast2)[-1,1],
    confint(lma3.sleep.kontrast3)[-1,1])
samlet.a3.est.ci.coef$upper <- 
  c(confint(lma3.sleep.kontrast1)[-1,2],
    confint(lma3.sleep.kontrast2)[-1,2],
    confint(lma3.sleep.kontrast3)[-1,2])
samlet.a3.est.ci.coef <- 
  samlet.a3.est.ci.coef[,c("estpred","lower","lowerb","upper","upperb")]
names(samlet.a3.est.ci.coef)[1] <- "estimat"
samlet.a3.est.ci.coef

#predictions with confidence intervals
samlet.a3.est.ci.pred <- samlet.a3.est.ci[13:276,]
names(samlet.a3.est.ci.pred)[1] <- "pred"
samlet.a3.est.ci.pred
# shuffling samlet.a3.est.ci.pred with activitites as variables
samlet.a3.est.ci.pred.wide <- data.frame(
  cbind(samlet.a3.est.ci.pred[1:66,],samlet.a3.est.ci.pred[67:132,],
        samlet.a3.est.ci.pred[133:198,],samlet.a3.est.ci.pred[199:264,]))
names(samlet.a3.est.ci.pred.wide) <- 
  paste0(rep(c("pred","lower","upper"),4))

# diagrams for predictions from approach 3,  
# each activity separate, with confidence intervals
# sleep
# diagram
lty.f <- "dotted"
lty.m <- "solid"
lwd.f <- 3
lwd.m <- 2
lwd.bounds <- 1
colsleep <- "#A59A6E"
colhvile <- "#C5D1E9"
collett <- "#7BAADF"
colhard <- "#11317E"
pch.a14 <- c(17,rep(NA,9),17) # age 14
pch.a16 <- c(3,rep(NA,9),3)   # age 16
pch.a17 <- c(15,rep(NA,9),15) # age 17
ylow.sleep <- 0.25
yhigh.sleep <- 0.35
par(mar=c(bottom=3,left=4,top=0,right=0)+0.1)
plot(x=0:10, y=rep(.3,11), col="white", axes=FALSE, xlab="CIT score (0-10)",
     ylab="predicted sleep, approach 3", ylim=c(ylow.sleep,yhigh.sleep))
axis(1)
axis(2, las=1, at=c(.25,.30,.35), labels=c(25,30,35))
box()
points(x=0:10, y=samlet.a3.est.ci.pred[1:11,1], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[1:11,2], 
       lty=lty.f, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[1:11,3], 
       lty=lty.f, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[12:22,1], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[12:22,2], 
       lty=lty.f, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[12:22,3], 
       lty=lty.f, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[23:33,1], 
       lty=lty.f, lwd=lwd.f, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[23:33,2], 
       lty=lty.f, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[23:33,3], 
       lty=lty.f, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[34:44,1], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[34:44,2], 
       lty=lty.m, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[34:44,3], 
       lty=lty.m, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[45:55,1], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[45:55,2], 
       lty=lty.m, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[45:55,3], 
       lty=lty.m, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[45:55,1], 
       lty=lty.m, lwd=lwd.m, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[56:66,2], 
       lty=lty.m, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[56:66,3], 
       lty=lty.m, lwd=lwd.bounds, col=colsleep, type="o", pch=pch.a17)
par(mar=c(bottom=5,left=4,top=4,right=2)+0.1)

# sedentary behaviour
# diagram
lty.f <- "dotted"
lty.m <- "solid"
lwd.f <- 3
lwd.m <- 2
lwd.bounds <- 1
colsleep <- "#A59A6E"
colhvile <- "#C5D1E9"
collett <- "#7BAADF"
colhard <- "#11317E"
pch.a14 <- c(17,rep(NA,9),17) # age 14
pch.a16 <- c(3,rep(NA,9),3)   # age 16
pch.a17 <- c(15,rep(NA,9),15) # age 17
ylow.hvile <- 0.45
yhigh.hvile <- 0.55
predlines.hvile.k.a14 <- 67:77
predlines.hvile.k.a16 <- 78:88
predlines.hvile.k.a17 <- 89:99
predlines.hvile.m.a14 <- 100:110
predlines.hvile.m.a16 <- 111:121
predlines.hvile.m.a17 <- 122:132
par(mar=c(bottom=3,left=4,top=0,right=0)+0.1)
plot(x=0:10, y=rep(.3,11), col="white", axes=FALSE, xlab="CIT score (0-10)",
     ylab="predicted hvile, approach 3", ylim=c(ylow.hvile,yhigh.hvile))
axis(1)
axis(2, las=1, at=c(45,50,55)/100, labels=c(45,50,55))
box()
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a14,1], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a14,2], 
       lty=lty.f, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a14,3], 
       lty=lty.f, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a16,1], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a16,2], 
       lty=lty.f, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a16,3], 
       lty=lty.f, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a17,1], 
       lty=lty.f, lwd=lwd.f, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a17,2], 
       lty=lty.f, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.k.a17,3], 
       lty=lty.f, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a14,1], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a14,2], 
       lty=lty.m, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a14,3], 
       lty=lty.m, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a16,1], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a16,2], 
       lty=lty.m, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a16,3], 
       lty=lty.m, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a17,1], 
       lty=lty.m, lwd=lwd.m, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a17,2], 
       lty=lty.m, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hvile.m.a17,3], 
       lty=lty.m, lwd=lwd.bounds, col=colhvile, type="o", pch=pch.a17)
par(mar=c(bottom=5,left=4,top=4,right=2)+0.1)

# LIPA
# diagram
lty.f <- "dotted"
lty.m <- "solid"
lwd.f <- 3
lwd.m <- 2
lwd.bounds <- 1
colsleep <- "#A59A6E"
colhvile <- "#C5D1E9"
collett <- "#7BAADF"
colhard <- "#11317E"
pch.a14 <- c(17,rep(NA,9),17) # age 14
pch.a16 <- c(3,rep(NA,9),3)   # age 16
pch.a17 <- c(15,rep(NA,9),15) # age 17
ylow.lett <- 0.14
yhigh.lett <- 0.21
predlines.lett.k.a14 <- 133:143
predlines.lett.k.a16 <- 144:154
predlines.lett.k.a17 <- 155:165
predlines.lett.m.a14 <- 166:176
predlines.lett.m.a16 <- 177:187
predlines.lett.m.a17 <- 188:198
par(mar=c(bottom=3,left=4,top=0,right=0)+0.1)
plot(x=0:10, y=rep(.3,11), col="white", axes=FALSE, xlab="CIT score (0-10)",
     ylab="predicted lett, approach 3", ylim=c(ylow.lett,yhigh.lett))
axis(1)
axis(2, las=1, at=c(14,21)/100, labels=c(14,21))
box()
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a14,1], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a14,2], 
       lty=lty.f, lwd=lwd.bounds, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a14,3], 
       lty=lty.f, lwd=lwd.bounds, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a16,1], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a16,2], 
       lty=lty.f, lwd=lwd.bounds, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a16,3], 
       lty=lty.f, lwd=lwd.bounds, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a17,1], 
       lty=lty.f, lwd=lwd.f, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a17,2], 
       lty=lty.f, lwd=lwd.bounds, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.k.a17,3], 
       lty=lty.f, lwd=lwd.bounds, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a14,1], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a14,2], 
       lty=lty.m, lwd=lwd.bounds, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a14,3], 
       lty=lty.m, lwd=lwd.bounds, col=collett, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a16,1], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a16,2], 
       lty=lty.m, lwd=lwd.bounds, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a16,3], 
       lty=lty.m, lwd=lwd.bounds, col=collett, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a17,1], 
       lty=lty.m, lwd=lwd.m, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a17,2], 
       lty=lty.m, lwd=lwd.bounds, col=collett, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.lett.m.a17,3], 
       lty=lty.m, lwd=lwd.bounds, col=collett, type="o", pch=pch.a17)
par(mar=c(bottom=5,left=4,top=4,right=2)+0.1)

# MVPA
# diagram
lty.f <- "dotted"
lty.m <- "solid"
lwd.f <- 3
lwd.m <- 2
lwd.bounds <- 1
colsleep <- "#A59A6E"
colhvile <- "#C5D1E9"
colhard <- "#7BAADF"
colhard <- "#11317E"
pch.a14 <- c(17,rep(NA,9),17) # age 14
pch.a16 <- c(3,rep(NA,9),3)   # age 16
pch.a17 <- c(15,rep(NA,9),15) # age 17
ylow.hard <- 0.02
yhigh.hard <- 0.04
predlines.hard.k.a14 <- 199:209
predlines.hard.k.a16 <- 210:220
predlines.hard.k.a17 <- 221:231
predlines.hard.m.a14 <- 232:242
predlines.hard.m.a16 <- 243:253
predlines.hard.m.a17 <- 254:264
par(mar=c(bottom=3,left=4,top=0,right=0)+0.1)
plot(x=0:10, y=rep(.3,11), col="white", axes=FALSE, xlab="CIT score (0-10)",
     ylab="predicted hard, approach 3", ylim=c(ylow.hard,yhigh.hard))
axis(1)
axis(2, las=1, at=c(2,3,4)/100, labels=c(2,3,4))
box()
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a14,1], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a14,2], 
       lty=lty.f, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a14,3], 
       lty=lty.f, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a16,1], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a16,2], 
       lty=lty.f, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a16,3], 
       lty=lty.f, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a17,1], 
       lty=lty.f, lwd=lwd.f, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a17,2], 
       lty=lty.f, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.k.a17,3], 
       lty=lty.f, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a14,1], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a14,2], 
       lty=lty.m, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a14,3], 
       lty=lty.m, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a14)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a16,1], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a16,2], 
       lty=lty.m, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a16,3], 
       lty=lty.m, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a16)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a17,1], 
       lty=lty.m, lwd=lwd.m, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a17,2], 
       lty=lty.m, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a17)
points(x=0:10, y=samlet.a3.est.ci.pred[predlines.hard.m.a17,3], 
       lty=lty.m, lwd=lwd.bounds, col=colhard, type="o", pch=pch.a17)
par(mar=c(bottom=5,left=4,top=4,right=2)+0.1)





