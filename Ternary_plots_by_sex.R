
#Ternary plots - separate for females and males

library(ggtern)
library(dplyr)
library(ggplot2)
citation("ggtern")


#Get datafile
TS <- read.spss("YH4AllCohorts_clean_use_paper_1.sav", to.data.frame=TRUE)
Ternary_sample <- TS[TS$Sample_paper1_mainanalysis_SleepSedLIPAMVPA=="Included"&is.na(TS$CIT_Sev_index_010)==0,]

#Convert from seconds to minutes
Ternary_sample$hvileMean3_min <- Ternary_sample$hvileMean3/60
Ternary_sample$hardMean3_min <- Ternary_sample$hardMean3/60
Ternary_sample$lettMean3_min <- Ternary_sample$lettMean3/60

#Create ternary sample
Ternary_sample <- Ternary_sample %>%
  mutate(total = sleepMean3_min+hvileMean3_min+lettMean3_min+hardMean3_min,
         sleep = sleepMean3_min/total,
         sedentary = hvileMean3_min/total,
         LIPA = lettMean3_min/total,
         MVPA = hardMean3_min/total)

#Defining levels Female and male
is.factor(Ternary_sample$Sex)
Ternary_sample$Sex <- as.factor(Ternary_sample$Sex)
levels(Ternary_sample$Sex) <- c("Female", "Male")

#Arranging direction of CIT score - going from 0 to 10
Ternary_sample <- Ternary_sample %>%
  arrange(CIT_Sev_index_010)

# Test with Sleep, sedentary MVPA
ggtern(data = Ternary_sample, aes(x = sleep, y = sedentary, z = MVPA))+
  geom_point ()+
  facet_wrap(~Sex)+
  labs(title = "Ternary plots",
       x = "Sleep", y = "Sedentary", z = "MVPA")+
  theme_bw()

#With colours
library(viridis)

#Sleep Sedentary MVPA
ggtern(data = Ternary_sample, aes(x = sleep, y = sedentary, z = MVPA, color = CIT_Sev_index_010))+
  geom_point (size = 1)+
  facet_wrap(~Sex)+
  labs(title = "Ternary plot Sleep-Sed-MVPA",
       x = "Sleep", y = "Sed", z = "MVPA", color = "CIT Sev score (0-10)")+
  scale_color_viridis_c(option = "D", direction = -1)+
  theme_bw()


#Sleep, sedentary og LIPA
ggtern(data = Ternary_sample, aes(x = sleep, y = sedentary, z = LIPA, color = CIT_Sev_index_010))+
  geom_point (size = 1)+
  facet_wrap(~Sex)+
  labs(title = "Ternary plot Sleep-Sed-LIPA",
       x = "Sleep", y = "Sed", z = "LIPA", color = "CIT Sev score (0-10)")+
  scale_color_viridis_c(option = "D", direction = -1)+
  theme_bw()

#Sleep LIPA MVPA
ggtern(data = Ternary_sample, aes(x = sleep, y = LIPA, z = MVPA, color = CIT_Sev_index_010))+
  geom_point (size = 1)+
  facet_wrap(~Sex)+
  labs(title = "Ternary plot Sleep-LIPA-MVPA",
       x = "Sleep", y = "LIPA", z = "MVPA", color = "CIT Sev score (0-10)")+
  scale_color_viridis_c(option = "D", direction = -1)+
  theme_bw()

#Sedentary LIPA MVPA
ggtern(data = Ternary_sample, aes(x = sedentary, y = LIPA, z = MVPA, color = CIT_Sev_index_010))+
  geom_point (size = 1)+
  facet_wrap(~Sex)+
  labs(title = "Ternary plot Sed-LIPA-MVPA",
       x = "Sed", y = "LIPA", z = "MVPA", color = "CIT Sev score (0-10)")+
  scale_color_viridis_c(option = "D", direction = -1)+
  theme_bw()

