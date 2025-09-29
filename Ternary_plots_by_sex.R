
#Ternary plots - separate for females and males

library(ggtern)
library(dplyr)
library(ggplot2)
citation("ggtern")


#Get datafile
TS <- read.spss("df.sav", to.data.frame=TRUE)
#Use filter, if applicable, & remove missing values from exposure variable
Ternary_sample <- TS[TS$Filter=="Included"&is.na(TS$ex_var)==0,] 

#Convert from seconds to minutes, if applicable
Ternary_sample$SB_min <- Ternary_sample$SB/60
Ternary_sample$MVPA_min <- Ternary_sample$MVPA/60
Ternary_sample$LIPA_min <- Ternary_sample$LIPA/60
Ternary_sample$sleep_min <- Ternary_sample$sleep/60

#Create ternary sample
Ternary_sample <- Ternary_sample %>%
  mutate(total = sleep_min+SB_min+LIPA_min+MVPA_min,
         sleep = sleep_min/total,
         sedentary = SB_min/total,
         LIPA = LIPA_min/total,
         MVPA = MVPA_min/total)

#Defining levels Female and male
is.factor(Ternary_sample$Sex)
Ternary_sample$Sex <- as.factor(Ternary_sample$Sex)
levels(Ternary_sample$Sex) <- c("Female", "Male")

#Arranging direction of CIT score - going from 0 to 10
Ternary_sample <- Ternary_sample %>%
  arrange(ex_var)

# Test with Sleep, sedentary MVPA
ggtern(data = Ternary_sample, aes(x = sleep, y = sedentary, z = MVPA))+
  geom_point ()+
  facet_wrap(~Sex)+
  labs(title = "Ternary plots",
       x = "Sleep", y = "Sedentary", z = "MVPA")+
  theme_bw()

#With colours
library(viridis)

#Sleep Sedentary (SB) MVPA
ggtern(data = Ternary_sample, aes(x = sleep, y = sedentary, z = MVPA, color = CIT_Sev_index_010))+
  geom_point (size = 1)+
  facet_wrap(~Sex)+
  labs(title = "Ternary plot Sleep-SB-MVPA",
       x = "Sleep", y = "SB", z = "MVPA", color = "CIT Sev score (0-10)")+
  scale_color_viridis_c(option = "D", direction = -1)+
  theme_bw()


#Sleep, sedentary og LIPA
ggtern(data = Ternary_sample, aes(x = sleep, y = sedentary, z = LIPA, color = CIT_Sev_index_010))+
  geom_point (size = 1)+
  facet_wrap(~Sex)+
  labs(title = "Ternary plot Sleep-SB-LIPA",
       x = "Sleep", y = "SB", z = "LIPA", color = "CIT Sev score (0-10)")+
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
  labs(title = "Ternary plot SB-LIPA-MVPA",
       x = "SB", y = "LIPA", z = "MVPA", color = "CIT Sev score (0-10)")+
  scale_color_viridis_c(option = "D", direction = -1)+
  theme_bw()


