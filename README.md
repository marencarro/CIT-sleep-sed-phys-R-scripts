# CIT-sleep-sed-phys-R-scripts

NB: these scripts are currently preliminary versions

📘 Scripts for the article: “Childhood Interpersonal Trauma and Adolescent Sleep–Activity Patterns: Findings from the HUNT Study.”

📄 Description

This repository contains the R scripts used for the analyses in the article:

Childhood Interpersonal Trauma and Adolescent Sleep–Activity Patterns: Findings from the HUNT Study. (preliminary title, not published).

The scripts allow reproduction of the statistical analyses and figures related to the compositional data analysis presented in the article. However, we were not able to share the dataset due to confidentiality.

📂 Repository structure
/scripts
    Compositional_data_analysis.R        # Original analysis script (without data)
    Ternary_plots_by_sex.R                # Original analysis script (without data)


🖥️ Requirements

R version: ≥ 4.2

Required packages for CoDA:

boot

nlme

foreign

robCompositions

Required packages for ternary plots:

ggtern

dplyr

ggplot2

You can install all required packages with:

install.packages(c("boot", "nlme", "robCompositions", "foreign", "dplyr", "ggtern", "ggplot2"))

▶️ How to run

Clone or download this repository.

Open the script in RStudio (or any R environment) and apply to own data.

To fully reproduce the article’s results, access to the original data is required (not publicly available due to confidentiality).

🔒 Data availability

The original dataset cannot be shared due to ethical and privacy restrictions.

Variable names in Compositional_data_analysis.R:

Sample_paper1_mainanalysis_SleepSedLIPAMVPA: filter variable
mean_PubDevScore_hr: pubertal development score, covariate
CIT_Sev_index_010: childhood interpersonal trauma score, exposure variable 
Alder_deltakelse_YH4: Age at inclusion
PID.XXXXX: ID variable 
sleepMean3: mean time in sleep (s)
hvileMean3: mean time in sedentary behaviour (s)
lettMean3: mean time in LIPA (s)
hardMean3: mean time in MVPA (s)

📬 Contact

For questions regarding the code or analyses, please contact:

Maren Werner, 
macwer@ous-hf.no

OR open an issue
 in this repository
