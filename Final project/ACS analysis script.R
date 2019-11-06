### Data Analytics with R - ACS Project ###


library(tidyverse)
library(data.table)
library(forcats)
library(knitr)
library(kableExtra)
library(ggthemes)
library(scales)
library(gridExtra)
library(ggcorrplot)
library(olsrr)
library(weights)
library(car)


### DATA DICTIONARY ###

      # PWGTP = weights
      # ADJINC = adjustment factor for income  * adjusts reported income to 2016 dollars.* 
   
      # AGEP = age
      # CIT = citizenship status
      # SCHL = educational attainment 
      # SEX = sex
      # DIS = disability record
      # HICOV = health insurance coverage
      # HINS3 = Medicare, for people 65 and older, or people with certain disabilities
      # HINS4 = Medicaid, Medical Assistance, or any kind of government-assistance plan for those with low incomes or a disability
      # LANP = language spoke at home
      # NATIVITY = nativity (native or foreign born)
      # PRIVCOV = priavte health insuracne coverage
      # PUBCOV = public health insurange coverage
      # RAC1P =  race code
      # RACWHT = white - yes or no
      # SCIENGP = Field of degree science and engineering
      # PAP = Public assistance income past 12 months    > use ADJINC
      # WAGP = Wages or salary income past 12 months     > use ADJINC
      # SEMP = Self-employment income past 12 months     > use ADJINC
      # PERNP = total persons earinings                  > use ADJINC
      # RETP = retirement income in the past 12 months   > use ADJINC


### Rounded / top and bottom coded variables used:
      
      # SEMP:      "N/A (less than 15 years old)"
      #            "None"
      #        >>> "Loss $1 to $10000 (Rounded and bottom-coded***)"
      #            "1 or break even"
      #        >>> "2 to $999999 (Rounded and top-coded***)"


      # WAGP:      "N/A (less than 15 years old)",
      #            "None",
      #        >>> "$1 to 999999 (Rounded and top-coded)")


      # PERNP:     "N/A (less than 15 years old)",
      #            "No earnings",
      #        >>> "Loss of $10000 or more(Rounded & bottom-coded .components***)",
      #        >>> "Loss $1 to $9999 (Rounded components)",
      #            "$1 or break even",
      #            "$2 to $9999999")


# read in only interesting columns using data.table:

keep_cols <- c("PWGTP", "ADJINC", "AGEP" , "CIT", "SCHL", "SEX", 
               "DIS", "HICOV", "HINS3", "HINS4", "LANP", "NATIVITY",
               "PRIVCOV", "PUBCOV", "RAC1P", "RACWHT", "SCIENGP", 
               "FCITP", "FDISP", "PAP","WAGP", "SEMP", "PERNP", "RETP")

# read_csv too slow, using data.table instead:
acs_data_1 <- fread("ss16pusa.csv", 
                  header = TRUE, 
                  select = keep_cols, 
                  verbose = TRUE)

acs_data_2 <- fread("ss16pusb.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

acs_data_3 <- fread("ss16pusc.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

acs_data_4 <- fread("ss16pusd.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

#combine data sets by rows:
acs_data <- data.table(rbind(acs_data_1, acs_data_2, acs_data_3, acs_data_4))

# remove data sets from workspace:
rm(acs_data_1, acs_data_2, acs_data_3, acs_data_4)



### >>>  Preprocessing and encoding  <<< ###


# replacing missing data coded as not NA

acs_data <- acs_data %>% 
                  mutate( SCHL = na_if(SCHL, 'bb'),
                          SEMP = na_if(SEMP, 'bbbbbb'),
                          WAGP = na_if(WAGP, 'bbbbbb'),
                          PERNP = na_if(PERNP, 'bbbbbbb') )


# check for columns with too much missing data

#colMeans(is.na(acs_data))

acs_data %>% 
      summarize_all(funs(mean(is.na(.))))


# drop columns with too little data
acs_data <- acs_data %>% 
      select (-c(LANP, SCIENGP))


# factor recoding

CIT_levels <- c("Born in the U.S.", 
                "Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas",
                "Born abroad of American parent(s)",
                "U.S. citizen by naturalization",
                "Not a citizen of the U.S.")

HINS3_levels <- c("Yes", "No")

HINS4_levels <- c("Yes", "No")

SCHL_levels <- c("N/A (less than 3 years old)",
                 "No schooling completed",
                 "Nursery school, preschool",
                 "Kindergarten",
                 "Grade 1",
                 "Grade 2",
                 "Grade 3",
                 "Grade 4",
                 "Grade 5",
                 "Grade 6",
                 "Grade 7",
                 "Grade 8",
                 "Grade 9",
                 "Grade 10",
                 "Grade 11",
                 "12th grade - no diploma",
                 "Regular high school diploma",
                 "GED or alternative credential",
                 "Some college, but less than 1 year",
                 "1 or more years of college credit, no degree",
                 "Associate's degree",
                 "Bachelor's degree",
                 "Master's degree",
                 "Professional degree beyond a bachelor's degree",
                 "Doctorate degree")


SEMP_levels <- c("N/A (less than 15 years old)",
                 "None",
                 "Loss $1 to $10000",
                 "1 or break even",
                 "2 to $999999")

SEX_levels <- c("Male", "Female")


WAGP_levels <- c("N/A (less than 15 years old)",
                 "None",
                 "$1 to 999999 (Rounded and top-coded)")

DIS_levels <- c("With a disability",
                "Without a disability")

HICOV_levels <- c("With health insurance coverage",
                  "No health insurance coverage")

NATIVITY_levels <- c("Native",
                     "Foreign born")

PERNP_levels <- c("N/A (less than 15 years old)",
                  "No earnings",
                  "Loss of $10000 or more",
                  "Loss $1 to $9999",
                  "$1 or break even",
                  "$2 to $9999999")

PRIVCOV_levels <- c("With private health insurance coverage", 
                    "Without private health insurance coverage")

PUBCOV_levels <- c("With public health coverage",
                   "Without public health coverage")

RAC1P_levels <- c("White",
                  "Black or African American",
                  "American Indian",
                  "Alaska Native",
                  "American Indian and Alaska Native tribes",
                  "Asian",
                  "Native Hawaiian and Other Pacific Islander",
                  "Some Other Race",
                  "Two or More Races")


RACWHT_levels <- c("No", "Yes")

FCITP_levels <- c("No", "Yes")

FDISP_levels <- c("No", "Yes")


# factor variables
f_cols <- c("CIT", "HINS3", "HINS4", "SCHL", "SEX", "DIS", 
            "HICOV", "NATIVITY", "PRIVCOV", "PUBCOV", 
            "RAC1P", "RACWHT", "FCITP", "FDISP")

# convert to factors
acs_data[f_cols] <- lapply(acs_data[f_cols], factor)

# confirm
sapply(acs_data, class)


# recode 

acs_data <- acs_data %>%
                  mutate(CIT = fct_recode(CIT,
                                          "Born in the U.S." = "1",
                                          "Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas" = "2",
                                          "Born abroad of American parent(s)" = "3",
                                          "U.S. citizen by naturalization" = "4",
                                          "Not a citizen of the U.S." = "5"),
                        HINS3 = fct_recode(HINS3,
                                           "Yes" = "1",
                                           "No" = "2"),
                        HINS4 = fct_recode(HINS4,
                                           "Yes" = "1",
                                           "No" = "2"),
                        SCHL = fct_recode(SCHL,
                                          "No schooling completed" = "1",
                                          "Nursery school, preschool" = "2",
                                          "Kindergarten" = "3",
                                          "Grade 1" = "4",
                                          "Grade 2" = "5",
                                          "Grade 3" = "6",
                                          "Grade 4" = "7",
                                          "Grade 5" = "8",
                                          "Grade 6" = "9",
                                          "Grade 7" = "10",
                                          "Grade 8" = "11",
                                          "Grade 9" = "12",
                                          "Grade 10" = "13",
                                          "Grade 11" = "14",
                                          "12th grade - no diploma" = "15",
                                          "Regular high school diploma" = "16",
                                          "GED or alternative credential" = "17",
                                          "Some college, but less than 1 year" = "18",
                                          "1 or more years of college credit, no degree" = "19",
                                          "Associate's degree" = "20",
                                          "Bachelor's degree" = "21",
                                          "Master's degree" = "22",
                                          "Professional degree beyond a bachelor's degree" = "23",
                                          "Doctorate degree" = "24"),
                        SEX = fct_recode(SEX,
                                         "Male" = "1",
                                         "Female" = "2"),
                        DIS = fct_recode(DIS,
                                         "With a disability" = "1",
                                         "Without a disability" = "2"),
                        HICOV = fct_recode(HICOV, 
                                           "With health insurance coverage" = "1",
                                           "No health insurance coverage" = "2"), 
                        NATIVITY = fct_recode(NATIVITY, 
                                              "Native" = "1",
                                              "Foreign born" = "2"),
                        PRIVCOV = fct_recode(PRIVCOV,
                                             "With private health insurance coverage" = "1",
                                             "Without private health insurance coverage" = "2"),
                        PUBCOV = fct_recode(PUBCOV,
                                            "With public health coverage" = "1",
                                            "Without public health coverage" = "2"),
                        RAC1P = fct_recode(RAC1P,
                                           "White" = "1",
                                           "Black or African American" = "2",
                                           "American Indian or\nAlaskan Native" = "3",
                                           "American Indian or\nAlaskan Native" = "4",
                                           "American Indian or\nAlaskan Native" = "5",
                                           "Asian" = "6",
                                           "Native Hawaiian and\nOther Pacific Islander" = "7",
                                           "Some Other Race" = "8",
                                           "Two or More Races" = "9"),
                        RACWHT = fct_recode(RACWHT,
                                            "No" = "0",
                                            "Yes" = "1"), 
                        FCITP = fct_recode(FCITP,
                                           "No" = "0", 
                                           "Yes" = "1"), 
                        FDISP = fct_recode(FDISP,
                                           "No" = "0", 
                                           "Yes" = "1")
)
                                           
                  
## Change variable names

acs_data <- acs_data %>%
                  rename(Age = AGEP,
                         Citizenship = CIT,
                         Edu_Attainment = SCHL,
                         Sex = SEX,
                         Disability_Rec = DIS,
                         Health_Cover = HICOV,
                         Medicare = HINS3,
                         Medicade = HINS4,
                         Nativity = NATIVITY,
                         Priv_Cover = PRIVCOV,
                         Pub_Cover = PUBCOV,
                         Race = RAC1P,
                         White = RACWHT,
                         Pub_assist = PAP,
                         Yearly_Income = WAGP,
                         Self_Income = SEMP,
                         Personal_Earn = PERNP,
                         Retire_Income = RETP)

# add year column:
acs_data <- acs_data %>%
      mutate(Year = case_when(.$ADJINC == 1056030 ~ 2012,
                              .$ADJINC == 1038170 ~ 2013,
                              .$ADJINC == 1022342 ~ 2014,
                              .$ADJINC == 1013916 ~ 2015,
                              .$ADJINC == 1007588 ~ 2016))
                              


## Adjust income variables to be consistent with 2016 values

acs_data <- acs_data %>%
      mutate(Pub_assist = ifelse(Pub_assist > 0, Pub_assist * (ADJINC / 1000000), Pub_assist),
             Yearly_Income = ifelse(Yearly_Income > 0, Yearly_Income * (ADJINC / 1000000), Yearly_Income),
             Self_Income = ifelse(Self_Income > 0, Self_Income * (ADJINC / 1000000), Self_Income),
             Personal_Earn = ifelse(Personal_Earn > 0, Personal_Earn * (ADJINC / 1000000), Personal_Earn),
             Retire_Income = ifelse(Retire_Income > 0, Retire_Income *  (ADJINC / 1000000), Retire_Income))
             
            

## Give native groups one title for ease of comparison:

#acs_data <- acs_data %>%
#      mutate(Race = ifelse(Race == "American Indian" | .$Race == "Alaska Native" | .$Race == "American Indian and Alaska Native tribes",
#                              "American Indian or\nAlaskan Native",
#                              ifelse(Race == "Native Hawaiian and Other Pacific Islander", 
#                                          "Native Hawaiian and\nOther Pacific Islander", 
#                                                Race)))




### >>>  Tabular and graphical data summaires  <<< ###

# Proper labelling of figure axes and table columns
# Discussion of the graphical and tabular summaries.

summary(acs_data)

## tables and summary graphs
      
 ## >>> Race  <<< ##

 # race count
race_count <- acs_data %>% count(Race)

kable(race_count, format = "markdown")

 # mean income by race
race_income <- with(acs_data, tapply(Yearly_Income, 
                               INDEX = list(Race, Sex), FUN = mean, na.rm = TRUE))
kable(race_income, format = "markdown")


# for plotting
race_income_agg <- aggregate(Yearly_Income ~ Race + Sex, FUN = mean, na.rm=TRUE, data= acs_data)

# all peoples
income_race_p1 <- ggplot(race_income_agg, aes(y = Yearly_Income, x = Race, fill = Sex)) +
                        geom_bar(stat = "identity", position = "dodge", colour = "grey40") +
                        labs(y = "Mean Annual Income (USD$)\n",
                             x = "\nRace",
                             title = "Mean Annual Income by Race, Gender",
                             subtitle = "* Individuals with no income included",
                             fill = "Gender") +
                        scale_fill_ptol() +
                        theme_hc() +
                        theme(plot.subtitle =element_text(face = "italic"))


# filter only those with salaries:
income_earners <- filter(acs_data, Yearly_Income > 0)
race_earners_agg <- aggregate(Yearly_Income ~ Race + Sex, FUN = mean, na.rm=TRUE, data= income_earners)

# only earners
income_race_p2 <-ggplot(race_earners_agg, aes(y = Yearly_Income, x = Race, fill = Sex)) +
                        geom_bar(stat = "identity", position = "dodge", colour = "grey40") +
                        labs(y = "Mean Annual Income (USD$)\n",
                             x = "\nRace",
                             title = "Mean Annual Income by Race, Gender",
                             subtitle = "* Individuals with no income discluded",
                             fill = "Gender") +
                        scale_fill_ptol() +
                        theme_hc() +
                        theme(plot.subtitle =element_text(face = "italic"))


# combine into one plot

grid.arrange(income_race_p1,
             income_race_p2,
             ncol = 2)
             


## Retirement income by race

retire_income <- filter(acs_data, Retire_Income > 0)
# reorder for legend to match
retire_income$Race <- with(retire_income, reorder(Race, Retire_Income))

# ggplot(retire_income, aes(x=reorder(Race, Retire_Income), y=Retire_Income, fill=Race)) +
#       geom_violin(alpha=0.6) +
#       labs( x = "",
#             fill = "Region\n",
#             y = "Life expectancy (in years)\n",
#             title = "Regional life expectancy",
#             subtitle = "2015") +
#       scale_y_continuous(limits = c(0, 50000)) +
#       theme_minimal()+ 
#       theme(text = element_text(size=11,  family="Calibri (Body)")) +
#       scale_color_few()

# boxplot - retirement income by race
ggplot(retire_income, aes(x=Race, Retire_Income, y=Retire_Income, fill=Race)) +
      geom_boxplot() + 
            labs(title="Retirement Income by Race", 
                 subtitle="Descending order based on mean retirement income\n",
                 x="",
                 y="\nRetirement Income (USD$)") +
      scale_y_continuous(limits = c(0, 100000)) +
      theme_minimal()+ 
      theme(text = element_text(size=11,  family="Calibri (Body)")) +
      scale_fill_gdocs() + 
      guides(fill = guide_legend(reverse = TRUE)) +
      coord_flip()

# boxplot - retirement income by race, gender
ggplot(retire_income, aes(x=Race, Retire_Income, y=Retire_Income, fill=Sex)) +
      geom_boxplot() + 
      labs(title="Retirement Income by Race, Gender", 
           subtitle="Descending order based on mean retirement income\n",
           x="",
           y="\nRetirement Income (USD$)",
           fill = "Gender") +
      scale_y_continuous(limits = c(0, 100000)) +
      theme_minimal()+ 
      theme(text = element_text(size=11,  family="Calibri (Body)")) +
      scale_fill_gdocs() + 
      guides(fill = guide_legend(reverse = TRUE)) +
      coord_flip()




# self income by native or foreign born across age

#  only self earners
only_self_income <- filter(acs_data, Self_Income > 0)

ggplot(only_self_income,
       aes(x=Age, y=Self_Income, colour=Nativity)) +
        geom_smooth(aes(col=Nativity), se=F) +
        geom_smooth(aes(col=Nativity), method = 'lm', se=F, linetype = "dotted") +
      labs(title="Self Income by Nativity", 
           subtitle="United States native vs. foreign born\n",
           x="Age",
           y="Self Income (USD$)\n",
           colour = "") +
      scale_y_continuous(breaks = c(10000, 20000, 30000, 40000, 50000)) +
      xlim(20, 85) +
      scale_color_fivethirtyeight() +
      theme_hc()





#  composition of educational attainment


# reduce number of factor levels for plotting
adjust_edu <- acs_data %>%
                  select(Edu_Attainment) %>%
                  mutate(Edu_good = case_when(.$Edu_Attainment == "No schooling completed" ~ "No schooling completed",
                                              .$Edu_Attainment == "Nursery school, preschool" ~ "Nursery school, preschool",
                                              .$Edu_Attainment == "Kindergarten" ~ "Kindergarten",
                                              .$Edu_Attainment == "Grade 1" | .$Edu_Attainment == "Grade 2" | .$Edu_Attainment == "Grade 3" | 
                                                    .$Edu_Attainment == "Grade 4" | .$Edu_Attainment == "Grade 5" | .$Edu_Attainment == "Grade 6" | 
                                                    .$Edu_Attainment == "Grade 7" | .$Edu_Attainment == "Grade 8"  ~ "Grade School",
                                              .$Edu_Attainment == "Grade 9" | .$Edu_Attainment == "Grade 10" | .$Edu_Attainment == "Grade 11" | 
                                                    .$Edu_Attainment == "12th grade - no diploma"~ "Some Highschool",
                                              .$Edu_Attainment == "Regular high school diploma" | .$Edu_Attainment == "GED or alternative credential" ~ "High School Diploma or GED",
                                              .$Edu_Attainment == "Some college, but less than 1 year" | .$Edu_Attainment == "1 or more years of college credit, no degree" ~ "Some College",
                                              .$Edu_Attainment == "Associate's degree" ~ "Associate's degree",
                                              .$Edu_Attainment == "Bachelor's degree" ~ "Bachelor's degree",
                                              .$Edu_Attainment == "Master's degree" ~ "Master's degree",
                                              .$Edu_Attainment == "Professional degree beyond a bachelor's degree" ~ "Professional degree",
                                              .$Edu_Attainment == "Doctorate degree" ~ "Doctorate degree")) %>%
      drop_na()


var <- adjust_edu$Edu_good  # the fixed data
var <- as.factor(var)


## Prep data (nothing to change here)
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table[5] <- 24  # increasing must common to be out of 100 (only adding 1)

df$category <- factor(rep(names(categ_table), categ_table))  
# NOTE: if sum(categ_table) is not 100 (i.e. nrows^2), it will need adjustment to make the sum to 100.

## Plot
ggplot(df, aes(x = x, y = y, fill = category)) + 
      geom_tile(color = "black", size = 0.5, alpha = 0.75) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
      scale_fill_stata()+
      labs(title="Education Breakdown", 
           subtitle="Based on educational attainment (composition of whole)\n",
           fill = "Education Level",
           x = "",
           y = "") +
      theme_few()



## educational attainment by white vs non-white

# reduce number of factor levels for plotting
adjust_edu_2 <- acs_data %>%
      select(Edu_Attainment, White, Nativity) %>%
      mutate(Edu_good = case_when(.$Edu_Attainment == "No schooling completed" ~ "No schooling completed",
                                  .$Edu_Attainment == "Nursery school, preschool" ~ "Nursery school, preschool",
                                  .$Edu_Attainment == "Kindergarten" ~ "Kindergarten",
                                  .$Edu_Attainment == "Grade 1" | .$Edu_Attainment == "Grade 2" | .$Edu_Attainment == "Grade 3" | 
                                        .$Edu_Attainment == "Grade 4" | .$Edu_Attainment == "Grade 5" | .$Edu_Attainment == "Grade 6" | 
                                        .$Edu_Attainment == "Grade 7" | .$Edu_Attainment == "Grade 8"  ~ "Grade School",
                                  .$Edu_Attainment == "Grade 9" | .$Edu_Attainment == "Grade 10" | .$Edu_Attainment == "Grade 11" | 
                                        .$Edu_Attainment == "12th grade - no diploma"~ "Some Highschool",
                                  .$Edu_Attainment == "Regular high school diploma" | .$Edu_Attainment == "GED or alternative credential" ~ "High School Diploma or GED",
                                  .$Edu_Attainment == "Some college, but less than 1 year" | .$Edu_Attainment == "1 or more years of college credit, no degree" ~ "Some College",
                                  .$Edu_Attainment == "Associate's degree" ~ "Associate's degree",
                                  .$Edu_Attainment == "Bachelor's degree" ~ "Bachelor's degree",
                                  .$Edu_Attainment == "Master's degree" ~ "Master's degree",
                                  .$Edu_Attainment == "Professional degree beyond a bachelor's degree" ~ "Professional degree",
                                  .$Edu_Attainment == "Doctorate degree" ~ "Doctorate degree")) %>%
      drop_na()



#reorder(Race, Retire_Income) 


options(scipen = 9999)

ggplot(adjust_edu_2, aes(Edu_good)) +
      geom_bar(aes(fill=White), width = 0.5) + 
            theme(axis.text.x = element_text(angle=15, vjust=0.7)) + 
            labs(title="Educational Attainment Counts", 
                  subtitle="By white vs. non-white",
                 fill = "White?",
                 x = "Education Level") +
      theme_hc() +
      scale_fill_ptol()


### statistical graphs

# corplot


# re read data for numeric
acs_data_1 <- fread("ss16pusa.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

acs_data_2 <- fread("ss16pusb.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

acs_data_3 <- fread("ss16pusc.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

acs_data_4 <- fread("ss16pusd.csv", 
                    header = TRUE, 
                    select = keep_cols, 
                    verbose = TRUE)

#combine data sets by rows:
acs_data_corr <- data.table(rbind(acs_data_1, acs_data_2, acs_data_3, acs_data_4))

# remove data sets from workspace:
rm(acs_data_1, acs_data_2, acs_data_3, acs_data_4)


# remove non binary factor variables. 
corr_vars <- acs_data_corr %>%
      select(-c(LANP, SCIENGP, RAC1P, SCHL, CIT)) %>%
      data.frame()


# factor variables
f_cols_corr <- c("HINS3", "HINS4","SEX", "DIS", 
            "HICOV", "NATIVITY", "PRIVCOV", "PUBCOV", 
            "RACWHT", "FCITP", "FDISP")

# convert to factors
corr_vars[f_cols_corr] <- lapply(corr_vars[f_cols_corr], factor)


# recode for point-biserial correlation to work
corr_vars <- corr_vars %>%
      mutate(HINS3 = fct_recode(HINS3,
                                "0" = "1",
                                "1" = "2"),
             HINS4 = fct_recode(HINS4,
                                "0" = "1",
                                "1" = "2"),
             SEX = fct_recode(SEX,
                              "0" = "1",
                              "1" = "2"),
             DIS = fct_recode(DIS,
                              "0" = "1",
                              "1" = "2"),
             HICOV = fct_recode(HICOV, 
                                "0" = "1",
                                "1" = "2"), 
             NATIVITY = fct_recode(NATIVITY, 
                                   "0" = "1",
                                   "1" = "2"),
             PRIVCOV = fct_recode(PRIVCOV,
                                  "0" = "1",
                                  "1" = "2"),
             PUBCOV = fct_recode(PUBCOV,
                                 "0" = "1",
                                 "1" = "2")
      )

#convert back to int for corr
corr_vars <- lapply(corr_vars, as.integer) 
# convert back to dframe
corr_vars <- data.frame(corr_vars)

#rename vars
corr_vars <- corr_vars %>%
      rename(Age = AGEP,
             Sex = SEX,
             Disability_Rec = DIS,
             Health_Cover = HICOV,
             Medicare = HINS3,
             Medicade = HINS4,
             Nativity = NATIVITY,
             Priv_Cover = PRIVCOV,
             Pub_Cover = PUBCOV,
             White = RACWHT,
             Pub_assist = PAP,
             Yearly_Income = WAGP,
             Self_Income = SEMP,
             Personal_Earn = PERNP,
             Retire_Income = RETP)


# adjust income variables 
corr_vars <- corr_vars %>%
      mutate(Pub_assist = ifelse(Pub_assist > 0, Pub_assist * (ADJINC / 1000000), Pub_assist),
             Yearly_Income = ifelse(Yearly_Income > 0, Yearly_Income * (ADJINC / 1000000), Yearly_Income),
             Self_Income = ifelse(Self_Income > 0, Self_Income * (ADJINC / 1000000), Self_Income),
             Personal_Earn = ifelse(Personal_Earn > 0, Personal_Earn * (ADJINC / 1000000), Personal_Earn),
             Retire_Income = ifelse(Retire_Income > 0, Retire_Income *  (ADJINC / 1000000), Retire_Income))

# filter only earners in all salary categories
#corr_vars <- corr_vars %>%
#     filter(Pub_assist > 0 & Yearly_Income > 0 & Self_Income > 0 & Personal_Earn > 0 & Retire_Income > 0)

# must replace 0's with NA's - best option by far. Data cannot be worked with prior to doing this
corr_vars <- corr_vars %>% mutate(Pub_assist = replace(Pub_assist, which(Pub_assist == 0L), NA),
                                  Yearly_Income = replace(Yearly_Income, which(Yearly_Income == 0L), NA),
                                  Self_Income = replace(Self_Income, which(Self_Income == 0L), NA),
                                  Personal_Earn = replace(Personal_Earn, which(Personal_Earn == 0L), NA),
                                  Retire_Income = replace(Retire_Income, which(Retire_Income == 0L), NA))



## correlation assumptions


# normality of numeric vars

# age
hist.Age <- ggplot(corr_vars, aes(Age)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey90") +
      labs(x="Age", y = "Density", title = "Age Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$Age, na.rm = TRUE), sd = sd(corr_vars$Age, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()


# public assistance
hist.Pub_assist <- ggplot(corr_vars, aes(Pub_assist)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Public Assistance", y = "Density", title = "Public Assistance Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$Pub_assist, na.rm = TRUE), sd = sd(corr_vars$Pub_assist, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()


# retirement income
hist.Retire_Income <- ggplot(corr_vars, aes(Retire_Income)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Public Assistance", y = "Density", title = "Retirement Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$Retire_Income, na.rm = TRUE), sd = sd(corr_vars$Retire_Income, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()


# self income
hist.Self_Income <- ggplot(corr_vars, aes(Self_Income)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Self Income", y = "Density", title = "Self Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$Self_Income, na.rm = TRUE), sd = sd(corr_vars$Self_Income, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()


# yearly income
hist.Yearly_Income <- ggplot(corr_vars, aes(Yearly_Income)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Yearly Income", y = "Density", title = "Yearly Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$Yearly_Income, na.rm = TRUE), sd = sd(corr_vars$Yearly_Income, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()

# personal earnings
hist.Personal_Earn <- ggplot(corr_vars, aes(log(Personal_Earn))) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Personal Earnings", y = "Density", title = "Personal Earnings Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$Personal_Earn, na.rm = TRUE), sd = sd(corr_vars$Personal_Earn, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()


## all violate normality except for age. Outliers must be dealt with

# option 1: remove outliers
# option 2: use robust approach
# option 3: transform the data

# will go with option 1 and 3

 # remove Pub_assist outliers
 pub_outliers <- quantile(corr_vars$Pub_assist, na.rm = T)[[4]] + (1.5 * IQR(corr_vars$Pub_assist, na.rm = T)) 
 
 corr_vars <- corr_vars %>%
      mutate(Pub_assist = ifelse(Pub_assist > pub_outliers, NA, Pub_assist))
# 
# 
# Pub_assist <- ggplot(corr_vars, aes(Pub_assist)) + 
#       theme(legend.position = "none") + 
#       geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
#       labs(x="Public Assistance", y = "Density", title = "Public Assistance Distribution\n")+
#       stat_function(fun = dnorm, 
#                     args = list(mean = mean(corr_vars$Pub_assist, na.rm = TRUE), sd = sd(corr_vars$Pub_assist, na.rm = TRUE)), 
#                     colour = "black", size = 1) +
#       theme_hc()


## option 3
# because the data are positively skewed, I will log or sqaure root transform

corr_vars$log_Pub_assist <- log(corr_vars$Pub_assist)

# sqrt looks better
corr_vars$sqrt_Pub_assist <- sqrt(corr_vars$Pub_assist)

hist.sqrt_Pub_assist <- ggplot(corr_vars, aes(sqrt_Pub_assist)) + 
                               theme(legend.position = "none") + 
                               geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
                               labs(x="Public Assistance (Square root)", y = "Density", title = "Public Assistance Distribution\n")+
                               stat_function(fun = dnorm, 
                                             args = list(mean = mean(corr_vars$sqrt_Pub_assist, na.rm = TRUE), sd = sd(corr_vars$sqrt_Pub_assist, na.rm = TRUE)), 
                                            colour = "black", size = 1) +
                               theme_hc()

# much better



# remove Retire_Income outliers
reture_outliers <- quantile(corr_vars$Retire_Income, na.rm = T)[[4]] + (1.5 * IQR(corr_vars$Retire_Income, na.rm = T)) 

corr_vars <- corr_vars %>%
      mutate(Retire_Income = ifelse(Retire_Income > reture_outliers, NA, Retire_Income))

# transform

corr_vars$log_Retire_Income <- log(corr_vars$Retire_Income)

# sqrt 
corr_vars$sqrt_Retire_Income <- sqrt(corr_vars$Retire_Income)


hist.sqrt_Retire_Income<- ggplot(corr_vars, aes(sqrt_Retire_Income)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Retirement Income (Square Root)", y = "Density", title = "Retirement Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$sqrt_Retire_Income, na.rm = TRUE), sd = sd(corr_vars$sqrt_Retire_Income, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()




# remove Self_Income outliers
self_outliers <- quantile(corr_vars$Self_Income, na.rm = T)[[4]] + (1.5 * IQR(corr_vars$Self_Income, na.rm = T)) 

corr_vars <- corr_vars %>%
      mutate(Self_Income = ifelse(Self_Income > self_outliers, NA, Self_Income))

# transform

corr_vars$log_Self_Income <- log(corr_vars$Self_Income)

# sqrt 
corr_vars$sqrt_Self_Income <- sqrt(corr_vars$Self_Income)


hist.sqrt_Self_Income <- ggplot(corr_vars, aes(sqrt_Self_Income)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Self Income (Square Root)", y = "Density", title = "Self Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$sqrt_Self_Income, na.rm = TRUE), sd = sd(corr_vars$sqrt_Self_Income, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()




# remove Yearly_Income outliers
yearly_outliers <- quantile(corr_vars$Yearly_Income, na.rm = T)[[4]] + (1.5 * IQR(corr_vars$Yearly_Income, na.rm = T)) 

corr_vars <- corr_vars %>%
      mutate(Yearly_Income = ifelse(Yearly_Income > yearly_outliers, NA, Yearly_Income))

# transform

corr_vars$log_Yearly_Income <- log(corr_vars$Yearly_Income)

# sqrt 
corr_vars$sqrt_Yearly_Income <- sqrt(corr_vars$Yearly_Income)


hist.sqrt_Yearly_Income<- ggplot(corr_vars, aes(sqrt_Yearly_Income)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Yearly Income (Square Root)", y = "Density", title = "Yearly Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$sqrt_Yearly_Income, na.rm = TRUE), sd = sd(corr_vars$sqrt_Yearly_Income, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()





# remove Personal_Earn outliers
personal_outliers <- quantile(corr_vars$Personal_Earn, na.rm = T)[[4]] + (1.5 * IQR(corr_vars$Personal_Earn, na.rm = T)) 

corr_vars <- corr_vars %>%
      mutate(Personal_Earn = ifelse(Personal_Earn > personal_outliers, NA, Personal_Earn))

# transform

corr_vars$log_Personal_Earn <- log(corr_vars$Personal_Earn)

# sqrt 
corr_vars$sqrt_Personal_Earn <- sqrt(corr_vars$Personal_Earn)


hist.sqrt_Personal_Earn<- ggplot(corr_vars, aes(sqrt_Personal_Earn)) + 
      theme(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="grey40") +
      labs(x="Personal Income (Square Root)", y = "Density", title = "Personal Income Distribution\n")+
      stat_function(fun = dnorm, 
                    args = list(mean = mean(corr_vars$sqrt_Personal_Earn, na.rm = TRUE), sd = sd(corr_vars$sqrt_Personal_Earn, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      theme_hc()






rm(adjust_edu)
rm(adjust_edu_2)
rm(acs_data)





## hists and qq-plots before and after transformation side-by-side


  # pub assistance hists
grid.arrange(hist.Pub_assist,
             hist.sqrt_Pub_assist,
             ncol = 2)

rm(hist.Pub_assist)
rm(hist.sqrt_Pub_assist)

 # pub assistance qqplots
qq.Pub_assist <- qqnorm(corr_vars$Pub_assist,
                        # xlim = c(-4,4),  
                        #ylim = c(-4,4),  
                        xlab = "Theoretical Quantiles", 
                        ylab = "Sample Quantiles Pub_assist")
qqline(corr_vars$Pub_assist)


qq.sqrt_Pub_assist <- qqnorm(corr_vars$sqrt_Pub_assist,
                             # xlim = c(-4,4),  
                             #ylim = c(-4,4),  
                             xlab = "Theoretical Quantiles", 
                             ylab = "Sample Quantiles Pub_assist (Square root")
qqline(corr_vars$sqrt_Pub_assist)






  # retirement income hists
grid.arrange(hist.Retire_Income,
             hist.sqrt_Retire_Income,
             ncol = 2)


rm(hist.Retire_Income)
rm(hist.sqrt_Retire_Income)


 # retirement income qqplots
qq.Retire_Income <- qqnorm(corr_vars$Retire_Income,
                        # xlim = c(-4,4),  
                        #ylim = c(-4,4),  
                        xlab = "Theoretical Quantiles", 
                        ylab = "Sample Quantiles Retire_Income")
qqline(corr_vars$Retire_Income)


qq.sqrt_Retire_Income <- qqnorm(corr_vars$sqrt_Retire_Income,
                             # xlim = c(-4,4),  
                             #ylim = c(-4,4),  
                             xlab = "Theoretical Quantiles", 
                             ylab = "Sample Quantiles Retire_Income(Square root")
qqline(corr_vars$sqrt_Retire_Income)






 # self  income hists
grid.arrange(hist.Self_Income,
             hist.sqrt_Self_Income,
             ncol = 2)

rm(hist.Self_Income)
rm(hist.sqrt_Self_Income)


 # self income qqplots
qq.Self_Income<- qqnorm(corr_vars$Self_Income,
                           # xlim = c(-4,4),  
                           #ylim = c(-4,4),  
                           xlab = "Theoretical Quantiles", 
                           ylab = "Sample Quantiles Self_Income")
qqline(corr_vars$Self_Income)


qq.sqrt_Self_Income <- qqnorm(corr_vars$sqrt_Self_Income,
                                # xlim = c(-4,4),  
                                #ylim = c(-4,4),  
                                xlab = "Theoretical Quantiles", 
                                ylab = "Sample Quantiles Self_Income(Square root")
qqline(corr_vars$sqrt_Self_Income)






 # yearly income hists
grid.arrange(hist.Yearly_Income,
             hist.sqrt_Yearly_Income,
             ncol = 2)

rm(hist.Yearly_Income)
rm(hist.sqrt_Yearly_Income)


# yearly income qqplots
qq.Yearly_Income<- qqnorm(corr_vars$Yearly_Income,
                        # xlim = c(-4,4),  
                        #ylim = c(-4,4),  
                        xlab = "Theoretical Quantiles", 
                        ylab = "Sample Quantiles Yearly_Income")
qqline(corr_vars$Yearly_Income)


qq.sqrt_Yearly_Income <- qqnorm(corr_vars$sqrt_Yearly_Income,
                              # xlim = c(-4,4),  
                              #ylim = c(-4,4),  
                              xlab = "Theoretical Quantiles", 
                              ylab = "Sample Quantiles Yearly_Income(Square root")
qqline(corr_vars$sqrt_Yearly_Income)






 # personal earnings hists
grid.arrange(hist.Personal_Earn,
             hist.sqrt_Personal_Earn,
             ncol = 2)


 # peronal earnings qqplots
qq.Personal_Earn<- qqnorm(corr_vars$Personal_Earn,
                          # xlim = c(-4,4),  
                          #ylim = c(-4,4),  
                          xlab = "Theoretical Quantiles", 
                          ylab = "Sample Quantiles Personal_Earn")
qqline(corr_vars$Personal_Earn)


qq.sqrt_Personal_Earn <- qqnorm(corr_vars$sqrt_Personal_Earn,
                                # xlim = c(-4,4),  
                                #ylim = c(-4,4),  
                                xlab = "Theoretical Quantiles", 
                                ylab = "Sample Quantiles Personal_Earn(Square root")
qqline(corr_vars$sqrt_Personal_Earn)

# big improvments made ot normality



#### linearity

## cant really test this due to data size

# pairs(corr_vars[,c(20, 22, 24, 25, 28)], pch = 19)

#  yearly income and personal earn show near perfect multicolinearity !!!


# interval or better  -- yes



# subset of only sqrt vars and binaries
corr_vars_2 <- corr_vars %>%
      select(-c(Pub_assist, Retire_Income, Self_Income, Yearly_Income, Personal_Earn,
                log_Pub_assist, log_Retire_Income, log_Self_Income, log_Yearly_Income, log_Personal_Earn))

# coefficient of determination R2 - pearon and point-biserial
acs_cors <- cor(corr_vars_2, use="pairwise.complete.obs")^2 * 100  #pairwise deletion

kable(acs_cors, format = "markdown")


corr <- round(cor(corr_vars_2, use="pairwise.complete.obs"), 1)



ggcorrplot(corr,
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Bivariate correlations of ACS data", 
           ggtheme=theme_bw)



## DISCUSS WHAT YOU SEE




#### REGRESSION

# stepwise selection before assumptions

# can do multiplicolinearity - personal ear and yearly income too correlated

regress_vars <- acs_data_corr %>%
      select(-c(LANP, SCIENGP)) %>%
      data.frame()


# factor variables
f_cols_regress <- c("RAC1P", "SCHL", "CIT",
                 "HINS3", "HINS4","SEX", "DIS", 
                 "HICOV", "NATIVITY", "PRIVCOV", "PUBCOV", 
                 "RACWHT", "FCITP", "FDISP")

# convert to factors
regress_vars[f_cols_regress] <- lapply(regress_vars[f_cols_regress], factor)


# # recode for point-biserial correlation to work
# corr_vars <- corr_vars %>%
#       mutate(HINS3 = fct_recode(HINS3,
#                                 "0" = "1",
#                                 "1" = "2"),
#              HINS4 = fct_recode(HINS4,
#                                 "0" = "1",
#                                 "1" = "2"),
#              SEX = fct_recode(SEX,
#                               "0" = "1",
#                               "1" = "2"),
#              DIS = fct_recode(DIS,
#                               "0" = "1",
#                               "1" = "2"),
#              HICOV = fct_recode(HICOV, 
#                                 "0" = "1",
#                                 "1" = "2"), 
#              NATIVITY = fct_recode(NATIVITY, 
#                                    "0" = "1",
#                                    "1" = "2"),
#              PRIVCOV = fct_recode(PRIVCOV,
#                                   "0" = "1",
#                                   "1" = "2"),
#              PUBCOV = fct_recode(PUBCOV,
#                                  "0" = "1",
#                                  "1" = "2")
#       )
# 
# #convert back to int for corr
# corr_vars <- lapply(corr_vars, as.integer) 
# convert back to dframe
regress_vars <- data.frame(regress_vars)

#rename vars
regress_vars <- regress_vars %>%
      rename(Race = RAC1P,
             Educational_attainment = SCHL,
             Citizenship = CIT, 
             Age = AGEP,
             Sex = SEX,
             Disability_Rec = DIS,
             Health_Cover = HICOV,
             Medicare = HINS3,
             Medicade = HINS4,
             Nativity = NATIVITY,
             Priv_Cover = PRIVCOV,
             Pub_Cover = PUBCOV,
             White = RACWHT,
             Pub_assist = PAP,
             Yearly_Income = WAGP,
             Self_Income = SEMP,
             Personal_Earn = PERNP,
             Retire_Income = RETP)


# adjust income variables 
regress_vars <- regress_vars %>%
      mutate(Pub_assist = ifelse(Pub_assist > 0, Pub_assist * (ADJINC / 1000000), Pub_assist),
             Yearly_Income = ifelse(Yearly_Income > 0, Yearly_Income * (ADJINC / 1000000), Yearly_Income),
             Self_Income = ifelse(Self_Income > 0, Self_Income * (ADJINC / 1000000), Self_Income),
             Personal_Earn = ifelse(Personal_Earn > 0, Personal_Earn * (ADJINC / 1000000), Personal_Earn),
             Retire_Income = ifelse(Retire_Income > 0, Retire_Income *  (ADJINC / 1000000), Retire_Income))

# filter only earners in all salary categories
#corr_vars <- corr_vars %>%
#     filter(Pub_assist > 0 & Yearly_Income > 0 & Self_Income > 0 & Personal_Earn > 0 & Retire_Income > 0)

# must replace 0's with NA's - best option by far. Data cannot be worked with prior to doing this
regress_vars <- regress_vars %>% mutate(Pub_assist = replace(Pub_assist, which(Pub_assist == 0L), NA),
                                  Yearly_Income = replace(Yearly_Income, which(Yearly_Income == 0L), NA),
                                  Self_Income = replace(Self_Income, which(Self_Income == 0L), NA),
                                  Personal_Earn = replace(Personal_Earn, which(Personal_Earn == 0L), NA),
                                  Retire_Income = replace(Retire_Income, which(Retire_Income == 0L), NA))



rm(corr_vars)
rm(acs_data)
rm(acs_data_corr)
rm(hist.sqrt_Personal_Earn)
rm(qq.sqrt_Personal_Earn)
rm(qq.sqrt_Pub_assist)
rm(qq.sqrt_Yearly_Income)
rm(qq.Personal_Earn)
rm(qq.Pub_assist)
rm(qq.Retire_Income)
rm(qq.Self_Income)
rm(qq.sqrt_Retire_Income)
rm(qq.sqrt_Self_Income)
rm(qq.Yearly_Income)
rm(retire_income)
rm(pub_corr_vars)
rm(only_self_income)
rm(income_earners)


# remove Pub_assist outliers
pub_outliers <- quantile(regress_vars$Pub_assist, na.rm = T)[[4]] + (1.5 * IQR(regress_vars$Pub_assist, na.rm = T)) 

regress_vars <- regress_vars %>%
      mutate(Pub_assist = ifelse(Pub_assist > pub_outliers, NA, Pub_assist))

# sqrt looks better
regress_vars$sqrt_Pub_assist <- sqrt(regress_vars$Pub_assist)



# remove Retire_Income outliers
reture_outliers <- quantile(regress_vars$Retire_Income, na.rm = T)[[4]] + (1.5 * IQR(regress_vars$Retire_Income, na.rm = T)) 

regress_vars <- regress_vars %>%
      mutate(Retire_Income = ifelse(Retire_Income > reture_outliers, NA, Retire_Income))

# transform
# sqrt 
regress_vars$sqrt_Retire_Income <- sqrt(regress_vars$Retire_Income)




# remove Self_Income outliers
self_outliers <- quantile(regress_vars$Self_Income, na.rm = T)[[4]] + (1.5 * IQR(regress_vars$Self_Income, na.rm = T)) 

regress_vars <- regress_vars %>%
      mutate(Self_Income = ifelse(Self_Income > self_outliers, NA, Self_Income))

# sqrt 
regress_vars$sqrt_Self_Income <- sqrt(regress_vars$Self_Income)




# remove Yearly_Income outliers
yearly_outliers <- quantile(regress_vars$Yearly_Income, na.rm = T)[[4]] + (1.5 * IQR(regress_vars$Yearly_Income, na.rm = T)) 

regress_vars <- regress_vars %>%
      mutate(Yearly_Income = ifelse(Yearly_Income > yearly_outliers, NA, Yearly_Income))


# sqrt 
regress_vars$sqrt_Yearly_Income <- sqrt(regress_vars$Yearly_Income)


# remove Personal_Earn outliers
personal_outliers <- quantile(regress_vars$Personal_Earn, na.rm = T)[[4]] + (1.5 * IQR(regress_vars$Personal_Earn, na.rm = T)) 

regress_vars <- regress_vars %>%
      mutate(Personal_Earn = ifelse(Personal_Earn > personal_outliers, NA, Personal_Earn))


# sqrt 
regress_vars$sqrt_Personal_Earn <- sqrt(regress_vars$Personal_Earn)



# take a smaller sample
reg_sample <- slice(regress_vars, nrow(regress_vars) - 6000:nrow(regress_vars))
# no sqrt vars
regress_vars_base <- reg_sample %>%
      select(-c("sqrt_Pub_assist","sqrt_Retire_Income",    
                "sqrt_Self_Income","sqrt_Yearly_Income","sqrt_Personal_Earn",
                "ADJINC", "FCITP","FDISP", "Personal_Earn", "Race"))
regress_vars_base
# only sqrt vars
regress_vars_sqrt <- reg_sample %>%
      select(-c("Pub_assist", "Retire_Income", "Self_Income",
                "Yearly_Income", "Personal_Earn", "ADJINC", "FCITP","FDISP","sqrt_Personal_Earn",
                "ADJINC", "FCITP","FDISP", "Personal_Earn", "Race"))


rm(reg_sample)
rm(regress_vars)


dep_vars_sqrt <- c(Age,                   
                   Citizenship,Educational_attainment, Sex,                   
                   Disability_Rec,Health_Cover,Medicare,              
                   Medicade,Nativity,Priv_Cover,            
                   Pub_Cover,Race,White,                 
                   FCITP,FDISP,sqrt_Pub_assist,       
                   sqrt_Retire_Income,sqrt_Self_Income,sqrt_Yearly_Income,    
                   sqrt_Personal_Earn)


# everything - sqrt data
income_model_sqrt <- lm(sqrt_Yearly_Income ~ Age + Citizenship + Educational_attainment +Sex +                   
                                          Disability_Rec + Health_Cover + Medicare +              
                                          Medicade + Nativity + Priv_Cover +             
                                          Pub_Cover  + White + sqrt_Pub_assist  +       
                                          sqrt_Retire_Income + sqrt_Self_Income,
                   data = regress_vars_sqrt, weights = PWGTP)

#summary(income_model_sqrt)




# everything - normal data
income_model <- lm(Yearly_Income ~ Age + Citizenship +Sex +                   
                              Disability_Rec + Health_Cover + Medicare +              
                              Medicade + Nativity + Priv_Cover +             
                              Pub_Cover  + White + Pub_assist  +       
                              Retire_Income + Self_Income + Educational_attainment, 
                        data = regress_vars_base, weights = PWGTP)

#summary(income_model)





# sqrt model
# step wise selection - removal based on aic
ols_step_forward_aic(income_model_sqrt, details = TRUE)


final_model_sqrt <- lm(sqrt_Yearly_Income ~ sqrt_Pub_assist  + sqrt_Retire_Income + sqrt_Self_Income   +                   
                        Citizenship + Pub_Cover + Medicade + Priv_Cover,
                  data = regress_vars_sqrt, weights = PWGTP, na.action = na.exclude)


summary(final_model_sqrt)



# non-sqrt model
# step wise selection - removal based on aic
ols_step_forward_aic(income_model, details = TRUE)


final_model <- lm(Yearly_Income ~ Pub_assist  + Retire_Income +Self_Income   +                   
                        Citizenship + Priv_Cover,
                  data = regress_vars_base, weights = PWGTP)

summary(final_model)




###  square root model fits slightly better


# neary normal residuals

final_model_sqrt$xlevels[["Citizenship"]] <- union(final_model_sqrt$xlevels[["Citizenship"]], levels(regress_vars_sqrt$Citizenship))

regress_vars_sqrt$predicted = round(predict(final_model_sqrt, 
                                            newdata = regress_vars_sqrt, 
                                            na.action = na.pass),3)


regress_vars_sqrt$residuals<-  round(resid(final_model_sqrt, 
                                          newdata = regress_vars_sqrt, 
                                          na.action = na.pass),3) 


ggplot(regress_vars_sqrt, aes(residuals)) + 
      labs(legend.position = "none") + 
      geom_histogram(aes(y=..density..), colour="black", fill="white") +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(regress_vars_sqrt$residuals, na.rm = TRUE), 
                                sd = sd(regress_vars_sqrt$residuals, na.rm = TRUE)), 
                    colour = "black", size = 1) +
      labs(x="MODEL RESIDUALS", y = "Density")


# pretty normal

## qq pplot

qqnorm(regress_vars_sqrt$residuals,
                             xlab = "Theoretical Quantiles", 
                             ylab = "Sample Quantiles RESIDUALS")

qqline(regress_vars_sqrt$residuals)


shapiro.test(regress_vars_sqrt$residuals)
## not significantly different from normal distrution


### constant variance

ggplot(regress_vars_sqrt, aes(predicted, residuals)) +
      geom_point() + 
      geom_smooth()+
      #geom_smooth(method = "lm", colour = "Red", se = F) + 
      labs(x = "PREDICTED VALUES (FITTED)", y = "RESIDUALS") 

# homoscedastic

ncvTest(final_model_sqrt)  # yup


## interpretation here








### t-tests

#  DO TABLE OF MEANS HERE!!!!!

# variance across sex
aggregate(Yearly_Income ~ Sex , data=regress_vars_base, var)


# normality 

ggplot(regress_vars_base, aes(x=Yearly_Income, fill=Sex)) +
             geom_histogram(alpha=1/2) +
      labs(x="Yearly Income (square root)", y = "Density", title = "Yearly Income Distribution by Gender\n",
           fill = "Gender")+
      scale_fill_manual(values = c("Blue", "Red"), labels = c("Male", "Female")) +
      theme_hc()


# not normal - transform
regress_vars_base$sqrt_Yearly_Income <- sqrt(regress_vars_base$Yearly_Income)

ggplot(regress_vars_base, aes(x=sqrt_Yearly_Income, fill=Sex)) +
      geom_histogram(alpha=1/2) +
      labs(x="Yearly Income (square root)", y = "Density", title = "Yearly Income Distribution by Gender\n",
           fill = "Gender")+
      scale_fill_manual(values = c("Blue", "Red"), labels = c("Male", "Female")) +
                 theme_hc()

# normal

var.test(regress_vars_base$sqrt_Yearly_Income[regress_vars_base$Sex == 1], regress_vars_base$sqrt_Yearly_Income[regress_vars_base$Sex == 2])
# variances different - use welches t test

t.test(sqrt_Yearly_Income ~ Sex, data=regress_vars_base, var.equal = FALSE)



# error bars

income_Summary<- regress_vars_base %>% 
      group_by(Sex) %>%
      dplyr::summarize(inc.mean=mean(sqrt_Yearly_Income, na.rm = T),
                inc.sd=sd(sqrt_Yearly_Income, na.rm = T),
                Lower=inc.mean - 2 * inc.sd / sqrt(NROW(sqrt_Yearly_Income)),
                Upper=inc.mean + 2 * inc.sd / sqrt(NROW(sqrt_Yearly_Income))
      )
income_Summary

ggplot(income_Summary, aes(x=inc.mean, y = Sex, colour=Sex)) + 
      geom_point() +
      geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2) +
      labs(x="Yearly Income (mean)", y = "", colour = "Gender")+
      scale_colour_manual(values = c("Blue", "Red"), labels = c("Male", "Female")) +
      theme_hc()


# very spread no overlap

