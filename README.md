---
title: "PM 566 Midterm"
author: "Arianna Hernandez"
format: html
editor: visual
embed-resources: true
---

## Midterm: How does Fiber Consumption Vary Across demographics throughout the United States?

## Introduction 

Dietary fiber is a key component for dietary health, and its adequate intake has been associated to reduced hypertension, obesity, type 2 diabetes, certain gastrointestinal disorders and cancers, as well as stroke and heart disease(1)(the leading cause of death in the United States).Total dietary fiber intake should be between 25 to 30 grams a day from food, not supplements(2). 

Despite fiber's established benefits, national completion surveys show that about 95% of the U.S. population does not meet fiber intake recommendations which has become a public health concern.**In this project, we are interested in exploring the fiber intake across different demographics–such as age, gender, and race–through the National Health and Nutrition Examination Survey (NHANES). Our main focus is going to be on aduls and teens 13 and older**

The NHANES is a yearly survey ran by the National Center for Health Statistics(NCHS) and its purpose is to monitor the health and nutritional status of adults and children throughout the United States. Due to the COVID-19 pandemic the NHANES program suspended all field operations in March of 2020, leaving the data collection for that year cycle incomplete.Therefore, they compiled a nationally representative pre-pandemic data of 2017-March 2020 from the data collected from the NHANES 2017-2018 and the data they had for 2019 to March 2020. The survey data is divided onto Demographics, Dietary Data, Examination Data, Laboratory Data, Questionnaire Data and Limited Access Data. Here, we will focus on demographics(P_DEMO.XPT) and dietary(P_DR1TOT.XPT) data.

## Methods 

The survey uses a 'stratified multistage probability design' to select participants from the non-institutionalized population. A computer program selects areas, neighborhoods, households, and then individuals. The survey is conducted in two parts. The first dietary recall interview is collected in-person in the Mobile Examination Center (MEC) and the second interview is collected by telephone 3 to 10 days later where a professional asks questions about demographics and basic health.

Demographic data and nutritional data were retrieved directly from the NCHS website and merged by sequence number to match each candidate. The data was then condensed into the important variables for the question of interest:

-   SEQN = Sequence Number
-   DR1TFIBE = Fiber in grams
-   DRQSDT6 = People following a High Fiber Diet
-   DR1DAY = The day the data was taken
-   RIDAGEYR = Age at the time of survey in Years
-   RIDAGEMN = Age in Months for those under 2 years
-   RIDRETH3 = Ethnic Category
-   RIAGENDR = Gender

Skimmed the top/bottom of the data, summarized it to check for missing values, and removed any missing variables for the fiber category to ensure we can get a proper analysis of our variables. Created the 'Age_range' variable that made it easier to analyze fiber intake within different age ranges and how they compare with USDA recommendations. We expected to see low values of fiber consumption across different demographics--

We created a bar graph of fiber intakes across age groups to learn the intake distribution across groups. Cleaned the data by creating labels to important variables like gender, ethnicity, and amount of food eaten in comparison to the usual. This allowed for better and neater understanding of the data. We then created a table with summary statistics for each age group and further analyzed any values that stood out, comparing them to people who reported consuming a high-fiber diet.

Then, we created a boxplot visual and summary statistics to see if there are any big overall differences between the two genders, categorizing by age group. Then, created a boxplot and line graph to see differences in genders across ethnic groups. Lastly, we looked at summary statistics for each ethnic group to check for differences within age groups and gender. 

```{r, results=FALSE, message=FALSE,warning=FALSE}
library(dplyr)
library(ggplot2)
library(haven)
library(tidyverse)
library(viridis)
##reading in the data
data <- read_xpt("P_DR1TOT.XPT")
nrow(data)
demo<- read_xpt("P_DEMO.XPT")
nrow(demo)
mdata<-left_join(data, demo, by= 'SEQN')
nrow(mdata)

mdata <- mdata[, c('SEQN', 'DR1TFIBE', 'DRQSDT6', 'DR1_300', 'RIDAGEYR', 'RIDAGEMN', 'RIDRETH3', 'RIAGENDR')]

summary(mdata)#Check the size of the data, Examine the variables and their types
mdata<-mdata[!is.na(mdata$DR1TFIBE), ] #exclude missing fiber data
mdata$RIAGENDR <- as.factor(mdata$RIAGENDR)
nrow(mdata)
head(mdata)
tail(mdata)

mdata<-mdata %>%
  mutate(Age_range= if_else(RIDAGEYR>=13 & RIDAGEYR<50,factor(1),if_else(
    RIDAGEYR>=50 , factor(2), factor(3)))) ##1 for adult, 2 for senior, 3 children
```

## Preliminary Results 

Figure 1.
```{r, message=FALSE}
##we plot to see the pattern
mdata %>%
  filter (RIDAGEYR >0) %>%
ggplot() + 
  geom_histogram(mapping = aes(x=DR1TFIBE, fill= Age_range)) +
  facet_wrap(~Age_range,nrow = 3) +
  scale_fill_manual(values = c("1" = "gold", "2" = "orange", "3" = "brown"),
                     labels = c("1" = "Ages 13-49", "2" = "Ages 50 and over", "3" = "Children under 13")) +
  labs(x= 'Fiber intake in grams', 
       title = 'Fiber Intake in the United States by Age Category',
       fill = "Age Range")


```
Across the different age groups we can see that fiber intake follows a similar pattern, with the majority of adults of all ages consume less than 15 grams of Fiber. We can also see some very large values that could be outlier.

*Table 1.*
```{r}
##for adults 
Adults<- mdata%>%
  filter(Age_range == 1) %>%
  summarize(
    Ages = "13-49 years old",
    Count = n(),       
    Mean = mean(DR1TFIBE, na.rm = TRUE), 
    Median = median(DR1TFIBE, na.rm = TRUE),  
    Sd = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE),  
    Max = max(DR1TFIBE, na.rm = TRUE)
  )
##for older adults 
OlderA <- mdata%>%
  filter(Age_range == 2) %>%
  summarize(
    Ages = "50+ years old",
    Count = n(),
    Mean = mean(DR1TFIBE, na.rm = TRUE), 
    Median = median(DR1TFIBE, na.rm = TRUE),  
    Sd = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE),  
    Max = max(DR1TFIBE, na.rm = TRUE)
    )
##for children
Child<- mdata%>%
  filter(Age_range == 3) %>%
  summarize(
    Ages = "0-12 years old",
    Count = n(),       
    Mean = mean(DR1TFIBE, na.rm = TRUE), 
    Median = median(DR1TFIBE, na.rm = TRUE),  
    Sd = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE),  
    Max = max(DR1TFIBE, na.rm = TRUE)
   )

combined<-rbind(Adults, OlderA, Child)
knitr::kable(combined, caption = "Fiber Intake Across Age Ranges in the U.S.", "html")
```

Like we saw in the previous graph, the data shows a very wide range of fiber intakes across the population. The maximum value for the Adult Population ages 13-49 has the highest intake value at 107.8, closely followed by the older population 50+ at 103.4. While these values are definitely out of the normal range, they cannot be counted as outliers because they are still within the range of possibility.

*Table 2*
```{r}
mdata <- mdata%>%
   mutate(RIAGENDR = factor(RIAGENDR, 
                            levels = c(1, 2), 
                            labels = c("Male", "Female")))
mdata <- mdata%>%
    mutate(RIDRETH3 = factor(RIDRETH3,
                            levels = c(1,2,3,4,6,7),
                            labels = c("Mexican American","Other Hispanic","Non-Hispanic White",
                                       "Non-Hispanic Black","Non-Hispanic Asian", "Other Race - Including Multi-Racial")))
mdata <- mdata%>%
    mutate(DR1_300 = factor(DR1_300, 
                            levels = c(1,2,3,7,9),
                            labels= c("Much more than usual", "Usual","Much less than usual", "Refused", "Don't Know")))
                             
HFDiet<-mdata %>%
  filter(!is.na(DRQSDT6)) %>%
  select( c('SEQN', 'DR1TFIBE', 'DRQSDT6', 'DR1_300', 'RIDAGEYR', 'RIDRETH3', 'RIAGENDR'))
knitr::kable(HFDiet, 
             caption = "People Who Reported Consuming a High Fiber Diet", 
             col.names = c("ID","Fiber(grams)", "HF","Comparison","Age(years)","Ethnicity", "Gender"),
             "html")
```

To better understand our high values, we looked at the people who indicated if they were on a "high fiber" diet. We can see that half of these people are still consuming under the recommended amounts of fiber--and one of them is under 13 years old. Only one person is consuming a 'high' amount of fiber at 44.5 grams, but we can see that no one from this list is part of these high values we saw in the previous graph. There is no ethnic or gender group that stands out for the consumption of a high-fiber diet either, but the majority of the people in this group are over 50 years old. 

*Figure 2*
```{r}

ggplot(data = mdata) + 
  geom_boxplot(mapping = aes(x = RIAGENDR, y= DR1TFIBE, fill = Age_range)) + 
  scale_fill_manual(values = c("1" = "lightgreen", "2" = "lightblue", "3" = "lightpink"),
                     labels = c("1" = "Ages 13-49", "2" = "Ages 50 and over", "3" = "Children under 13")) +
  labs(
    x = "Gender",
    y = "Fiber Intake (grams)",
    fill = "Age Category",
    title = "Fiber Intake by Gender and Age Category"
  )

```
*Table 3*
```{r}
Males<-mdata%>%
  filter(RIAGENDR == "Male") %>%
  pull(DR1TFIBE) %>%
  summary()
Females<-mdata%>%
  filter(RIAGENDR == "Female") %>%
  pull(DR1TFIBE) %>%
  summary() 

comb<-rbind(Males, Females)
knitr::kable(comb, caption = "Summary of Fiber Intake Across Genders in the U.S.", "html")
```
Though there no huge differences between the genders we can see that males, on average, consume higher amounts of fiber. The median for male adults of all ages was 13.8, while women consumed a median amount of 12 grams of fiber. We see similar outlier points on both genders, with a bit of a higher amount for female children.

**Figure 3**
```{r}
ggplot(data = mdata) + 
  geom_boxplot(mapping = aes(x=RIAGENDR, y= DR1TFIBE, fill = RIAGENDR))+
  facet_wrap(~RIDRETH3) +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Fiber Intake Across different Ethnic Groups and Genders in the United States",
       x = "Gender",
       y = "Fiber intake (grams)",
       fill = "Gender")
```

*Figure 4*
```{r}
 mdata%>%
  ggplot() + 
  geom_line(mapping = aes(x = RIAGENDR, y = DR1TFIBE, color = as.factor(RIDRETH3), group = RIDRETH3)) + 
  facet_wrap(~ RIDRETH3, nrow=3) +
  labs(
    x = "Gender",
    y = "Fiber intake (grams)",
    title = "Fiber Intake Across different Ethnic Groups and Genders in the United States",
    color = "Ethnic Groups")
```
We can see that there are some noticeable differences across ethnic groups, with the highest fiber consumption withinin Mexican Americans and Non-Hispanic Asian groups. As for gender differences within ethnic groups, we can see that almost every grup has a big difference between the two. The most noticeable groups with gender differences are Hispanic(not Mexican American) and Non-Hispanic White.

*Table 4*
```{r,message=FALSE, warning=FALSE}
MA<- mdata%>%
  filter(RIDRETH3 == "Mexican American") %>%
  group_by(Age_range,RIAGENDR) %>%
  summarize(
    Count = n(),             
    Mean = mean(DR1TFIBE, na.rm = TRUE),  
    Median = median(DR1TFIBE, na.rm = TRUE),
    SD = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE), 
    Max = max(DR1TFIBE, na.rm = TRUE)   
  )
knitr::kable(MA, caption = "Summary of Fiber Intake Across Mexican Americans by Gender", "html")
```

Mexican Americans, as we know from the previous boxplot, have a higher median consumption of fiber than most of the other groups. However, we see a 2grams difference in gender for adults below 50 and a much bigger gap for adults over 50 years old of almost 4 grams. Males over 50 are closest to the recommendations of daily fiber intake at 20.35 median grams .
*Table 5*
```{r,message=FALSE, warning=FALSE}

nha<-mdata%>%
  filter(RIDRETH3 == "Non-Hispanic Asian") %>%
  group_by(Age_range,RIAGENDR) %>%
  summarize(
    Count = n(),             
    Mean = mean(DR1TFIBE, na.rm = TRUE),  
    Median = median(DR1TFIBE, na.rm = TRUE),
    SD = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE), 
    Max = max(DR1TFIBE, na.rm = TRUE)   
  )
knitr::kable(nha, caption = "Summary of Fiber Intake Across Non-Hispanic Asians by Gender", "html")
```
Non-Hispanic Asians also seem to have higher median consumption patterns, with males over 50 having the closest consumption per grams to the recommended 25-30 gm. However,  males over 50 consume over 7 gm more fiber than women over 50. Adults below 50 seem to consume similar quantities to Mexican Americans of the same age category.
*Table 6*
```{r,message=FALSE, warning=FALSE}
OH<-mdata%>%
  filter(RIDRETH3 == "Other Hispanic") %>%
  group_by(Age_range,RIAGENDR) %>%
  summarize(
    Count = n(),             
    Mean = mean(DR1TFIBE, na.rm = TRUE),  
    Median = median(DR1TFIBE, na.rm = TRUE),
    SD = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE), 
    Max = max(DR1TFIBE, na.rm = TRUE)   
  )
knitr::kable(OH, caption = "Summary of Fiber Intake Across Hispanics(Except for MA) by Gender", "html")
```

Compared to Mexican Americans and Asians, other Hispanic groups have much lower consumption rates of fiber. Here we see smaller gaps in between the genders, but still prominent in groups of 50+. Again, while still well-under the recommended amount, males over 50 have the highest rate of consumption.

*Table 7, 8, 9*
```{r,message=FALSE, warning=FALSE}


nhw<-mdata%>%
  filter(RIDRETH3 == "Non-Hispanic White") %>%
  group_by(Age_range,RIAGENDR) %>%
  summarize(
    Count = n(),             
    Mean = mean(DR1TFIBE, na.rm = TRUE),  
    Median = median(DR1TFIBE, na.rm = TRUE),
    SD = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE), 
    Max = max(DR1TFIBE, na.rm = TRUE)   
  )
knitr::kable(nhw, caption = "Summary of Fiber Intake Across Non-Hispanic Whites by Gender", "html")

nhb<-mdata%>%
  filter(RIDRETH3 == "Non-Hispanic Black") %>%
  group_by(Age_range,RIAGENDR) %>%
  summarize(
    Count = n(),             
    Mean = mean(DR1TFIBE, na.rm = TRUE),  
    Median = median(DR1TFIBE, na.rm = TRUE),
    SD = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE), 
    Max = max(DR1TFIBE, na.rm = TRUE)   
  )
knitr::kable(nhb, caption = "Summary of Fiber Intake Across Non-Hispanic Blacks by Gender", "html")

other<-mdata%>%
  filter(RIDRETH3 == "Other Race - Including Multi-Racial") %>%
  group_by(Age_range,RIAGENDR) %>%
  summarize(
    Count = n(),             
    Mean = mean(DR1TFIBE, na.rm = TRUE),  
    Median = median(DR1TFIBE, na.rm = TRUE),
    SD = sd(DR1TFIBE, na.rm = TRUE),  
    Min = min(DR1TFIBE, na.rm = TRUE), 
    Max = max(DR1TFIBE, na.rm = TRUE)   
  )
knitr::kable(other, caption = "Summary of Fiber Intake Across Other Races by Gender", "html")
```

Non-Hispanic Whites, Blacks, and other race groups seem to have the lowest fiber consumption patterns. We can still see men over 50 tend to consume the most fiber out of the other categories, but they all have medians well below the recommended amounts for adults their ages.


## Conclusion about what you found in terms of the formulated question

As previously speculated, the overall consumption of fiber is well below what it should be across the United States. The NHANES survey gave us a deeper insight to those statistics, showing that typically, people between 13-49 have the highest value points when it comes to fiber consumption, while people 50+ tend to have higher median values of consumption between age groups--particularly males. 

We had some outlier values that made the relationship of fiber consumption across age groups less clear, so we looked at people who noted consuming a 'high fiber diet' in the survey questions, yet those people were not nearly close to the highest fiber values we had from the survey. The maximum value for the survey was 107.8gm, while the highest value for people who ate a 'high fiber diet' was 44.5 gm. It should be noted that more than half of the people who noted eating high fiber were still below the recommended grams for Americans and they pointed out their consumption was not out of the ordinary either.

The relationship of gender and fiber intake across the age groups showed that there is a gap between male and female fiber consumption. It can be attributed to women generally eating less than men, but more analysis needs to be done to come to a conclusion.

We further analyzed gender across different ethnic groups in the United States. This survey shows  that Mexican Americans and Non-Hispanic Asians tend to consume more fiber than Non-Hispanic Whites, Blacks, and non-defined ethnic groups. The difference is not significant, however, more analysis could offer better insight.

In the future, I would also examine kilocalories, macronutrients, water consumption, and other demographic data (such as pregnancy status or whether participants were born in the United States or another country), with a greater focus on gender differences to better understand which population groups are most vulnerable to fiber underconsumption and to address this issue more effectively. Additionally, I would compare survey data across different years to assess any changes since the pandemic.

Sources
- 1 (https://www.cdc.gov/nchs/fastats/leading-causes-of-death.htm) 
- 2 (https://www.ucsfhealth.org/education/increasing-fiber-intake#:~:text=Total%20dietary%20fiber%20intake%20should,about%20half%20the%20recommended%20amount.)
- 3 (https://wwwn.cdc.gov/nchs/nhanes/Default.aspx)
