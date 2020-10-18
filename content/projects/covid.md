---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Mortality and recovery analysis of Covid-19 in the UK
draft: false
image: covid.jpg
keywords: ""
slug: covid
title: Mortality and recovery analysis of Covid-19 in the UK
---

Let us revisit the [CDC Covid-19 Case Surveillance Data](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf). There are well over 3 million entries of individual, de-identified patient data. Since this is a large file, I suggest to use `vroom` to load it and you keep `cache=TRUE` in the chunk options.


```{r, cache=TRUE}
# file contains 11 variables and 3.66m rows and is well over 380Mb. 
# It will take time to download

# URL link to CDC to download data
url <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"

covid_data <- vroom::vroom(url)%>% 
  clean_names()


```

Covid_19 death rate (%) by age group, sex, and whether the patient had co-morbidities or not:

```{r covid_challenge1.1, out.width="100%"}

  #Removing useless variables
removed_val=c("Missing", "Unknown", "Other", NA)


death_rate_condition <- covid_data %>%
select(medcond_yn, death_yn, sex, age_group) %>% 
  #Removing the variables linked to "missing", "unknown", "Other"   and    NA
filter(!(death_yn %in% removed_val) ,
      !(medcond_yn %in% removed_val), 
      !(sex %in% removed_val),
      !(age_group %in% removed_val),)  %>%
mutate(death = ifelse(death_yn == "Yes", 1, 0), co_mor=
ifelse(medcond_yn == "Yes", "With Comorbidities", "Without
Comorbidities")) %>% group_by(age_group, sex, co_mor) %>%
summarize(death_rate = prop(death))

ggplot(death_rate_condition, aes(x=death_rate, y=age_group)) +
geom_col(fill="#000099", alpha=0.8) + facet_grid(rows=vars(co_mor),cols
= vars (sex)) +
  #Scales function seemed to be masked
scale_x_continuous(labels = scales::percent) +  theme_bw()+
geom_text(aes(label=round(death_rate*100, 2)),
position=position_dodge(width=0.8), hjust=-0.06, size=1.8) + 
labs (title = "Covid death % by age group, sex and presence of co-morbidities", caption="Source: CDC", x= "", y= "") + theme(text=element_text(size=5), axis.text.y = element_text(size = 4))
      

```

Covid_19 death rate (%) by age group, sex, and whether the patient was admited 
to Intensive Care Unit (ICU) or not:

```{r covid_challenge1.2, out.width="100%" }

  #Removing useless variables
removed_val=c("Missing", "Unknown", "Other", NA)

death_rate_icu <- covid_data %>%
select(icu_yn, death_yn, sex, age_group) %>% 
  #Removing the variables linked to "missing", "unknown", "Other"   and    NA 
filter(!(death_yn %in% removed_val) ,
      !(icu_yn %in% removed_val), 
      !(sex %in% removed_val),
      !(age_group %in% removed_val),)  %>%
mutate(death = ifelse(death_yn == "Yes", 1, 0), 
icu= ifelse(icu_yn == "Yes", "Admitted to ICU", "No ICU")) %>%
group_by(age_group, sex, icu) %>% 
summarize(death_rate = prop(death))
  
ggplot(death_rate_icu, aes(x=death_rate, y=age_group)) +  geom_col(fill="#FF0000", alpha=0.8) + 
facet_grid(rows=vars(icu), cols = vars (sex)) +
  #Scales function seemed to be masked
scale_x_continuous(labels = scales::percent) +  theme_bw()+
geom_text(aes(label=round(death_rate*100, 2)),position=position_dodge(width=0.8), hjust=-0.06, size=1.8) + 
labs (title = "Covid death % by age group, sex and and whether patient was admitted to ICU", caption="Source: CDC", x= "", y= "") +
theme(text=element_text(size=5), axis.text.y = element_text(size = 4))
   
    
```
