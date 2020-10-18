---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: 2016 US Elections
draft: false
image: elections.jpg
keywords: ""
slug: elections
title: 2016 US Elections 
---

I would like you to reproduce the plot that shows the top ten cities in highest amounts raised in political contributions in California during the 2016 US Presidential election.

```{r challenge2, echo=FALSE, out.width="100%"}

knitr::include_graphics(here::here("images", "challenge2.png"), error = FALSE)
CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))
zipcodes_2016 <- vroom::vroom(here::here("data","zip_code_database.csv"))

zipcodes_2016 <- zipcodes_2016 %>%
    mutate(zip=as.double(zip))
merged_dataframe <- left_join( CA_contributors_2016, zipcodes_2016, by="zip")

merged_dataframe %>%
  group_by(primary_city) %>%
  summarise(total_amount=sum(contb_receipt_amt)) %>% arrange(desc(total_amount)) %>% slice(1:10)

library(patchwork)
library(dplyr)

merged_dataframe %>%
  group_by(cand_nm) %>%
  summarise(total_amount=sum(contb_receipt_amt)) %>% arrange(desc(total_amount)) %>% slice(1:10)

CA_2016_Clinton <- merged_dataframe %>% filter(cand_nm == "Clinton, Hillary Rodham") %>% group_by(primary_city) %>%
      summarise(total_amount=sum(contb_receipt_amt)) %>% arrange(desc(total_amount)) %>%
      mutate(primary_city = fct_reorder(primary_city, total_amount))%>%  slice (1:10)

CA_2016_Clinton

CA_2016_trump <- merged_dataframe %>% filter(cand_nm == "Trump, Donald J.") %>% group_by(primary_city) %>%
      summarise(total_amount=sum(contb_receipt_amt)) %>% arrange(desc(total_amount))%>%
      mutate(primary_city = fct_reorder(primary_city, total_amount))%>%  slice (1:10)

CA_2016_trump

Plot_clinton<- ggplot(CA_2016_Clinton, aes(x=total_amount, y=primary_city)) + 
  geom_col()  + 
  theme_minimal() + 
  labs(x ="" , y = "", title = "Clinton, Hillary Rodham") 

Plot_clinton

Plot_trump<- ggplot(CA_2016_trump, aes(x=total_amount, y=primary_city)) + 
  geom_col()  + 
  theme_minimal() + 
  labs(x ="" , y = "", title = "Trump, Donald J.") 

Plot_trump

library(patchwork)

wrap_plots(Plot_clinton, Plot_trump)

library(tidytext)

top_cand <- merged_dataframe %>%
  group_by(cand_nm) %>%
  summarise(total_amount=sum(contb_receipt_amt)) %>% arrange(desc(total_amount)) %>% slice(1:10)

top_cand

top_cand_10 <-merged_dataframe %>%
    filter(cand_nm %in% top_cand$cand_nm) %>% 
    group_by(primary_city, cand_nm) %>% 
    summarise(total_amount =sum(contb_receipt_amt)) %>% 
    group_by(cand_nm) %>%
    slice(1:10) %>% 
    ungroup %>% 
    mutate(cand_nm = as.factor(cand_nm),
    primary_city = reorder_within(primary_city, total_amount, cand_nm)) 

Plot_top_10 <- ggplot(top_cand_10, aes(primary_city, total_amount)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~cand_nm, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    theme_bw()+
    scale_y_continuous(expand = c(0,0)) + 
    theme (axis.title.x = element_blank(), 
        axis.title.y = element_blank())

Plot_top_10



```