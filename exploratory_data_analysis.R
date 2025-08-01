---
title: "Common Assessment (CA) Exploratory Data Analysis"
description: |
  Exploring Common Assessment (CA) data to discover how CA is performing as a tool to collect client intake information.
author:
  - name: Rui Sun | Analytics Unit | Finance, Analysis and Systems Support Branch (FASSB) 
    url: http://intra.infogo.gov.on.ca/infogo/home.html#orgProfile/113819/en
    affiliation: Employment and Training Division (ETD) | Ministry of Labour, Training and Skills Development (MLTSD)
    affiliation_url: 
date: "`r format(Sys.Date(), '%B %d %Y')`"
knit: (function(inputFile, encoding) {
  rmarkdown::render(inputFile, encoding = encoding, output_dir = "output") })
output: 
  distill::distill_article:
    toc: TRUE
    code_folding: TRUE
editor_options: 
  chunk_output_type: inline
---

```{r setup, warning=FALSE}

# Load libraries

require(knitr)
require(tidyverse)
require(odbc)
require(dbplyr)
require(rstudioapi)
require(here)
require(janitor)
require(lubridate)
require(DT)
library(kableExtra)
library(tidytext)
library(dplyr)
library(stringi)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(mgsub)
library(scales) 
library(stringr)
library(textdata)
library(widyr)
library(ggraph)


# Set parameters
opts_chunk$set(echo = FALSE)
options(scipen = 999)


# Set colours

cl_maroon <- "#8f103c"
cl_drk_grn <- "#035951"
cl_teal <- "#47A6A1"
cl_blue <- "#047cc2"
cl_lt_blue <- "#77c1ed"
cl_gold <- "#8f6310"


palette <- c(cl_blue,cl_drk_grn,cl_lt_blue,cl_maroon,cl_teal,
             "#a147a6","#a6a147","#108f63","#103c8f","#8f6310") 


# Import data
# Let's look at the conditions of the respondent.
raw <- read_rds(here("data/ca_data.rds"))
```
# Purpose  

- The purpose of this exploratory analysis is to gain more insight into how Common Assessment (CA) is performing as a tool to collect key information about Integrated Employment Services (IES) clients.

- To accomplish this, using data from CA records submitted between January 1, 2021 to early July 2022, we explored:

  - Patterns in “prefer not to answer” (PNTA)  

  - Trends between socio-demographic groups and streams 

  - Themes within “other (specify)” comments
 
# PART 1: "Prefer not to answer"(PNTA)

## How often is each client choosing "Prefer not to answer"?
-	Overall, 29% or 14,421 clients indicated PNTA to at least one of the 20 questions in CA with this option.
-	Among all clients who indicated PNTA at least once, most of them indicated PNTA to only one question (67%), followed by a relatively lower proportion indicating PNTA to two questions (23%). 
-	Rates and numbers of clients indicating PNTA more than two times drastically decreases; for example, only 0.3% or 40 clients indicate PNTA six times.
-	In general, it doesn’t seem like PNTA is being overused, nor that the same clients are choosing PNTA across all questions.

```{r}
# Let's look at all the variables with "prefer not to answer" option.
dat <- raw %>% 
  select(gender, lgbtq, indigenous, francophone, race_category, marital_status, filter_sensory, filter_physical, filter_cognitive, filter_mentalhealth, filter_other, crime_job_difficult, work_precarious, crime, country_of_birth_c, arrival_date_c, dependents_c, work_wage_cur_c, work_wage_past_c, work_goal_wage_c )
```

```{r}
# pick up the "prefer not to answer groups and convert them to numbers to do next statistics
pnta<- dat %>% 
    filter_all(any_vars(. == "Prefer not to answer")) %>% 
    mutate_all(function(x)ifelse(x == "Prefer not to answer",1,0)) %>% 
    mutate_all(function(x)replace(x,is.na(x),0))
```

```{r}
# how many people answered "prefer not to answer" multiple times
Count <-rowSums(pnta[,1:20])
tabyl(Count) %>% 
  mutate(percent = paste(round(percent*100,2),"%", sep="")) %>% 
  kbl(.,digits = 2, format.args=list(big.mark = ","),
      col.name=c( "Number of times clients indicate PNTA",
                  "# of times PNTA was indicated",
                    "% of Clients indicate PNTA")) %>%
  kable_classic(full_width = F, html_font = "Cambria")

Count <- as.data.frame(tabyl(Count))
ggplot(Count,aes(x = Count, y = percent)) +
theme_classic() +
geom_col(aes(y = percent*100, fill = 'cl_lt_blue'), show.legend = FALSE) +
labs(title = "Number of times clients indicate PNTA",
x = "# of times PNTA was indicated",
y = "% of Clients indicate PNTA") +
scale_y_continuous(label = scales :: percent_format(scale=1))

```
## How often is PNTA chosen for each question?
-	Rates of preferring not to answer range from 0.1% of clients who were asked about sensory difficulties up to 14.7% of clients who were asked about past wage 
-	Wage questions have the highest proportions of preferring not to answer, followed by race and LGBTQ+ identity
-	Wage questions and some socio-demographic questions, like race and LGBTQ+ identity, may be more sensitive or complicated for clients or caseworkers to answer, which may suggest the need for accessible help text and clearer rationale


```{r}
# To avoid error warning, we need to eliminate variables with date first
group <- raw %>% 
  select(ends_with("date"))

raw <- raw %>% 
  select(-names(group))
```

```{r}
# Check which variables has the "Prefer not to answer option" and the number of that group
filter.result <- data.frame(matrix(nrow = 175, ncol = 2))
colnames(filter.result) <- c("NAMES", "NUMBER")
for (i in 1:ncol(raw))
{
 filterfirst <- length(which(raw[,i] == "Prefer not to answer"))
 filter.result[i,1] <- names(raw[,i])
 filter.result[i,2] <- filterfirst
}

```

```{r}
#Filter only the variables we will use later
result.pnta <- filter.result %>% 
  filter(NUMBER>0) %>% 
  filter(!grepl('see|hear|mobi|flex|dext|learn|devel|mem|mentalhealth_|other_|pain', NAMES))

Ques.name <- c("Gender", "LGBTQ", "Indigenous", "Francophone", "Race category", "Marital Status", "Sensory related", "Physical related", "Cognitive related", "Mental health", "Other health", "Crime record difficulty", "Work precarious", "Crime history", "Country of birth", "Arrival date", "Dependents", "Current wage", "Past wage", "Goal wage" )

result.pnta$NAMES <- Ques.name
```

```{r,include=FALSE}
# Show the percentage results
ques.name <- c("gender", "LGBTQ", "indigenous", "francophone", "race category", "marital status", "sensory related", "physical related", "cognitive related", "mental health", "other health", "crime record difficulty", "work precarious", "crime history", "country of birth", "arrival date", "dependents", "current wage", "past wage", "goal wage" )
for ( i in 1:nrow(result.pnta))
{result <- paste("The proportion of people who replied 'Prefer not to answer' in the ",ques.name[i]," question is ", round((result.pnta$NUMBER[i]/sum(!is.na(dat[[i]])))*100,2), "%",sep="")
 print(result)
}
```

```{r}
#Now we can use the number from last step to calculate the proportion of people who choose 'Prefer not to answer' in ad hoc questions
for ( i in 1:nrow(result.pnta)){
perc = round((result.pnta$NUMBER[i]/sum(!is.na(dat[[i]])))*100,2) 
result.pnta[i,3] <- perc
}
```

```{r}
ggplot(result.pnta,aes(x = NAMES, y = V3)) +
geom_text(aes(x = reorder(NAMES,V3), y = V3,
label = paste0(V3,"%"),
vjust = 0.3, hjust = - 0.1),size=3) +
theme_classic() +
geom_col(aes(y = V3,fill = "cl_lt_blue"),  show.legend = FALSE) +
labs(title = "The proportion of clients who PNTA each question",
x = "Questions",
y = "Proportion") +
scale_y_continuous(label = scales :: percent_format(scale=1),limits = c(0,20))+
coord_flip()
```


#  PART 2: Socio-Demographic Data

## What is the proportion of clients in each socio-demographic group?

```{r}
# Select all the demographical variables and regroup some of them
demo <- raw %>%
  select(gender, lgbtq, indigenous, francophone, race_category, marital_status,education_level, stream) %>% 
  drop_na() %>% 
  mutate(gender = case_when(gender %in% c("Another gender identity (specify), Man" ,
                                          "Gender non-binary, Woman",
                                          "Transgender woman, Woman",
                                          "Gender non-binary, Man")|
                              gender %in% unique(raw$gender)[13:23]
                            ~ "Muliple Choices",
                            gender %in% c("Transgender man","Transgender woman","Transgender")
                            ~ "All Transgender Options",
                            TRUE ~ gender),
    indigenous= case_when(indigenous %in% c("First Nations, Inuk (Inuit)"
                                                 ,"First Nations, Métis",
                                                 "First Nations, No")
                               ~ "Muliple Choices",
                               indigenous == "No, Prefer not to answer"
                               ~ "No",
                               indigenous == "Inuk (Inuit)"
                               ~"Inuit (Inuk)",
                               TRUE ~ indigenous),
          race_category = case_when(race_category %in% unique(raw$race_category)[10:55]|
                                    race_category == "Another race category (specify):" 
                                    ~ "Muliple Choices",
                                    TRUE ~ race_category),
         education_level = case_when(education_level %in% c( "Grade 10","Grade 11","Grade 0-8",
                                                             "Grade 9")
                                     ~  "Under Grade 12",
                                     education_level %in% c( "OAC or Grade 13, Some university",
                                                             "OAC or Grade 13","Grade 12 or equivalent (GED)",
                                                             "Grade 12 or equivalent (GED), Grade 9")
                                     ~ "Grade 12/13/GED/OAC",
                                     TRUE ~ education_level),
    marital_status = case_when(marital_status == "Married, Married"
                               ~"Married",
                               TRUE ~ marital_status)
          
          ) 
```


```{r}
# Take a look at the demographically distribution
demo.name <- c("Gender", "LGBTQ", "Indigenous", "Francophone", "Race category", "Marital status","Education levels","Stream")
# 1-digit decimal 
demo.table1 <- function(Category,name){
options(digits=1)
transform(as.data.frame(table(Category)),Perc=paste(format(round(Freq/nrow(demo)*100,1),nsmall = 1),"%",sep="")) %>%
  arrange(desc(Freq)) %>%
 kbl(.,digits = 1, format.args=list(big.mark = ",")) %>%
  kable_classic(full_width = F, html_font = "Cambria")
}

# 2-digit decimal
demo.table2 <- function(Category,name){
options(digits=2)
transform(as.data.frame(table(Category)),Perc=paste(round(Freq/nrow(demo)*100,2),"%",sep="")) %>%
  arrange(desc(Freq)) %>%
 kbl(.,digits = 2, format.args=list(big.mark = ",")) %>%
  kable_classic(full_width = F, html_font = "Cambria")
}

# 0-digit decimal
demo.table0 <- function(Category,name){
options(digits=0)
transform(as.data.frame(table(Category)),Perc=paste(round(Freq/nrow(demo)*100,0),"%",sep="")) %>%
  arrange(desc(Freq)) %>%
 kbl(.,digits = 0, format.args=list(big.mark = ",")) %>%
  kable_classic(full_width = F, html_font = "Cambria")
}
```


### Gender

```{r}
# Apply the function above
demo.table2(demo$gender,demo.name[1])
```

-  Woman and man comprise the majority and the other gender options are the minority of clients.

### LGBTQ

```{r}
demo.table0(demo$lgbtq,demo.name[2])
```

- Interestingly, 5% of clients indicated Yes to LGBTQ+ and almost the same proportion indicated PNTA. 

### Indigenous

```{r}
demo.table2(demo$indigenous,demo.name[3])
```

### Francophone

```{r}
demo.table0(demo$francophone,demo.name[4])
```

### Race

```{r}
demo.table0(demo$race_category,demo.name[5])
race_num <- demo %>% 
  count(race_category) %>% 
  mutate(perc = round(n / sum(n) * 100,0))


ggplot(race_num,aes(x = race_category, y = perc)) +
geom_text(aes(x = reorder(race_category,-n), y = perc,
label = paste0(perc,"%"),
vjust = -0.5, hjust =  0.5),size=3) +
theme_classic() +
geom_col(fill = "light blue") +
labs(x = "Race",y = "Proportion")+
scale_y_continuous(label = scales :: percent_format(scale=1),limits = c(0,50))+
theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
```


- Interestingly, the fourth most common response is to racial identity is “Prefer not to answer”, suggesting this question is sensitive or complicated for clients or caseworkers to answer. 

- The responses written when “Another race category” was selected are also interesting, which will be explained in Part 3.

### Marital status

```{r}
demo.table1(demo$marital_status,demo.name[6])
mart_num <- demo %>% 
  count(marital_status) %>% 
  mutate(perc = round(n / sum(n) * 100,1))
ggplot(mart_num,aes(x = marital_status, y = perc)) +
geom_text(aes(x = reorder(marital_status,-n), y = perc,
label = paste0(perc,"%"),
vjust = -0.5, hjust =  0.5),size=3) +
theme_classic() +
geom_col(fill = "light blue") +
labs(x = "Marital Status",y = "Proportion")+
scale_y_continuous(label = scales :: percent_format(scale=1),limits = c(0,60))+
theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

-	The majority of clients (56%) are Single, a quarter are Married, and the proportions in all other groups are relatively much smaller.

### Education level

```{r}
demo.table1(demo$education_level,demo.name[7])
educ_num <- demo %>% 
  count(education_level) %>% 
  mutate(perc = round(n / sum(n) * 100,1))
ggplot(educ_num,aes(x = education_level, y = perc)) +
geom_text(aes(x = reorder(education_level,-n), y = perc,
label = paste0(perc,"%"),
vjust = -0.5, hjust =  0.5),size=3) +
theme_classic() +
geom_col(fill = "light blue") +
labs(x = "Education level",y = "Proportion")+
scale_y_continuous(label = scales :: percent_format(scale=1),limits = c(0,35))+
theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

-	The highest level of education most of our clients have – 28.6% – is a high school diploma or equivalent, followed by 19.8% with a college certificate or diploma, 14.8% with less than grade 12, and 13.8% with a bachelor’s degree.

### Stream

```{r}
demo.table1(demo$stream,demo.name[8])
stream_num <- demo %>% 
  count(stream) %>% 
  mutate(perc = round(n / sum(n) * 100,1))
ggplot(stream_num,aes(x = stream, y = perc)) +
geom_text(aes(x = reorder(stream,n), y = perc,
label = paste0(perc,"%"),
vjust = -0.5, hjust =  0.5),size=3) +
theme_classic() +
geom_col(fill = "light blue") +
labs(x = "Stream",y = "Proportion")+
scale_y_continuous(label = scales :: percent_format(scale=1),limits = c(0,50))+
theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

-	Most clients in IES – 47% – are segmented into Stream C, followed by 31% in Stream B and 22% in Stream A. According to the current statistical segmentation model, clients in Stream C have the relatively highest risk of long-term unemployment compared to those in Stream B who have a relatively higher risk than those in Stream A. 

## How do the proportions of socio-demographic groups vary by stream?

```{r}
attach(demo)
demo_stream_inter<- function(Category,name){
 datatable(as.data.frame(prop.table(table(Category,stream),margin=2)) %>% 
   pivot_wider(names_from=stream ,values_from=Freq) %>%
   arrange(desc(.[,4])) %>% 
   adorn_totals("row"),
   rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    paste("Cross counts for ", name, sep="")),
   options=list(info=FALSE, paging = FALSE, searching = FALSE)
  ) 
}
```

```{r}
# cross demographic variables with stream table
stacked.graph <- function(Category,name){
dt = demo%>%
  dplyr::group_by({{Category}}, stream)%>%
  dplyr::tally()%>%
  dplyr::mutate(percent=n/sum(n))
ggplot(data = dt,aes(x=reorder({{Category}},-n,sum), y = n ,fill = stream))+
 geom_bar(stat="identity",position = "fill")+
 geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                     position=position_fill(vjust=0.5), colour="white", size =2)+
 theme_minimal()+
 labs(title =paste0("Stream distribution among different ",name, " groups",sep=""),x =name, y = "Stream")+
 theme(axis.text.x = element_text(angle = 45,hjust =1 ))
}
```

### Gender by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(gender,demo.name[1])%>%
  formatPercentage(2:4, digits = 2)
```

-	As Stream “increases” (i.e., from Stream A to C) the proportion of clients indicating a gender other than just Woman or just Man also increases (with the exception of “another gender identity”). 
-	The overwhelming majority of clients who identify as a non-binary gender, like two-spirit, transgender, or gender non-binary, are in Stream C.

```{r}
stacked.graph(gender,demo.name[1])
```

### LGBTQ by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(lgbtq,demo.name[2])%>%
  formatPercentage(2:4, digits = 0)
```

-	Those who identify as LGBTQ+ or prefer not to answer this question are incrementally more likely to be in Stream B and C; as stream increases, proportion of clients answering yes or PNTA to LGBTQ+ also increases.
-	Accordingly, and in alignment with research on sexual orientation and unemployment, this question may be a valuable addition to a future iteration of the segmentation model.
-	Clients who indicated Yes and PNTA (to LGBTQ+) have similar proportions in each Stream, compared to those who indicated No.

```{r}
stacked.graph(lgbtq,demo.name[2])
```

### Indigenous by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(indigenous,demo.name[3])%>%
  formatPercentage(2:4, digits = 2)
```

- As stream increases, the proportion of clients indicating an Indigenous identity also increases (with the exception of clients who choose multiple indigenous identities).
-	A large majority of clients who identify as Inuit, Metis and First Nations are in Stream C.

```{r}
stacked.graph(indigenous,demo.name[3])
```

### Francophone by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(francophone,demo.name[4])%>%
  formatPercentage(2:4, digits = 1)
```

- Like the above trends, the proportion of clients indicating PNTA to Francophone increases incrementally with stream increase. Unlike the previous trends, an equal proportion of clients identify Yes to Francophone in each stream.
- Clients who PNTA for Francophone make up a higher proportion of Stream C compared to clients who indicate Yes or No.

```{r}
stacked.graph(francophone,demo.name[4])
```
 
### Race by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(race_category,demo.name[5])%>%
  formatPercentage(2:4, digits = 1)
```

- We see differing trends when comparing different race categories with stream. 
   -	Specifically, those who identify as South Asian or East/Southeast Asian have an incrementally lower likelihood of long-term unemployment. 
   -	Comparatively, proportions are similar across streams for those who identify as Black, Middle Eastern, Latino, Another race category, or PNTA. 
   -	As stream increases, the proportion of clients indicating indigenous or indicating multiple race categories also increases.
- A large majority of Indigenous clients are in Stream C (77%). Conversely, the majority of South Asian clients are in Stream A (43%).

```{r}
stacked.graph(race_category,demo.name[5])
```

### Marital status by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(marital_status,demo.name[6])%>%
  formatPercentage(2:4, digits = 1)
```

- As stream increases, the proportion of divorced, separated, and widowed clients also increases. Conversely, the proportion of married clients decreases as stream increases.
- The large majority of clients who are widowed, separated or divorced are in Stream C, while the majority of clients who are married are in Stream B.

```{r}
stacked.graph(marital_status,demo.name[6])
```

### Education by stream

```{r,fig.align = 'center',out.width="200%"}
demo_stream_inter(education_level,demo.name[7])%>%
  formatPercentage(2:4, digits = 1)
```

- Clients with a grade 12 education or less, have an incrementally higher risk of long-term unemployment, whereas, clients who completed college or university have an incrementally lower risk of long-term unemployment.
- Compared to other education levels, the overwhelming majority of clients with less than grade 12 education are in Stream C (74.3%).

```{r}
stacked.graph(education_level,demo.name[7])
```


```{r, include = FALSE}
# Take a look at how the education levels among different race groups


dt = demo%>%
  mutate(education_level = case_when(education_level == "Some university" 
                                     ~ "Bachelor's degree",
                                     education_level == "Some college"
                                     ~ "Certificate/Diploma",
                                     education_level == "Some apprenticeship"
                                     ~ "Certificate of Apprenticeship",
                                     TRUE ~ education_level)) %>% 
  dplyr::group_by(race_category, education_level)%>%
  dplyr::tally()%>%
  dplyr::mutate(percent=n/sum(n))

ggplot(data = dt,aes(x= race_category, y = n ,fill = education_level))+
 geom_bar(stat="identity", position ="fill")+
 geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                     position=position_fill(vjust=0.5), colour="white", size =2)+
 theme_minimal()+
 labs(title ="Education distribution among different race groups",x ="Race", y = "Education")+
 theme(axis.text.x = element_text(angle = 45,hjust =1 ))
```

#  PART 3: Text detection for "other(specify)" comments

```{r}
text <- read_rds(here("data/other_responses.rds"))

preptext <- data.frame(matrix(nrow = 9, ncol = 2))
colnames(preptext) <- c("Variable","UsefulData")
for (i in 1:9){
usefuldata <- sum(!is.na(text[,i]))
preptext[ ,1] <- c("CA id","Residency Status","Gender","Race","Source of Income","Received assistance type","Student type","Work nature","Unemployment reason")
preptext[i,2] <- usefuldata
}
```

## What questions have the "other(specify)" option?

```{r}
preptext %>% 
  arrange(desc(.[,2])) %>% 
  kbl(.,digits = 1, format.args=list(big.mark = ","),col.names = c("Question","Number of other responses")) %>%
  kable_classic(full_width = F, html_font = "Cambria")
```

-	In general, the more data we have to work with when text mining, the more useful the results are. 

## Unemployment reason
- The rate of indicating the “other (specify)” option for this question is relatively high compared to other questions with this option, i.e., 29% of the clients who were asked this question (or 11,666 clients)

```{r}
#text detection part

other_unempl_reason <- text %>% 
  select(work_unempl_reason_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- sapply(other_unempl_reason$work_unempl_reason_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
other_unempl_reason$work_unempl_reason_other <- tolower(other_unempl_reason$work_unempl_reason_other)
# Remove numbers in the text
other_unempl_reason$work_unempl_reason_other <- removeNumbers(other_unempl_reason$work_unempl_reason_other)
# Remove punctuations in the text
other_unempl_reason$work_unempl_reason_other <- removePunctuation(other_unempl_reason$work_unempl_reason_other)
# make wasn't=was not, can't=can not, etc..
other_unempl_reason$work_unempl_reason_other <- gsub("wasn[\u2019']t", "was not", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("won[\u2019']t", "will not", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("can[\u2019']t", "can not", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("didn[\u2019']t", "did not", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("don[\u2019']t", "do not", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("I[\u2019']m", "I am", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("[\u2019']ve", " have", other_unempl_reason$work_unempl_reason_other) 
other_unempl_reason$work_unempl_reason_other <- gsub("[\u2019|']s", "", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("[\u2019']re", " are", other_unempl_reason$work_unempl_reason_other)
other_unempl_reason$work_unempl_reason_other <- gsub("[\u2019']ll", " will", other_unempl_reason$work_unempl_reason_other)
```

```{r,results=FALSE}
# Fix Negations
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                    "no",
                    "without",
                    "never",
                    "bad",
                    "none",
                    "never",
                    "nobody",
                    "nowhere",
                    "neither",
                    "nothing"
)
# Run the following to view Shifted sentiments sorted by polarity point
shifted.words <- other_unempl_reason %>%
  unnest_tokens(bigram, work_unempl_reason_other, token = "ngrams", n = 2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(word1 %in% negation.words & !word2 %in% stop_words$word)%>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))%>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  mutate(score = sentiment * n) %>%
  mutate(word2 = reorder(word2, score))
shifted.words
 
```

```{r}
# Pick the most effective sentiment shifters
negated.phrases <- c("not suitable ",
                     "not renewed",
                     "not satisfied",
                     "no reliable",
                     "not afford",
                     "not comfortable",
                     "not happy"
                     
)
# Find synonyms for the phrases above to replace
synonyms <- c("unsuitable",
              "unrenewed",
              "disappointed",
              "unreliable",
              "unaffordable",
              "uncomfortable",
              "unhappy"
)
# Replace the negations with their synonyms.
other_unempl_reason <- mgsub(other_unempl_reason$work_unempl_reason_other, negated.phrases, synonyms) %>%
  dplyr::as_data_frame() %>%
  rename(work_unempl_reason_other = value)
```

```{r}
#  ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("canada","due","job"))
```

```{r}
# create the words freq table
freq.table <- other_unempl_reason %>% 
  unnest_tokens(word, work_unempl_reason_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Table1: Word frequency table"))
```

```{r}
# Most Common Bigrams
freq.bi.table <- other_unempl_reason %>%
  unnest_tokens(bigram, work_unempl_reason_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Table2: Bigram frequency table"))
```

```{r}
# Most Common Trigrams
freq.tri.table <- other_unempl_reason %>%
  unnest_tokens(trigram, work_unempl_reason_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Table3: Trigram frequency table"))
```

-	Common themes emerging from the text detection algorithms are personal reasons, business closing, temp/contract/seasonal job, and covid concerns.
-	Based on these results, this question about reason for leaving last job may benefit from refining options or adding help text.

-	The following is a network diagram of the commonly occurring bigrams. The darker the arrow, the more frequent the bigrams.

```{r,results=FALSE}
library(igraph)
bigram_graph <- freq.bi.table %>% 
  filter(n > 15) %>% 
  graph_from_data_frame()
bigram_graph
```

```{r,fig.align = 'center',out.width="200%"}
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = cl_lt_blue, size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5) +
  scale_edge_colour_manual(name="frequency",values=n) +
  theme_void()
```

## Nature of work

```{r}
#text detection part

nature_of_job <- text %>% 
  select(work_nature_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- sapply(nature_of_job$work_nature_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
nature_of_job$work_nature_other <- tolower(nature_of_job$work_nature_other)
# Remove numbers in the text
nature_of_job$work_nature_other <- removeNumbers(nature_of_job$work_nature_other)
# Remove punctuations in the text
nature_of_job$work_nature_other <- removePunctuation(nature_of_job$work_nature_other)
# make wasn't=was not, can't=can not, etc..
nature_of_job$work_nature_other <- gsub("wasn[\u2019']t", "was not", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("won[\u2019']t", "will not", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("can[\u2019']t", "can not", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("didn[\u2019']t", "did not", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("don[\u2019']t", "do not", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("I[\u2019']m", "I am", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("[\u2019']ve", " have", nature_of_job$work_nature_other) 
nature_of_job$work_nature_other <- gsub("[\u2019|']s", "", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("[\u2019']re", " are", nature_of_job$work_nature_other)
nature_of_job$work_nature_other <- gsub("[\u2019']ll", " will", nature_of_job$work_nature_other)
```

```{r,results=FALSE}
# Fix Negations
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                    "no",
                    "without",
                    "never",
                    "bad",
                    "none",
                    "never",
                    "nobody",
                    "nowhere",
                    "neither",
                    "nothing"
)
# Run the following to view Shifted sentiments sorted by polarity point
shifted.words <- nature_of_job %>%
  unnest_tokens(bigram, work_nature_other, token = "ngrams", n = 2)%>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(word1 %in% negation.words & !word2 %in% stop_words$word)%>%
  inner_join(get_sentiments("bing"), by = c(word2 = "word"))%>%
  mutate(sentiment = ifelse(sentiment == "positive", 1, -1)) %>%
  mutate(score = sentiment * n) %>%
  mutate(word2 = reorder(word2, score))

shifted.words
 
```

```{r}
# Pick the most effective sentiment shifters
negated.phrases <- c("not suitable ")
                     

# Find synonyms for the phrases above to replace
synonyms <- c("unsuitable")
         

# Replace the negations with their synonyms.
nature_of_job <- mgsub(nature_of_job$work_nature_other, negated.phrases, synonyms) %>%
  dplyr::as_data_frame() %>%
  rename(work_nature_other = value)
```

```{r}
#  ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("pt","time"))
```

```{r}
# create the words freq table
freq.table2 <- nature_of_job %>% 
  unnest_tokens(word, work_nature_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table2,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Nature of Work: Word frequency table"))

```

```{r}
# Most Common Bigrams
freq.bi.table2 <- nature_of_job %>%
  unnest_tokens(bigram, work_nature_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()

datatable(freq.bi.table2,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Nature of Work: Bigram frequency table"))
```

```{r,include=FALSE}
# Most Common Trigrams
freq.tri.table2 <-nature_of_job %>%
  unnest_tokens(trigram, work_nature_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()

datatable(freq.tri.table2,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Nature of Work: Trigram frequency table"))
```

## Source of income

```{r}
#text detection part
source_of_income <- text %>% 
  select(soi_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(source_of_income$soi_other)
source_of_income$soi_other <- sapply(source_of_income$soi_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
source_of_income$soi_other <- tolower(source_of_income$soi_other)
# Remove numbers in the text
source_of_income$soi_other <- removeNumbers(source_of_income$soi_other)
# Remove punctuations in the text
source_of_income$soi_other <- removePunctuation(source_of_income$soi_other)
# make wasn't=was not, can't=can not, etc..
source_of_income$soi_other <- gsub("wasn[\u2019']t", "was not", source_of_income$soi_other)
source_of_income$soi_other <- gsub("won[\u2019']t", "will not", source_of_income$soi_other)
source_of_income$soi_other <- gsub("can[\u2019']t", "can not", source_of_income$soi_other)
source_of_income$soi_other <- gsub("didn[\u2019']t", "did not", source_of_income$soi_other)
source_of_income$soi_other <- gsub("don[\u2019']t", "do not", source_of_income$soi_other)
source_of_income$soi_other <- gsub("I[\u2019']m", "I am", source_of_income$soi_other)
source_of_income$soi_other <- gsub("[\u2019']ve", " have", source_of_income$soi_other) 
source_of_income$soi_other <- gsub("[\u2019|']s", "", source_of_income$soi_other)
source_of_income$soi_other <- gsub("[\u2019']re", " are", source_of_income$soi_other)
source_of_income$soi_other <- gsub("[\u2019']ll", " will", source_of_income$soi_other)
```

```{r}
#  ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("pt","job"))
```

```{r}
# create the words freq table
freq.table3 <- source_of_income %>% 
  unnest_tokens(word, soi_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table3,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Source of Income: Word frequency table"))
```

```{r}
# Most Common Bigrams
freq.bi.table3 <- source_of_income %>%
  unnest_tokens(bigram, soi_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table3,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Source of Income: Bigram frequency table"))
```

```{r}
# Most Common Trigrams
freq.tri.table3 <-source_of_income %>%
  unnest_tokens(trigram, soi_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table3,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Source of Income: Trigram frequency table"))
```

-	The following is a network diagram of the commonly occurring bigrams. The darker the arrow, the more frequent the bigrams.

```{r,results=FALSE}
library(igraph)
bigram_graph <- freq.bi.table3 %>% 
  filter(n > 15) %>% 
  graph_from_data_frame()
bigram_graph
```

```{r}
set.seed(10)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = cl_lt_blue, size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5) +
  scale_edge_colour_manual(name="frequency",values=n) +
  theme_void()
```

## Assistance Type

```{r}
#text detection part
received_assistance_type <- text %>% 
  select(assist_type_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- sapply(received_assistance_type$assist_type_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
received_assistance_type$assist_type_other <- tolower(received_assistance_type$assist_type_other)
# Remove numbers in the text
received_assistance_type$assist_type_other <- removeNumbers(received_assistance_type$assist_type_other)
# Remove punctuations in the text
received_assistance_type$assist_type_other <- removePunctuation(received_assistance_type$assist_type_other)
# make wasn't=was not, can't=can not, etc..
received_assistance_type$assist_type_other <- gsub("wasn[\u2019']t", "was not", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("won[\u2019']t", "will not", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("can[\u2019']t", "can not", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("didn[\u2019']t", "did not", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("don[\u2019']t", "do not", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("I[\u2019']m", "I am", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("[\u2019']ve", " have", received_assistance_type$assist_type_other) 
received_assistance_type$assist_type_other <- gsub("[\u2019|']s", "", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("[\u2019']re", " are", received_assistance_type$assist_type_other)
received_assistance_type$assist_type_other <- gsub("[\u2019']ll", " will", received_assistance_type$assist_type_other)
```


```{r}
#  ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("pt","time"))
```

```{r}
# create the words freq table
freq.table4 <- received_assistance_type %>% 
  unnest_tokens(word, assist_type_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table4,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Assistance Type: Word frequency table"))
```

```{r}
# Most Common Bigrams
freq.bi.table4 <- received_assistance_type %>%
  unnest_tokens(bigram,assist_type_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table4,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Assistance Type: Bigram frequency table"))
```

```{r}
# Most Common Trigrams
freq.tri.table4 <- received_assistance_type %>%
  unnest_tokens(trigram, assist_type_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table4,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Assistance Type: Trigram frequency table"))
```

-	The following is a network diagram of the commonly occurring bigrams. The darker the arrow, the more frequent the bigrams.

```{r,results=FALSE}
library(igraph)
bigram_graph <- freq.bi.table4 %>% 
  filter(n > 5) %>% 
  graph_from_data_frame()
bigram_graph
```

```{r}
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = cl_lt_blue, size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5) +
  scale_edge_colour_manual(name="frequency",values=n) +
  theme_void()
```

## Race

```{r}
#text detection part
racial_type <- text %>% 
  select(racialized_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(racial_type$racialized_other)
racial_type$racialized_other <- sapply(racial_type$racialized_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
racial_type$racialized_other <- tolower(racial_type$racialized_other)
# Remove numbers in the text
racial_type$racialized_other <- removeNumbers(racial_type$racialized_other)
# Remove punctuations in the text
racial_type$racialized_other <- removePunctuation(racial_type$racialized_other)
# make wasn't=was not, can't=can not, etc..
racial_type$racialized_other <- gsub("wasn[\u2019']t", "was not", racial_type$racialized_other)
racial_type$racialized_other <- gsub("won[\u2019']t", "will not", racial_type$racialized_other)
racial_type$racialized_other <- gsub("can[\u2019']t", "can not", racial_type$racialized_other)
racial_type$racialized_other <- gsub("didn[\u2019']t", "did not", racial_type$racialized_other)
racial_type$racialized_other <- gsub("don[\u2019']t", "do not", racial_type$racialized_other)
racial_type$racialized_other <- gsub("I[\u2019']m", "I am", racial_type$racialized_other)
racial_type$racialized_other <- gsub("[\u2019']ve", " have", racial_type$racialized_other) 
racial_type$racialized_other <- gsub("[\u2019|']s", "", racial_type$racialized_other)
racial_type$racialized_other <- gsub("[\u2019']re", " are", racial_type$racialized_other)
racial_type$racialized_other <- gsub("[\u2019']ll", " will", racial_type$racialized_other)
```


```{r}
#  ignore words that are frequent but doesn't help, add them to this list.
ignore.words <- data_frame(word = c("pt","time"))
```

```{r}
# create the words freq table
freq.table5 <- racial_type %>% 
  unnest_tokens(word, racialized_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table5,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Racialized group: Word frequency table"))
```

```{r}
# Most Common Bigrams
freq.bi.table5 <- racial_type %>%
  unnest_tokens(bigram, racialized_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table5,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Racialized group: Bigram frequency table"))
```

```{r,include=FALSE}
# Most Common Trigrams
freq.tri.table5 <- racial_type %>%
  unnest_tokens(trigram, racialized_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table5,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Racialized group: Trigram frequency table"))
```

## Residency status

```{r}
#text detection part
residency_status <- text %>% 
  select(residency_status_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(residency_status$residency_status_other)
residency_status$residency_status_other <- sapply(residency_status$residency_status_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
residency_status$residency_status_other <- tolower(residency_status$residency_status_other)
# Remove numbers in the text
residency_status$residency_status_other <- removeNumbers(residency_status$residency_status_other)
# Remove punctuations in the text
residency_status$residency_status_other <- removePunctuation(residency_status$residency_status_other)
# make wasn't=was not, can't=can not, etc..
residency_status$residency_status_other <- gsub("wasn[\u2019']t", "was not", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("won[\u2019']t", "will not", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("can[\u2019']t", "can not", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("didn[\u2019']t", "did not", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("don[\u2019']t", "do not", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("I[\u2019']m", "I am", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("[\u2019']ve", " have", residency_status$residency_status_other) 
residency_status$residency_status_other <- gsub("[\u2019|']s", "", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("[\u2019']re", " are", residency_status$residency_status_other)
residency_status$residency_status_other <- gsub("[\u2019']ll", " will", residency_status$residency_status_other)
```


```{r}
# create the words freq table
freq.table6 <- residency_status %>% 
  unnest_tokens(word, residency_status_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table6,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Residency Status: Word frequency table"))
```

```{r}
# Most Common Bigrams
freq.bi.table6 <- residency_status %>%
  unnest_tokens(bigram, residency_status_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table6,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Residency Status: Bigram frequency table"))
```

```{r,include=FALSE}
# Most Common Trigrams
freq.tri.table6 <- residency_status %>%
  unnest_tokens(trigram, residency_status_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table6,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Residency Status: Trigram frequency table"))
```

## Gender

```{r}
#text detection part
gender_type <- text %>% 
  select(gender_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(gender_type$gender_other)
gender_type$gender_other <- sapply(gender_type$gender_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
gender_type$gender_other <- tolower(gender_type$gender_other)
# Remove numbers in the text
gender_type$gender_other <- removeNumbers(gender_type$gender_other)
# Remove punctuations in the text
gender_type$gender_other <- removePunctuation(gender_type$gender_other)
# make wasn't=was not, can't=can not, etc..
gender_type$gender_other <- gsub("wasn[\u2019']t", "was not", gender_type$gender_other)
gender_type$gender_other <- gsub("won[\u2019']t", "will not", gender_type$gender_other)
gender_type$gender_other <- gsub("can[\u2019']t", "can not", gender_type$gender_other)
gender_type$gender_other <- gsub("didn[\u2019']t", "did not", gender_type$gender_other)
gender_type$gender_other <- gsub("don[\u2019']t", "do not", gender_type$gender_other)
gender_type$gender_other <- gsub("I[\u2019']m", "I am", gender_type$gender_other)
gender_type$gender_other <- gsub("[\u2019']ve", " have", gender_type$gender_other) 
gender_type$gender_other <- gsub("[\u2019|']s", "", gender_type$gender_other)
gender_type$gender_other <- gsub("[\u2019']re", " are", gender_type$gender_other)
gender_type$gender_other <- gsub("[\u2019']ll", " will", gender_type$gender_other)
```

```{r}
# create the words freq table
freq.table7 <- gender_type %>% 
  unnest_tokens(word, gender_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table7,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Gender group: Word frequency table"))
```

```{r,include=FALSE}
# Most Common Bigrams
freq.bi.table7 <- gender_type %>%
  unnest_tokens(bigram, gender_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table7,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Gender group: Bigram frequency table"))
```

```{r,include=FALSE}
# Most Common Trigrams
freq.tri.table7 <- gender_type %>%
  unnest_tokens(trigram, gender_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table7,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Gender group: Trigram frequency table"))
```

## Student type

```{r}
#text detection part
student_type <- text %>% 
  select(student_type_other) %>% 
  drop_na()
```

```{r,results=FALSE}
#clean the data
# Load "Stop Words" from the tidytext package
data("stop_words")
# Encoding
# Check Encoding and Make it consistent
stri_enc_mark(student_type$student_type_other)
student_type$student_type_other <- sapply(student_type$student_type_other,
                                                  function(row) iconv(row,
                                                                      "latin1",
                                                                      "ASCII",
                                                                      sub = " "))
# Lowercase all text
student_type$student_type_other <- tolower(student_type$student_type_other)
# Remove numbers in the text
student_type$student_type_other <- removeNumbers(student_type$student_type_other)
# Remove punctuations in the text
student_type$student_type_other <- removePunctuation(student_type$student_type_other)
# make wasn't=was not, can't=can not, etc..
student_type$student_type_other <- gsub("wasn[\u2019']t", "was not", student_type$student_type_other)
student_type$student_type_other <- gsub("won[\u2019']t", "will not", student_type$student_type_other)
student_type$student_type_other <- gsub("can[\u2019']t", "can not", student_type$student_type_other)
student_type$student_type_other <- gsub("didn[\u2019']t", "did not", student_type$student_type_other)
student_type$student_type_other <- gsub("don[\u2019']t", "do not", student_type$student_type_other)
student_type$student_type_other <- gsub("I[\u2019']m", "I am", student_type$student_type_other)
student_type$student_type_other <- gsub("[\u2019']ve", " have", student_type$student_type_other) 
student_type$student_type_other <- gsub("[\u2019|']s", "", student_type$student_type_other)
student_type$student_type_other <- gsub("[\u2019']re", " are", student_type$student_type_other)
student_type$student_type_other <- gsub("[\u2019']ll", " will", student_type$student_type_other)
```


```{r}
# create the words freq table
freq.table8 <- student_type %>% 
  unnest_tokens(word, student_type_other) %>%
  anti_join(stop_words) %>%
  anti_join(ignore.words) %>%
  count(word, sort = TRUE)
datatable(freq.table8,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Student Type: Word frequency table"))
```

```{r}
# Most Common Bigrams
freq.bi.table8 <- student_type %>%
  unnest_tokens(bigram, student_type_other, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  drop_na()
datatable(freq.bi.table8,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Student Type: Bigram frequency table"))
```

```{r,include=FALSE}
# Most Common Trigrams
freq.tri.table8 <- student_type %>%
  unnest_tokens(trigram, student_type_other, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  drop_na()
datatable(freq.tri.table8,
          rownames = FALSE,
   caption = htmltools::tags$caption(style = 'caption-side: top; 
                                    text-align: left; 
                                    color:black; 
                                    font-size:120%;
                                    font-weight: bold',
                                    "Student Type: Trigram frequency table"))
```
