
##INTRODUCTION & APPROACH
##This project addresses the question: "What are the most valued data science skills?
##To answer this question, the team took the following approach:
##1. COLLECTING: mined the job board glassdoor.com to identify which skills were requested the most frequently in job descriptions. (Alvaro Bueno & Silverio Vasquez)
##2. TIDYING: creating databases from the raw data. (Sarah Wigodsky)
##3. ANALYZING: developing relevant dataframes from the databases. (Nathan Cooper)
##4. VISUALIZING: creating graphs and descriptions of the conclusions from the analysis. (Jill Anderson)


##SET-UP
####Importing Libraries
suppressWarnings(suppressMessages(library(stringr)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(RCurl)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(DT)))
suppressWarnings(suppressMessages(library(ggplot2)))


##1) COLLECTING
####Importing National average data science salary by company.
##salarydata, eval=TRUE
salary <- read.csv("https://raw.githubusercontent.com/sjv1030/group_project/master/company_salary.csv", stringsAsFactors = FALSE)



####Tidying the Salary Data - Removing dollar sign, commas and making the salary be stored as a number.
##tidysalary, eval=TRUE
salarytidy <- salary[,-1]
salarytidy <- salarytidy %>%
separate(salary, c("salary", "pay_period"), sep="per ")
salarytidy$salary <- gsub(",", "", salarytidy$salary)
salarytidy$salary <- unlist(str_extract_all(salarytidy$salary, "[[:digit:]]{3,}|[[:digit:]]{1,}.[[:digit:]]{1,}"))
salarytidy$salary <- as.numeric(salarytidy$salary)
head(salarytidy)



####Converting Daily, Monthly, and Hourly Salaries into a yearly salary assuming a 40 hour work week for 12 months.
##yearly-salary, eval=TRUE

#pay_period still says month, day or week even though it is now yearly so that it is clear which salaries were changed so that they are all yearly
for (i in 1:length(salarytidy$salary)){
if (salarytidy$pay_period[i] == "month") {salarytidy$salary[i] <- salarytidy$salary[i]*12}
if (salarytidy$pay_period[i] == "week") {salarytidy$salary[i] <- salarytidy$salary[i]*52}
if (salarytidy$pay_period[i] == "hour") {salarytidy$salary[i] <- salarytidy$salary[i]*8*5*52}
if (salarytidy$pay_period[i] == "day") {salarytidy$salary[i] <- salarytidy$salary[i]*5*52}
}  



####Importing the data from Glassdoor for Boston.
##importingdataMA, eval=TRUE
joblistma <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_final_with_dupes_reduced.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")



####Importing data from Glassdoor for New York.
##importingdataNY, eval=TRUE}
joblistny <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_ny.csv", stringsAsFactors=FALSE, fileEncoding = "latin1")



####Importing data from Glassdoor for San Francisco.
##importingdataSF, eval=TRUE}
joblistsf <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_SF.csv", stringsAsFactors=FALSE, fileEncoding = "latin1")
joblistsf <- joblistsf[,-1]



####Importing data from Glassdoor for Chicago.
##importingdataCHI, eval=TRUE}
joblistchi <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_CHI.csv", stringsAsFactors=FALSE, fileEncoding = "latin1")
joblistchi <- joblistchi[,-1]


##2) TIDYING
####Combining data tables from Boston, New York, San Francisco.
##combining-data, eval=TRUE
joblisttidy <- rbind(joblistma, joblistny, joblistsf, joblistchi)


####Eliminating duplicate entries.
##\n\ Since some companies post the same job on different days, the posted date column is elimintated prior to testing for duplicate entries.
##eliminate_duplicates, eval=TRUE
joblisttidy <- joblisttidy[,-5]
joblisttidy <- subset(joblisttidy, duplicated(joblisttidy)==FALSE)


####Eliminating symbols in front of location.
##location, eval = TRUE
joblisttidy$location <- unlist(str_extract_all(joblisttidy$location, "[[:upper:]]{1}[[:lower:]]{2,}, [[:alpha:]]{2}|[[:upper:]]{1}[[:lower:]]{2,} [[:alpha:]]{2,}, [[:alpha:]]{2}"))
joblisttidy$description <- gsub("[^[:ascii:]]", "", joblisttidy$description, perl=T)


####Identifying job descriptions that look for specific computer skills.
##computerskills, eval=TRUE
compskills <- joblisttidy %>%
mutate(python = grepl("python", description, ignore.case=TRUE)) %>%
mutate(perl = grepl("perl", description, ignore.case=TRUE)) %>%
mutate(Cplusplus = grepl("C++", description, fixed=TRUE)) %>%
mutate(SQL = grepl("SQL", description)) %>%
mutate(java = grepl("java\\b", description, ignore.case=TRUE)) %>%
mutate(javascript = grepl("javascript", description, ignore.case=TRUE)) %>%
mutate(R = grepl("\\bR\\b,", description)) %>%
mutate(hadoop = grepl("hadoop", description, ignore.case=TRUE)) %>%
mutate(spark = grepl("spark", description, ignore.case=TRUE)) %>%
mutate(scala = grepl("scala", description, ignore.case=TRUE)) %>%
select(job_title, company, python, perl, Cplusplus, SQL, java, javascript, R, hadoop, spark, scala)
datatable(compskills)
summary(compskills)


####Identifying analytical skills.
##analytical-skills, eval=TRUE
skills <- joblisttidy %>%
mutate(machinelearning = grepl("machine learning", description, ignore.case=TRUE)) %>%
mutate(statisticalmodeling = grepl("statistical model", description, ignore.case=TRUE)) %>%
mutate(techwriting = grepl("technical writing", description, ignore.case=TRUE)) %>%
mutate(plateau = grepl("plateau", description, ignore.case=TRUE)) %>%
mutate(d3 = grepl("D3", description)) %>%
select(job_title, company, machinelearning, statisticalmodeling, techwriting, plateau, d3)
datatable(skills)
summary(skills)


####Identifying soft skills.
##soft-skills, eval=TRUE
softskills <- joblisttidy %>%
mutate(collaborative = grepl("collaborat", description, ignore.case=TRUE)) %>%
mutate(organized = grepl("organized", description, ignore.case=TRUE)) %>%
mutate(selfstarter = grepl("self starter", description, ignore.case=TRUE)) %>%
mutate(attndetail = grepl("attention to detail", description, ignore.case=TRUE)) %>%
mutate(communication = grepl("communicat", description, ignore.case=TRUE)) %>%
mutate(creative = grepl("creativ", description, ignore.case=TRUE)) %>%
mutate(visualization = grepl("visualization", description, ignore.case=TRUE)) %>%
select(job_title, company, collaborative, organized, selfstarter, attndetail, communication, creative, visualization)
summary(softskills)  


##Create dataframe from the soft skills summary data to prepare for visualization
softskills_df <- do.call(cbind, lapply(softskills, summary))
softskills_df <- softskills_df[-c(1),]
softskills_df <- softskills_df[,-c(1:2)]
rownames(softskills_df) <- c("True","False")
datatable(softskills_df)



##4) VISUALIZE
####Create visualizations of the data and draw conclusions.

##names <- Combined$softskills
##barplot(Combined$softskills,main="Soft Skills", horiz=TRUE, names.arg=names, las=1, col=darkcols, cex.axis=0.5, cex.names = 0.5)

##ggplot(data = softskills, aes(x = Batch, y = as.numeric(collaborative), fill = Batch))
##+ stat_summary(fun.y = sum, geom = "bar")



####Importing the data from Glassdoor for Boston
```{r importingdataMA, eval=TRUE}
-joblistma <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_final_with_dupes_reduced.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
+joblistma <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_final_with_dupes_reduced.csv", stringsAsFactors = FALSE)
```

####Importing data from Glassdoor for NYC
```{r importingdataNY, eval=TRUE}
-joblistny <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_ny.csv", stringsAsFactors=FALSE, fileEncoding = "latin1")
+joblistny <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_ny.csv", stringsAsFactors=FALSE)
```


####Importing data from Glassdoor for SF
```{r importingdataSF, eval=TRUE}
-joblistsf <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_SF.csv", stringsAsFactors=FALSE, fileEncoding = "latin1")
+joblistsf <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_SF.csv", stringsAsFactors=FALSE)
joblistsf <- joblistsf[,-1]
```

####Importing data from Glassdoor for CHI
```{r importingdataSF, eval=TRUE}
-joblistchi <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_CHI.csv", stringsAsFactors=FALSE, fileEncoding = "latin1")
+joblistchi <- read.csv("https://raw.githubusercontent.com/delagroove/dataScience/master/jobOffers_CHI.csv", stringsAsFactors=FALSE)
joblistchi <- joblistchi[,-1]
```

@@ -101,6 +101,16 @@ compskills <- joblisttidy %>%
  mutate(scala = grepl("scala", description, ignore.case=TRUE)) %>%
  select(job_title, company, python, perl, Cplusplus, SQL, java, javascript, R, hadoop, spark, scala)
summary(compskills)
+barplot(table(compskills$python),main = "Python")
+barplot(table(compskills$perl),main = "Perl")
+barplot(table(compskills$Cplusplus),main = "C++")
+barplot(table(compskills$SQL),main = "SQL")
+barplot(table(compskills$java),main = "Java")
+barplot(table(compskills$javascript),main = "JavaScript")
+barplot(table(compskills$R),main = "R")
+barplot(table(compskills$hadoop),main = "Hadoop")
+barplot(table(compskills$spark),main = "Spark")
+barplot(table(compskills$scala),main = "Scala")
```

####Identifying analytical skills
@@ -112,7 +122,12 @@ skills <- joblisttidy %>%
  mutate(plateau = grepl("plateau", description, ignore.case=TRUE)) %>%
  mutate(d3 = grepl("D3", description)) %>%
  select(job_title, company, machinelearning, statisticalmodeling, techwriting, plateau, d3)
-summary(skills)  
+summary(skills)
+barplot(table(skills$machinelearning),main = "Machine Learning")
+barplot(table(skills$statisticalmodeling),main = "Statistical Modeling")
+barplot(table(skills$techwriting),main = "Technical Writing")
+barplot(table(skills$plateau),main = "Plateau")
+barplot(table(skills$d3),main = "D3")
```

####Identifying soft skills
@@ -126,5 +141,12 @@ softskills <- joblisttidy %>%
  mutate(creative = grepl("creativ", description, ignore.case=TRUE)) %>%
  mutate(visualization = grepl("visualization", description, ignore.case=TRUE)) %>%
  select(job_title, company, collaborative, organized, selfstarter, attndetail, communication, creative, visualization)
-summary(softskills)  
-```
+summary(softskills)
+barplot(table(softskills$collaborative),main = "Collaborative")
+barplot(table(softskills$organized),main = "Organized")
+barplot(table(softskills$selfstarter),main = "Self Starter")
+barplot(table(softskills$attndetail),main = "Attention to Detail")
+barplot(table(softskills$communication),main = "Communication")
+barplot(table(softskills$creative),main = "Creative")
+barplot(table(softskills$visualization),main = "Visualization")
+```

