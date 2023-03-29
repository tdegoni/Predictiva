
link_ppt="https://docs.google.com/presentation/d/1XxPK4pNFoj43J3J6qtSv8YiROG1njnN2p8cFPZr1UZw/edit?usp=sharing"
link_bd="https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets"


bd <- read.csv("C:/Users/Usuario/Desktop/ITBA/Analitica Predicitva/TP 1/train.csv", sep=";")

library(tidyverse)
library(janitor)
library(skimr)
library(ggpubr)
library(dplyr)



# 1 - age (numeric)
# 2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
#                        "blue-collar","self-employed","retired","technician","services")
# 3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
# 4 - education (categorical: "unknown","secondary","primary","tertiary")
# 5 - default: has credit in default? (binary: "yes","no")
# 6 - balance: average yearly balance, in euros (numeric)
# 7 - housing: has housing loan? (binary: "yes","no")
# 8 - loan: has personal loan? (binary: "yes","no")
# 
# related with the last contact of the current campaign:
#   
#   9 - contact: contact communication type (categorical: "unknown","telephone","cellular")
# 
# 10 - day: last contact day of the month (numeric)
# 11 - month: last contact month of year (categorical: "jan", "feb", "mar", …, "nov", "dec")
# 12 - duration: last contact duration, in seconds (numeric)
# 
# other attributes:
#   
#   13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# 
# 14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
# 15 - previous: number of contacts performed before this campaign and for this client (numeric)
# 16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
# Output variable (desired target):
#   
#   17 - y - has the client subscribed a term deposit? (binary: "yes","no")

bd %>% summarise_if(is.numeric, list(media=mean, max=max, min=min))

skim(bd)

skim_eda = partition(skim(bd))
names(skim_eda)

bd_num =bd %>% select_if(is.numeric)
bd_cat = bd %>% select_if(function (x) !is.numeric(x))


#quantiles para ver bien como se distribuye pdays y duration

quantile(bd_num$pdays, seq(0,1,0.01))

quantile(bd_num$duration/60, seq(0,1,0.01))

quantile(bd_num$previous, seq(0,1,0.01))
#Tablas

table(bd_cat$job)

tabs = bd_cat %>% map(function(x) table(x, useNA="ifany"))
tabs


#Trate de crear varios gráficos separando por la variable target en cada categórica, no es muy visualmente claro
#asi que lo deje afuera

# Create the table object
table <- apply(bd_cat[, -ncol(bd_cat)], 2, function(x) table(x, bd_cat$y))

table[[1]]

# Define a function to create a bar chart given a categorical variable
create_bar_chart <- function(var_name) {
  var_table <- table(bd_cat[[var_name]], bd_cat$y)
  var_table_df <- as.data.frame(var_table)
  var_table_df <- var_table_df %>%
    mutate(Category = rownames(var_table_df)) %>%
    rename(`No` = Var1, `Yes` = Var2) %>%
    gather(key = "Outcome", value = "Count", -Category)
  
  ggplot(var_table_df, aes(x = Category, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle(paste("Distribution of ", var_name, " vs. Term Deposit")) +
    xlab(var_name) +
    ylab("Count") +
    theme_minimal()
}


# Apply the function to each categorical variable in bd_cat
cat_vars <- names(bd_cat)[1:9] # replace with your categorical variables
plots <- lapply(cat_vars, create_bar_chart)

# Display the plots
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]


#Graficos para la edad

ggplot(bd, aes(x=age, fill=y)) +
  geom_histogram(alpha=0.5) 


#age

ggplot(bd, aes(x=age, fill=y)) +
  geom_density(alpha=0.5, adjust=2)

#previous


bd_prev=bd[bd$previous<quantile(bd$previous, probs = 0.99, na.rm = TRUE),]

ggplot(bd_prev, aes(x=previous, fill=y)) +
  geom_density(alpha=0.5, adjust=2)


#pdays

bd_pdays=bd[bd$pdays>0,] %>% select(pdays,y)

ggplot(bd_pdays, aes(x=pdays, fill=y)) +
  geom_density(alpha=0.5, adjust=2)
ggplot(bd_pdays, aes(x=pdays, fill=y)) +
  geom_histogram(alpha=0.5, adjust=2)


#duration
ggplot(bd, aes(x=duration/60, fill=y)) +
  geom_density(alpha=0.5, adjust=2)
ggplot(bd, aes(x=duration)) +
  geom_histogram(alpha=0.5, adjust=2)
ggplot(bd, aes(y=duration)) +
  geom_boxplot()


#balance

ggplot(bd, aes(x=balance)) +
  geom_histogram(alpha=0.5) +
  NULL

ggplot(bd, aes(x=log(balance))) +
  geom_histogram(alpha=0.5) +
  NULL

ggplot(bd, aes(y=log(balance))) +
  geom_boxplot()


ggplot(bd, aes(x=log(balance), fill=y)) +
  geom_histogram(alpha=0.5)


#Graficos Barras 
#Job
ggplot(bd) +
  geom_bar(aes(x=job, fill=y)) +
  NULL
#education
ggplot(bd) +
  geom_bar(aes(x=education, fill=y), position="dodge") 
#Contact
ggplot(bd) +
  geom_bar(aes(x=contact, fill=y)) +
  NULL
#Month

ggplot(bd, aes(x = as.factor(month), fill = y)) +
  geom_bar() +
  NULL

#poutcome

ggplot(bd) +
  geom_bar(aes(x=poutcome, fill=y)) +
  NULL


#Correlacion

y_numeric <- ifelse(bd_cat$y == "no", 0, 1)

# Add the y_numeric variable to bd_num
bd_num$y <- y_numeric

GGally::ggcorr(
  bd_num, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)


#Cramer V
#install.packages("vcd")
library(vcd)

# Create a function to calculate Cramer's V for each categorical variable
calc_cramers_v <- function(var_name) {
  table <- table(bd_cat[[var_name]], bd_cat$y)
  assocstats(table)$cramer
}

# Apply the function to each categorical variable in bd_cat
cat_vars <- names(bd_cat)[1:9] # replace with your categorical variables
cramers_v <- sapply(cat_vars, calc_cramers_v)

# Display the results
cramers_v



#Otro metodo para ver corr entre categ, no lo use al final
library(entropy)


# Create a function to calculate Theil's U for each categorical variable
calc_theils_u <- function(var_name) {
  table <- table(bd_cat[[var_name]], bd_cat$y)
  n <- sum(table)
  U <- 1 - (entropy::entropy(table) - entropy::crossentropy(table))/entropy::entropy(matrix(colSums(table),nrow = 1))
  U
}

# Apply the function to each categorical variable in bd_cat
cat_vars <- names(bd_cat)[1:9] # replace with your categorical variables
theils_u <- sapply(cat_vars, calc_theils_u)

# Display the results
theils_u








