---
  title: "R Notebook"
output: html_notebook
---
  
  {r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

############################################################
## Define packages to be loaded, and if needed, installed ##
##                                                        ##    
## by robert lohne (rlohne@gmail.com / @robertlohne)      ##
############################################################

# 1 Definitions
# 1.1 Package names 
# Define the names of the packages to be used, these are stored in the packages variable, and loaded/installed as needed
packages <- c("gapminder", "dplyr", "ggplot2", "gridExtra", "tidymodels", "titanic", "mice", "visdat", "naniar", "parallel")

# 1.1 Packages description
# 

# 2. Install packages not yet installed
# Installed packages are stored in the installed packages variable
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# 3. Packages loading
invisible(lapply(packages, library, character.only = TRUE))

1. Introduction to logistic regression

Where linear regression is commonly used to predict continuous outcome variables, it can also be used for binary classifications. However, because it cannot easily predict qualitative outcome variables with more than two levels, another approach is needed. In this notebook, we'll look at logistic regression. There are indeed others as well, and we'll get to those in another notebook.

1.1 Binary outcomes - icebergs ahoi!
  
  Let's first consider a classic classification task; a binary outcome. In this case, we will use the Titanic dataset, and predict whether or not passengers survive. We're going to work with what we have in the dataset, and also try to do some feature engineering. The Titanic dataset is found in the titanic-package, and features two datasets, a training and a test set. The training data is labelled with Survived coded as a binary (0 = did not survive, 1 = survived) variable. The test data lacks the Survived variable.

In this example, we are dealing with a binomial, or binary logistic regression. This means that the outcome variable have only two possible types. A multinominal logistic regression is when the outcome variable can have three or more possible types, but they are not ordered (for instance if we tried to predict whether a car in the mtcars dataset was a SUV, a convertible or a sedan. The last type is the ordinal logistic regression, which is similar to the multinominal, but the levels are ordered. For instance if we tried to predict which Class a passenger traveled in on the Titanic.Mathematically, the formula for the probability of $Y$ being equal to 1 (Survived) can be expressed as:
                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                      $\log_b \frac{\rho}{1- \rho} = \beta_0 + B_1\chi_1 + B_2\chi_2$
                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                      where $log$ symbolizes the log-odds (logit), $_b$ is the base of the logarithm, and $\beta_1$, and $\beta_2$the parameters of the model, and $X$ the predictors.
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    1.2 Looking at our data
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    Let's look at our data. First, we take a look at the variables, their types and if any data are missing. To do this, I am going to use the vis_dat() function for the visdat-package. I like visdat because it has some very intuitive to use functions for visualizing a dataset - in particular when it comes to missing data. Let's take a look at the training data first:
                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                      {r visualize the training data}
                                                                                                                                                                                                                                                                                    train <- titanic_train
                                                                                                                                                                                                                                                                                    test <- titanic_test
                                                                                                                                                                                                                                                                                    vis_dat(train)
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    So - we are looking at five character variables, five integer variables, and two numeric variables. Only the age variable has missing data. Let's explore this a bit further, using the vis_miss() function:

{r visualize missing data}
vis_miss(train)

About 20 % of the rows are missing data in the Age column; in total this amounts to 1,7 % of the data. However, dropping 20 % of the rows because we have missing data doesn't sound attractive. I have a hunch that age will be an important predictor, but let's see if there are any clues:

{r plotting survival explained by age}

train %>% 
  mutate(Agegroup = cut(Age, breaks = c(0,10,20,30,40,50,60,70,80))) %>%
  group_by(Agegroup) %>%
  summarise(
    count = n(),
    survival_rate = sum(Survived)/count) %>%
  ggplot(aes(x = Agegroup, y = survival_rate))+
  scale_fill_gradient(low="red", high="blue") +
  geom_col(aes(fill = survival_rate))+
  labs(
    title = "Survival rates are higher among the youngest",
    caption = "Source: Titanic dataset",
    fill = "Survival rate"
  ) + 
  xlab("Age groups")+
  ylab("Survival rate")
  
  



And for our test data, this seems to be the case (since we are missing the Survived variable in our test set, we have no way of assessing the same for the test dataset).

1.2.1 Dealing with missing data

So, on order to deal with the missing age-data, we are going to impute it. It is hard to know whether the data is Missing at Random (MAR), Missing Completely at Random (MCAR) or Missing not at Random (MNAR). It is most likely not MCAR. It could be MAR, but there also might be underlying patterns that is hard to see. If they are MAR or MCAR, it is usually safe to remove the data, but if we remove a lot of observations that are missing data due to MNAR, we might introduce a bias in our data. This warrants more exploration of the data.

Step 1 is the visualization we saw earlier;

{r visualize missing data part 2}
vis_miss(train)

From the observations alone, it looks to be pretty evenly distributed. But this is just row-wise. Let's look at missing data by other variables, to see if there are any patterns. For this, we turn to the naniar package, and some useful functions. First, let's look at gg_miss_var(), which allows us to view missingness across variables, using the facet argument. In order to show the percentage, set show_pct to TRUE:

{r visualizing missingness by variables}
gg_miss_var(train,
            facet = Pclass,
            show_pct = TRUE) + 
  ggtitle("Missingness across passenger classes")


There seems to be a fair amount of variations across passenger class. While 1st class is missing about 15 % of the age variables, it's only about 7% for 2nd class, and almost 30 % for 3rd class. This tells us that we might be dealing with the MNAR kind of data, which means it could be a bad idea to remove the rows we are missing.
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    1.2.2 Feature engineering
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    Before we impute, let's see if our data could benefit from some feature engineering. Feature engineering means that we create new variables, often by wrangling our data or combining variables or interpreting them.

{r glimpse our variables}
glimpse(train, width = 75)

Looking at our data, some seems to stick out. PassengerId is not likely to be useful, so I think we can drop that. However, the Name column also contains the title. Let's explore that a bit.
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    {r split titles}
                                                                                                                                                                                                                                                                                    train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
                                                                                                                                                                                                                                                                                    as.data.frame(table(train$Title))
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    This leaves us with 17 different titles. Some have a low number of occurrences in the data, so let's pile those into a group to avoid having too many levels in the title factor. Others are variations of the same title, in different languages, such as Mlle (mademoiselle) and Ms/Miss.

{r wrangling the titles}
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
as.data.frame(table(train$Title))
titles <- table(train$Title)
rare_titles <- rownames(titles[titles < 10])
train$Title[train$Title %in% rare_titles] <- "Rare title"
train$Title[train$Title == "Mlle"] <- "Miss"
train$Title[train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mme"]<- "Mrs"

train %>% 
  group_by(Title) %>%
  summarise(
    count = n(),
    survival_rate = sum(Survived)/count) %>%
  ggplot(aes(x = Title, y = survival_rate))+
  scale_fill_gradient(low="red", high="blue") +
  geom_col(aes(fill = survival_rate))+
  labs(
    title = "Survival rates significantly lower among common men",
    caption = "Source: Titanic dataset",
    fill = "Survival rate"
  ) + 
  xlab("Titles")+
  ylab("Survival rate")

Ok! This looks interesting. Less than 1 in 5 men with the title "Mr" survived, compared to almost 4 in 5 among married women. Men bearing the title "Master" fared much better, almost 6 in 10 survived.

Another feature we can look at, is the combination of Sibsp (sibling and spouces) and Parch (parent and children). Going by this, we can create a variable for the size of each family the passenger belonged to (of the family members present on the Titanic).

{r wrangling family size}
train$Fam_size <- train$SibSp + train$Parch + 1

train %>% 
  group_by(Fam_size) %>%
  summarise(
    count = n(),
    survival_rate = sum(Survived)/count) %>%
  ggplot(aes(x = Fam_size, y = survival_rate))+
  scale_fill_gradient(low="red", high="blue") +
  geom_col(aes(fill = survival_rate))+
  scale_x_continuous(breaks = c(1:10)) +
  labs(
    title = "Survival rates are lower for passengers traveling alone or in large families",
    caption = "Source: Titanic dataset",
    fill = "Survival rate"
  ) + 
  xlab("Family size")+
  ylab("Survival rate")


1.3 Dealing with missing data

Imputing data carries it's own risks, but nonetheless we will attempt. There are many ways to go about this, from the really simple (i.e. impute with mean), to using specialized functions such as impute_lm() from the naniar package, to the mice package. In this case, I will use the latter.
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    {r impute, include=FALSE}
                                                                                                                                                                                                                                                                                    # Wrangle data
                                                                                                                                                                                                                                                                                    train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
                                                                                                                                                                                                                                                                                    titles <- table(train$Title)
                                                                                                                                                                                                                                                                                    rare_titles <- rownames(titles[titles < 10])
                                                                                                                                                                                                                                                                                    train$Title[train$Title %in% rare_titles] <- "Rare title"
                                                                                                                                                                                                                                                                                    train$Title[train$Title == "Mlle"] <- "Miss"
                                                                                                                                                                                                                                                                                    train$Title[train$Title == "Ms"] <- "Miss"
                                                                                                                                                                                                                                                                                    train$Title[train$Title == "Mme"]<- "Mrs"
                                                                                                                                                                                                                                                                                    train$Fam_size <- train$SibSp + train$Parch + 1
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    # prepare for imputation
                                                                                                                                                                                                                                                                                    impute_variables <- names(train)[3:14]
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    cores_2_use <- detectCores() - 1
                                                                                                                                                                                                                                                                                    cl <- makeCluster(cores_2_use)
                                                                                                                                                                                                                                                                                    clusterSetRNGStream(cl, 42)
                                                                                                                                                                                                                                                                                    clusterExport(cl, "train")
                                                                                                                                                                                                                                                                                    clusterEvalQ(cl, library(mice))
                                                                                                                                                                                                                                                                                    imp_pars <- 
                                                                                                                                                                                                                                                                                      parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
                                                                                                                                                                                                                                                                                        mice(train, m = 60, printFlag = FALSE)
                                                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                                                    stopCluster(cl)
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    imp_merged <- imp_pars[[1]]
                                                                                                                                                                                                                                                                                    for (n in 2:length(imp_pars)){
                                                                                                                                                                                                                                                                                      imp_merged <- 
                                                                                                                                                                                                                                                                                        ibind(imp_merged,
                                                                                                                                                                                                                                                                                              imp_pars[[n]])
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    train <- mice::complete(imp_merged)
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    2. Training the model
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    So, we have our training set completed. Let's look at it, and decide on the model.

{r}
glimpse(train, width = 30)


PassengerId and Ticket isn't going to help us, and Cabin has a lot of blanks. I don't think Embarked is going to help either. Let's try the following:
                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                      {r train the model}
                                                                                                                                                                                                                                                                                    log_titanic <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + Fam_size, data = train, family = binomial)
                                                                                                                                                                                                                                                                                    train$Pred <- predict(log_titanic, type = "response")
                                                                                                                                                                                                                                                                                    train$.pred_yes <- round(0+train$Pred,2)
                                                                                                                                                                                                                                                                                    train$.pred_no <- round(1-train$Pred,2)
                                                                                                                                                                                                                                                                                    train$Survived_fct <- ifelse(train$Survived == 1, "Yes", "No")
                                                                                                                                                                                                                                                                                    train$Pred <- ifelse(train$Pred > 0.5, "Yes", "No")
                                                                                                                                                                                                                                                                                    train$Survived_fct <- as.factor(train$Survived_fct)
                                                                                                                                                                                                                                                                                    train$Pred <- as.factor(train$Pred)
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    titanic_metrics <- metric_set(accuracy, sens, spec)
                                                                                                                                                                                                                                                                                    titanic_metrics(train, truth = Survived_fct,
                                                                                                                                                                                                                                                                                                    estimate = Pred)
                                                                                                                                                                                                                                                                                    threshold_df <- train %>% 
                                                                                                                                                                                                                                                                                      roc_curve(truth = Survived_fct, .pred_yes)
                                                                                                                                                                                                                                                                                    threshold_df %>% 
                                                                                                                                                                                                                                                                                      autoplot(threshold_df)
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    roc_auc(train,
                                                                                                                                                                                                                                                                                            truth = Survived_fct, 
                                                                                                                                                                                                                                                                                            .pred_yes)
                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                    