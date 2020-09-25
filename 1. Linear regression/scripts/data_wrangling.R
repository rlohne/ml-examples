#Load data
Life_Expectancy_Data <- read_csv("1. Linear regression/data/Life Expectancy Data.csv") # source: https://www.kaggle.com/kumarajarshi/life-expectancy-who
continent_table <- read_delim("1. Linear regression/data/continent_table.csv", ";", escape_double = FALSE, trim_ws = TRUE) # https://simple.wikipedia.org/wiki/List_of_countries_by_continents
country_government <- read_delim("1. Linear regression/data/datacountry_government.csv", ";", escape_double = FALSE, trim_ws = TRUE)


#Enrich Life_Expectancy_Data

Life_Expectancy_Data$Continent <- vlookup(Life_Expectancy_Data$Country, continent_table, 2, 1) # Add continent
country_government$Name <- str_trim(country_government$Name) # Trim whitespace from beginning of string
Life_Expectancy_Data$Governtment <- vlookup(Life_Expectancy_Data$Country, country_government, 2, 1) # Add form of government
GDP <- Life_Expectancy_Data$GDP[!is.na(Life_Expectancy_Data$GDP)] # Get the GDP-data
quantiles <- quantile(GDP) # split into quantiles
Life_Expectancy_Data$GDP <- round(Life_Expectancy_Data$GDP, 0) # Round GDP
Life_Expectancy_Data$Quantile <- cut_number(Life_Expectancy_Data$GDP, n = 5, dig.lab = 6) # Cut into qunatiles
Life_Expectancy_Data <- Life_Expectancy_Data %>% mutate(Quantilegroup = ntile(GDP, 4)) # Add quantile groups

LED <- Life_Expectancy_Data %>%
  rename(life_expectancy = `Life expectancy`) %>%
  rename(adult_mortality = `Adult Mortality`) %>%
  rename(hep_b = `Hepatitis B`) %>%
  rename(total_exp = `Total expenditure`) %>%
  rename(thin_young = `thinness  1-19 years`) %>%
  rename(thin_child = `thinness 5-9 years`) %>%
  rename(income_comp = `Income composition of resources`) %>%
  rename(infant_deaths = `infant deaths`) %>%
  rename(perc_expend = `percentage expenditure`) %>%
  rename(infant_mort = `under-five deaths`) %>%
  rename(hiv_aids = `HIV/AIDS`)
  
  
  

# Description of the dataset
# Status = Developed or Developing status
# Life expectancy = Life Expectancy in age
# Adult mortality = Adult Mortality Rates of both sexes (probability of dying between 15 and 60 years per 1000 population)
# Infant deaths = Number of Infant Deaths per 1000 population
# Alcohol = Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)
# Percentage expenditure = Expenditure on health as a percentage of Gross Domestic Product per capita(%)
# Hepatitis B = Hepatitis B (HepB) immunization coverage among 1-year-olds (%)
# Measles = Measles - number of reported cases per 1000 population
# BMI = Average Body Mass Index of entire population
# under-five-deaths = Number of under-five deaths per 1000 population
# Polio = Polio (Pol3) immunization coverage among 1-year-olds (%)
# Total expenditure = General government expenditure on health as a percentage of total government expenditure (%)
# Diphteria = Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds (%)
# HIV/AIDS = Deaths per 1 000 live births HIV/AIDS (0-4 years)
# GDP = Gross Domestic Product per capita (in USD)
# Population = Population of the country
# Thinness 1-19 = Prevalence of thinness among children and adolescents for Age 10 to 19 (% )
# Thinness 5-9 = Prevalence of thinness among children for Age 5 to 9(%)
# Income composition of resources = Human Development Index in terms of income composition of resources (index ranging from 0 to 1)
# Schooling = Number of years of Schooling(years average) 
# Continent = Which content does the country belong to
# Government = Which form of government does the country have
# Quantile = what quantile range of GDP does the country fall under
# Quantilegroup = what group (1-4) does the country belong in (GDP)
 

# Visualize missingness

vis_miss(Life_Expectancy_Data) #Visualize overall missingness



tempData <- mice(LED[,1:26], m=5,maxit=50,meth='cart',seed=500)
summary(tempData)

tic()
temp_data <- parlmice(
  LED[,1:26],
  m = 8,
  seed = NA,
  maxit = 50,
  cluster.seed = 500,
  n.core = NULL,
  n.imp.core = NULL,
  cl.type = "PSOCK",
  method = "cart"
)
toc()

led_imp <- complete(temp_data, "long", inc = TRUE)

densityplot(temp_data)