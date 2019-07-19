#This code includes all the simulations for all quantile regressions included in the final paper

#### Packages ####
require("tidyverse")  # Data manipulation
library("lubridate")  # Date/hour manipulation
library("stringr")    # String manipulation
library("scales")     # Personalized graph scales
library("cowplot")
library("reshape")
library("moments")    # For skewness and kurtosis
library("pwt9")       # Penn World Tables
library("quantreg")   # Quantile regression
library("readr")
library("wesanderson") # Palettes
library("ggmap")      # Spatial data management

#source("cross_validation_function.R")   #function to perform cross validation

#### Preliminary Operations ####
theme_set(theme_bw()) # Set graph theme

disasters <- read_csv("data/disasters_koppen.csv", n_max=10902) #load geolocalized database

#add contintent codes
continents <- read_csv("data/continents.csv")
continents <- plyr::rename(continents, c("alpha-3" = "ISO"))


continents[250:260, ] = NA
continents[250:260, 3] = c("SUN" ,"AZO", "YUG", "SPI" ,"CSK", "DDR", "DFR", "YMN", "YMD", "SCG" , "ANT")
continents[250:260, 11] = c("AS" ,"EU", "EU", "EU" ,"EU",   "EU", "EU", "AS", "AS", "EU", "AM") 
continents[which(is.na(continents$continent)),]$continent <- "AM"
disasters <- merge(disasters, continents[ , c("ISO", "continent")], by.x = c("ISO"), by.y = c("ISO"))

disasters$continent <- factor(disasters$continent, 
                              levels = c("AF", "AM", "AS", "EU", "OC", "SA"),
                              labels = c("Africa", "North America", "Asia", "Europe", "Oceania", "South America"))


#Add level of income
income_class <- read_delim("data/income_class.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

income_class[267:292, 1] =  c("AIA", "ANT", "AZO", "COD", "COK", "CSK", "DDR", "DFR", "GLP", "GUF", "MSR", "MTQ", "NIU", "PSE", "REU", "ROU", "SCG", "SHN", "SPI", "SUN", "TKL", "TLS", "WLF", "YMD", "YMN", "YUG")
income_class[267:292, 2] = c("High income", "High income", "High income", "Low income", "High income", "High income", "High income", "High income", "Upper middle income", "Upper middle income", "Upper middle income", "High income", "Upper middle income", "Upper middle income", "High income", "Upper middle income", "Upper middle income", "Lower middle income", "High income", "Upper middle income", "Low income", "Lower middle income", "Upper middle income", "Lower middle income", "Lower middle income", "Upper middle income" )


# merge two data frames by ID
disasters <- merge(disasters, income_class, by="ISO")


#### PWT ####
data("pwt9.0")
pwt9.0$country=as.character(pwt9.0$country)
pwt9.0$isocode=as.character(pwt9.0$isocode)

#German Federal Republic to Germany
pwt9.0[pwt9.0$country=="Germany",] ->germany #65 osservazioni
#Serbia Montenegro to Serbia
pwt9.0[pwt9.0$country=="Serbia",] -> serbia #65 osservazioni
#aMerge two Yemens
pwt9.0[pwt9.0$country=="Yemen",] -> yemen #65 osservazioni

pwt9.0[11831:11895,] <- germany
pwt9.0[11831:11895,1] = "Germany Fed Rep"
pwt9.0[11831:11895,2] = "DFR"

pwt9.0[11896:11960,] <- serbia
pwt9.0[11896:11960,1] = "Serbia Montenegro"
pwt9.0[11896:11960,2] = "SCG"

pwt9.0[11961:12025,] <- yemen
pwt9.0[11961:12025,1] = "Yemen P Dem Rep"
pwt9.0[11961:12025,2] = "YMD"

pwt9.0[12026:12090,] <- yemen
pwt9.0[12026:12090,1] = "Yemen Arab Rep"
pwt9.0[12026:12090,2] = "YMN"
remove(germany, serbia, yemen)


# Res tells which cuntries are present in disaster dataset and not in PWT
pwiso <- pwt9.0$isocode %>% unique()
disiso <- disasters$ISO %>% unique()
intersection <- intersect(pwiso,disiso)
res <- disiso[is.na(pmatch(disiso,intersection))]

sum(disasters$ISO %in% res) # Number of observations uncovered by PWT
pwt_missing <- disasters[disasters$ISO %in% res,]
table(pwt_missing$country)
remove(disiso, pwiso, intersection, res)

pwt9.0 <- plyr::rename(pwt9.0, c("isocode" ="ISO"))
odisasters <- disasters # save original with all observations
disasters <- merge(disasters, pwt9.0[ , c("year", "ISO", "cgdpo", "ck", "pop")], by.x = c("ISO", "year"), by.y = c("ISO", "year"))

#TYPO in PWT: negative GDP estimates
disasters$cgdpo[disasters$index==1107] = -disasters$cgdpo[disasters$index==1107]

#### ISO Armonization ####

# Keep track of original country, add the relative string into location
disasters$location <- ifelse(disasters$ISO == "ARM",paste("Armenia", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "AZE",paste("Azerbaijan", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "BLR",paste("Belarus", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "EST",paste("Estonia", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "GEO",paste("Georgia", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "KAZ",paste("Kazakhstan", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "LTV",paste("Latvia", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "LTU",paste("Lithuania", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "MDA",paste("Moldova (the Republic of)", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "RUS",paste("Russian Federation (the)", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "TJK",paste("Tajikistan", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "TKM",paste("Turkmenistan", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "UKR",paste("Ukraine", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "UZB",paste("Uzbekistan", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "SRB",paste("Serbia", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "MNE",paste("Montenegro", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "BIH",paste("Bosnia and Herzegovina", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "HRV",paste("Croatia", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "MKD",paste("Macedonia (the former Yugoslav Republic of)", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "SCG",paste("Serbia Montenegro", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "SVN",paste("Slovenia", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "CZE",paste("Czech Republic (the)", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "SVK",paste("Slovakia", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "DDR",paste("Germany Dem Rep", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "DFR",paste("Germany Fed Rep", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "YMN",paste("Yemen Arab Rep", disasters$location, sep=" - "), disasters$location)
disasters$location <- ifelse(disasters$ISO == "YMD",paste("Yemen P Dem Rep", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "PSE",paste("Palestine, State of", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "TLS",paste("Timor-Leste", disasters$location, sep=" - "), disasters$location)

disasters$location <- ifelse(disasters$ISO == "ERI",paste("Eritrea", disasters$location, sep=" - "), disasters$location)

#Change country names and relative ISOs
disasters <- disasters %>%
  mutate(
    # Group former URRS Countries (Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Moldova (the Republic of), Russian Federation (the), Tajikistan, Turkmenistan, Ukraine, Uzbekistan) under Soviet Union
    country = replace(country, country == "Armenia", "Soviet Union"),
    ISO = replace(ISO, ISO == "ARM", "SUN"), #Armenia
    country = replace(country, country == "Azerbaijan", "Soviet Union"),
    ISO = replace(ISO, ISO == "AZE", "SUN"), #Azerbaijan
    country = replace(country, country == "Belarus", "Soviet Union"),
    ISO = replace(ISO, ISO == "BLR", "SUN"), #Belarus
    country = replace(country, country == "Estonia", "Soviet Union"),
    ISO = replace(ISO, ISO == "EST", "SUN"), #Estonia
    country = replace(country, country == "Georgia", "Soviet Union"),
    ISO = replace(ISO, ISO == "GEO", "SUN"), #Georgia
    country = replace(country, country == "Kazakhstan", "Soviet Union"),
    ISO = replace(ISO, ISO == "KAZ", "SUN"), #Kazakhstan
    country = replace(country, country == "Latvia", "Soviet Union"),
    ISO = replace(ISO, ISO == "LTV", "SUN"), #Latvia
    country = replace(country, country == "Lithuania", "Soviet Union"),
    ISO = replace(ISO, ISO == "LTU", "SUN"), #Lithuania
    country = replace(country, country == "Moldova (the Republic of)", "Soviet Union"),
    ISO = replace(ISO, ISO == "MDA", "SUN"), #Moldova (the Republic of)
    country = replace(country, country == "Russian Federation (the)", "Soviet Union"),
    ISO = replace(ISO, ISO == "RUS", "SUN"), #Russian Federation (the)
    country = replace(country, country == "Tajikistan", "Soviet Union"),
    ISO = replace(ISO, ISO == "TJK", "SUN"), #Tajikistan
    country = replace(country, country == "Turkmenistan", "Soviet Union"),
    ISO = replace(ISO, ISO == "TKM", "SUN"), #Turkmenistan
    country = replace(country, country == "Ukraine", "Soviet Union"),
    ISO = replace(ISO, ISO == "UKR", "SUN"), #Ukraine
    country = replace(country, country == "Uzbekistan", "Soviet Union"),
    ISO = replace(ISO, ISO == "UZB", "SUN"), #Uzbekistan
    
    #Group Serbia and Montenegro under Serbia Montenegro
    country = replace(country, country == "Serbia", "Serbia Montenegro"),
    ISO = replace(ISO, ISO == "SRB", "SCG"), #Serbia
    country = replace(country, country == "Montenegro", "Serbia Montenegro"),
    ISO = replace(ISO, ISO == "MNE", "SCG"), #Montenegro
    
    #Group ex-Jugoslavia countries under Jugoslavia (Bosnia and Herzegovina, Croatia, Macedonia (the former Yugoslav Republic of), Serbia Montenegro, Slovenia)
    country = replace(country, country == "Bosnia and Herzegovina", "Yugoslavia"),
    ISO = replace(ISO, ISO == "BIH", "YUG"), #Bosnia and Herzegovina
    country = replace(country, country == "Croatia", "Yugoslavia"),
    ISO = replace(ISO, ISO == "HRV", "YUG"), #Croatia
    country = replace(country, country == "Macedonia (the former Yugoslav Republic of)", "Yugoslavia"),
    ISO = replace(ISO, ISO == "MKD", "YUG"), #Macedonia (the former Yugoslav Republic of)
    country = replace(country, country == "Serbia Montenegro", "Yugoslavia"),
    ISO = replace(ISO, ISO == "SCG", "YUG"), #Serbia Montenegro
    country = replace(country, country == "Slovenia", "Yugoslavia"),
    ISO = replace(ISO, ISO == "SVN", "YUG"), #Slovenia
    
    # Czechoslovakia
    country = replace(country, country == "Czech Republic (the)", "Czechoslovakia"),
    ISO = replace(ISO, ISO == "CZE", "CSK"), #Czech Republic (the)
    country = replace(country, country == "Slovakia", "Czechoslovakia"),
    ISO = replace(ISO, ISO == "SVK", "CSK"), #Slovakia
    
    #Germany
    country = replace(country, country == "Germany Dem Rep", "Germany"),
    ISO = replace(ISO, ISO == "DDR", "DEU"), #Germany Dem Rep
    country = replace(country, country == "Germany Fed Rep", "Germany"),
    ISO = replace(ISO, ISO == "DFR", "DEU"), #Germany Fed Rep
    
    # Yemen
    country = replace(country, country == "Yemen Arab Rep", "Yemen"),
    ISO = replace(ISO, ISO == "YMN", "YEM"), #Yemen Arab Rep
    country = replace(country, country == "Yemen P Dem Rep", "Yemen"),
    ISO = replace(ISO, ISO == "YMD", "YEM"), #Yemen P Dem Rep
    
    #Palestine
    country = replace(country, country == "Palestine, State of", "Israel"),
    ISO = replace(ISO, ISO == "PSE", "ISR"), #Palestine, State of
    
    #Timor-Leste
    country = replace(country, country == "Timor-Leste", "Indonesia"),
    ISO = replace(ISO, ISO == "TLS", "IDN"), #Timor-Leste
    
    #Eritrea
    country = replace(country, country == "Eritrea", "Ethiopia"),
    ISO = replace(ISO, ISO == "ERI", "ETH") #Eritrea
  )

#### Rescale #### 
#Rescale data for more managable unit of measures

disasters <- disasters %>% 
  mutate(
    gdp_percapita = cgdpo/pop,
    damages_percapita = tot_damages/(pop*1000), # 1000 indicates that damages are now expressed in Million of Dollars, not thousands anymore
    damage_scale = tot_damages/1000,
    affected_scale = tot_affected/1000,
    deaths_pc = tot_deaths/pop,
    affected_pc = tot_affected/(pop*1000) #Thousand of affected on population in millions
  )  


#Divide damages by 1000, to get the coefficient in Billions of Dollars
disasters <- disasters %>% 
  mutate(
    cgdpo = cgdpo/1000
  )


#### Settings ####  

tau_flag <- 8 #if 1, use selected taus for tables (less computational intensive), otherwise from 0.5 to 0.99 with skips of 0.01

#Specify vector of quantiles to use for quantile regressions
if(tau_flag==1){
  tau_vec <- c(0.7, 0.8, 0.9, 0.95, 0.99)
}else{
  tau_vec <- seq(0.5,0.99, by=0.01)
}


r1s <- matrix(nrow = 23, ncol=length(tau_vec))
rownames(r1s) <- c(
  "base_damage_60",
  "base_deaths_60",
  "pop_damage_60",
  "gdp_deaths_60",
  "base_damage_70",
  "base_deaths_70",
  "pop_damage_70",
  "gdp_deaths_70",
  "base_damage_80",
  "base_deaths_80",
  "pop_damage_80",
  "gdp_deaths_80",
  "gdp_per_capita",
  "interaction_term",
  "damages_per_capita",
  "income",
  "koppen",
  "deaths_int_60",
  "deaths_int_70",
  "deaths_int_80",
  "deaths_gdpint_60",
  "deaths_gdpint_70",
  "deaths_gdpint_80"
)

colnames(r1s) <- tau_vec


#Flag for computation of bootstrapped standard error (computational intensive). Set 0 if want to suppress

flag_se <- 1

repl <- 1000


#### BASELINE 1960 ####



# # # # Since 1960 # # # # 

#Create data
baseline_data <- disasters %>% 
  filter(decade >= 1960) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale)

#Create x as index, for easy interpretation of constant term
baseline_data[,"index"] <-baseline_data$year-1960

# # S1, the baseline version (MB) # # 

# Economic Damages
rq(data = baseline_data, formula = damage_scale ~ index +
     cgdpo, 
   tau = tau_vec) -> s1_60_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s1_60_damage <- summary.rqs(s1_60_damage_reg, se="boot", R=repl)
  plot_s1_60_damage <- plot(summary_s1_60_damage, level = 0.95)
}

#linear
lm(data = baseline_data, formula = damage_scale ~ index +
     cgdpo) -> s1_60_damage_lin

# Null Model
rq(data = baseline_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s1_60_damage_null

#save r1 to final table
r1s["base_damage_60",]  <- 1 - (s1_60_damage_reg$rho/s1_60_damage_null$rho)







# Deaths
rq(data = baseline_data, formula = tot_deaths ~ index +
     pop, 
   tau = tau_vec) -> s1_60_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_s1_60_deaths <- summary.rqs(s1_60_deaths_reg, se="boot", R=repl)
  plot_s1_60_deaths <- plot(summary_s1_60_deaths, level = 0.95)
}


#linear
lm(data = baseline_data, formula = tot_deaths ~ index +
     pop) -> s1_60_deaths_lin

# Null Model
rq(data = baseline_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> s1_60_deaths_null


#save r1 to final table
r1s["base_deaths_60",] <- 1 - (s1_60_deaths_reg$rho/s1_60_deaths_null$rho)






# # S3, with pop and gdp (MB) # # 

# Economic Damages
rq(data = baseline_data, formula = damage_scale ~ index +
     cgdpo + pop, 
   tau = tau_vec) -> s3_60_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s3_60_damage <- summary.rqs(s3_60_damage_reg, se="boot", R=repl)
  plot_s3_60_damage <- plot(summary_s3_60_damage, level = 0.95)
}


#linear
lm(data = baseline_data, formula = damage_scale ~ index +
     cgdpo + pop) -> s3_60_damage_lin


# Null Model
rq(data = baseline_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s3_60_damage_null

#save r1 to final table
r1s["pop_damage_60",] <- 1 - (s3_60_damage_reg$rho/s3_60_damage_null$rho)



# Deaths
rq(data = baseline_data, formula = tot_deaths ~ index +
     pop + cgdpo, 
   tau = tau_vec) -> s3_60_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_s3_60_deaths <- summary.rqs(s3_60_deaths_reg, se="boot", R=repl)
  plot_s3_60_deaths <- plot(summary_s3_60_deaths, level = 0.95)
}


#linear
lm(data = baseline_data, formula = tot_deaths ~ index +
     pop + cgdpo) -> s3_60_deaths_lin


# Null Model
rq(data = baseline_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> s3_60_deaths_null

#save r1 to final table
r1s["gdp_deaths_60",] <- 1 - (s3_60_deaths_reg$rho/s3_60_deaths_null$rho)




#### BASELINE 1970 ####

# # # # Since 1970 # # # # 

#Create data
baseline_data <- disasters %>% 
  filter(decade >= 1970) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale)

#Create x as index, for easy interpretation of constant term
baseline_data[,"index"] <-baseline_data$year-1970

# # S1, the baseline version (MB) # # 

# Economic Damages
rq(data = baseline_data, formula = damage_scale ~ index +
     cgdpo, 
   tau = tau_vec) -> s1_70_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s1_70_damage <-  summary.rqs(s1_70_damage_reg, se="boot", R=repl)
  plot_s1_70_damage <- plot(summary_s1_70_damage, level = 0.95)
}


#linear
lm(data = baseline_data, formula = damage_scale ~ index +
     cgdpo) -> s1_70_damage_lin

# Null Model
rq(data = baseline_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s1_70_damage_null

#save r1 to final table
r1s["base_damage_70",] <- 1 - (s1_70_damage_reg$rho/s1_70_damage_null$rho)



# Deaths
rq(data = baseline_data, formula = tot_deaths ~ index +
     pop, 
   tau = tau_vec) -> s1_70_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_s1_70_deaths <-  summary.rqs(s1_70_deaths_reg, se="boot", R=repl)
  plot_s1_70_deaths <- plot(summary_s1_70_deaths, level = 0.95)
}


#linear
lm(data = baseline_data, formula = tot_deaths ~ index +
     pop) -> s1_70_deaths_lin

# Null Model
rq(data = baseline_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> s1_70_deaths_null

#save r1 to final table
r1s["base_deaths_70",]  <- 1 - (s1_70_deaths_reg$rho/s1_70_deaths_null$rho)






# # S3, with pop and gdp (MB) # # 

# Economic Damages
rq(data = baseline_data, formula = damage_scale ~ index +
     cgdpo + pop, 
   tau = tau_vec) -> s3_70_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s3_70_damage <-  summary.rqs(s3_70_damage_reg, se="boot", R=repl)
  plot_s3_70_damage <- plot(summary_s3_70_damage, level = 0.95)
}


#linear
lm(data = baseline_data, formula = damage_scale ~ index +
     cgdpo + pop) -> s3_70_damage_lin


# Null Model
rq(data = baseline_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s3_70_damage_null


#save r1 to final table
r1s["pop_damage_70",] <- 1 - (s3_70_damage_reg$rho/s3_70_damage_null$rho)



# Deaths
rq(data = baseline_data, formula = tot_deaths ~ index +
     pop + cgdpo, 
   tau = tau_vec) -> s3_70_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_s3_70_deaths <-  summary.rqs(s3_70_deaths_reg, se="boot", R=repl)
  plot_s3_70_deaths <- plot(summary_s3_70_deaths, level = 0.95)
}


#linear
lm(data = baseline_data, formula = tot_deaths ~ index +
     pop + cgdpo) -> s3_70_deaths_lin


# Null Model
rq(data = baseline_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> s3_70_deaths_null

#save r1 to final table
r1s["gdp_deaths_70",] <- 1 - (s3_70_deaths_reg$rho/s3_70_deaths_null$rho)




#### BASELINE 1980 ####

# # # # Since 1980 # # # # 

#Create data
baseline_data <- disasters %>% 
  filter(decade >= 1980) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale)

#Create x as index, for easy interpretation of constant term
baseline_data[,"index"] <-baseline_data$year-1980

# # S1, the baseline version (MB) # # 

# Economic Damages
rq(data = baseline_data, formula = damage_scale ~ index +
     cgdpo, 
   tau = tau_vec) -> s1_80_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s1_80_damage <-  summary.rqs(s1_80_damage_reg, se="boot", R=repl)
  plot_s1_80_damage <- plot(summary_s1_80_damage, level = 0.95)
}


#linear
lm(data = baseline_data, formula = damage_scale ~ index +
     cgdpo) -> s1_80_damage_lin

# Null Model
rq(data = baseline_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s1_80_damage_null

#save r1 to final table
r1s["base_damage_80",] <- 1 - (s1_80_damage_reg$rho/s1_80_damage_null$rho)



# Deaths
rq(data = baseline_data, formula = tot_deaths ~ index +
     pop, 
   tau = tau_vec) -> s1_80_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_s1_80_deaths <-  summary.rqs(s1_80_deaths_reg, se="boot", R=repl)
  plot_s1_80_deaths <- plot(summary_s1_80_deaths, level = 0.95)
}


#linear
lm(data = baseline_data, formula = tot_deaths ~ index +
     pop) -> s1_80_deaths_lin

# Null Model
rq(data = baseline_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> s1_80_deaths_null


#save r1 to final table
r1s["base_deaths_80",] <- 1 - (s1_80_deaths_reg$rho/s1_80_deaths_null$rho)



# # S3, with pop and gdp (MB) # # 

# Economic Damages
rq(data = baseline_data, formula = damage_scale ~ index +
     cgdpo + pop, 
   tau = tau_vec) -> s3_80_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s3_80_damage <-  summary.rqs(s3_80_damage_reg, se="boot", R=repl)
  plot_s3_80_damage <- plot(summary_s3_80_damage, level = 0.95)
}


#linear
lm(data = baseline_data, formula = damage_scale ~ index +
     cgdpo + pop) -> s3_80_damage_lin


# Null Model
rq(data = baseline_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s3_80_damage_null

#save r1 to final table
r1s["pop_damage_80",] <- 1 - (s3_80_damage_reg$rho/s3_80_damage_null$rho)





# Deaths
rq(data = baseline_data, formula = tot_deaths ~ index +
     pop + cgdpo, 
   tau = tau_vec) -> s3_80_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_s3_80_deaths <-  summary.rqs(s3_80_deaths_reg, se="boot", R=repl)
  plot_s3_80_deaths <- plot(summary_s3_80_deaths, level = 0.95)
}


#linear
lm(data = baseline_data, formula = tot_deaths ~ index +
     pop + cgdpo) -> s3_80_deaths_lin

# Null Model
rq(data = baseline_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> s3_80_deaths_null


#save r1 to final table
r1s["gdp_deaths_80",] <- 1 - (s3_80_deaths_reg$rho/s3_80_deaths_null$rho)




#### ONLY DAMAGES s4-s6 ####

#Create data
dam_data <- disasters %>% 
  filter(decade >= 1960) %>%                   
  select(year, cgdpo, pop, 
         gdp_percapita,
         damage_scale,
         damages_percapita)

#Small patch: rescale also gdp_percapita in order to have better betas

dam_data <- dam_data %>% mutate(
  gdp_percapita = gdp_percapita/1000
)

#Create x as index, for easy interpretation of constant term
dam_data[,"index"] <-dam_data$year-1960


# s4, gdp per capita, since 1960
rq(data = dam_data, formula = damage_scale ~ index +
     gdp_percapita, 
   tau = tau_vec) -> s4_60_reg
if(flag_se!=0){
  set.seed(134)
  summary_s4_60 <- summary.rqs(s4_60_reg, se="boot", R=repl)
  plot_s4_60 <- plot(summary_s4_60, level = 0.95)
}


#linear
lm(data = dam_data, formula = damage_scale ~ index +
     gdp_percapita) -> s4_60_lin


# Null Model
rq(data = dam_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s4_60_null

#save r1 to final table
r1s["gdp_per_capita",]  <- 1 - (s4_60_reg$rho/s4_60_null$rho)



# s5, interaction term, since 1960
rq(data = dam_data, formula = damage_scale ~ index +
     cgdpo + 
     (index*cgdpo), 
   tau = tau_vec) -> s5_60_reg
if(flag_se!=0){
  set.seed(134)
  summary_s5_60 <-  summary.rqs(s5_60_reg, se="boot", R=repl)
  plot_s5_60 <- plot(summary_s5_60, level = 0.95)
}


#linear
lm(data = dam_data, formula = damage_scale ~ index +
     cgdpo + (index*cgdpo) ) -> s5_60_lin

# Null Model
rq(data = dam_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s5_60_null

#save r1 to final table
r1s["interaction_term",] <- 1 - (s5_60_reg$rho/s5_60_null$rho)





# s6, normalized dependent variable, since 1960
rq(data = dam_data, formula = damages_percapita ~ index, 
   tau = tau_vec) -> s6_60_reg
if(flag_se!=0){
  set.seed(134)
  summary_s6_60 <-  summary.rqs(s6_60_reg, se="boot", R=repl)
  plot_s6_60 <- plot(summary_s6_60, level = 0.95)
}


#linear
lm(data = dam_data, formula = damages_percapita ~ index) -> s6_60_lin

# Null Model
rq(data = dam_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s6_60_null

#save r1 to final table
r1s["damages_per_capita",] <- 1 - (s6_60_reg$rho/s6_60_null$rho)





#### Income Level Dummy ####

income_data <- disasters %>% 
  filter(year >= 1960) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale,
         income_group)

#Create x as index, for easy interpretation of constant term
income_data[,"index"] <-income_data$year-1960

table(income_data$income_group) #numerosittycategories


#Generate dummy
income_data$low <- 0
income_data$low[income_data$income_group=="Low income"] <- 1
income_data$lower_mid <- 0
income_data$lower_mid[income_data$income_group=="Lower middle income"] <- 1
income_data$upper_mid <- 0
income_data$upper_mid[income_data$income_group=="Upper middle income"] <- 1
income_data$high <- 0
income_data$high[income_data$income_group=="High income"] <- 1


#Generate interaction terms

income_data[,"t_low"]         <- income_data$index*income_data$low
income_data[,"t_lower_mid"]   <- income_data$index*income_data$lower_mid
income_data[,"t_upper_mid"]   <- income_data$index*income_data$upper_mid
income_data[,"t_high"]        <- income_data$index*income_data$high

#Regressions (baseline is high)

# Economic Damages
rq(data = income_data, formula = damage_scale ~ index +
     cgdpo +
     low +
     lower_mid +
     upper_mid +
     t_low +
     t_lower_mid +
     t_upper_mid, 
   tau = tau_vec) -> s7_60_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s7_60_damage <-  summary.rqs(s7_60_damage_reg, se="boot", R=repl)
  plot_s7_60_damage <- plot(summary_s7_60_damage, level = 0.95)
}


#linear
lm(data = income_data, formula = damage_scale ~ index +
     cgdpo +
     low +
     lower_mid +
     upper_mid +
     t_low +
     t_lower_mid +
     t_upper_mid) -> s7_60_damage_lin


# Null Model
rq(data = income_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s7_60_damage_null

#save r1 to final table
r1s["income",] <- 1 - (s7_60_damage_reg$rho/s7_60_damage_null$rho)





#### Koppen Dummy ####

koppen_data <- disasters %>% 
  filter(year >= 1960) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale,
         income_group, koppen1)

#Create x as index, for easy interpretation of constant term
koppen_data[,"index"] <-koppen_data$year-1960

table(koppen_data$koppen1) #numerosità categorie dummy

#Generate dummy
koppen_data$tropical <- 0
koppen_data$tropical[koppen_data$koppen1=="Tropical"] <- 1
koppen_data$arid <- 0
koppen_data$arid[koppen_data$koppen1=="Arid"] <- 1
koppen_data$temperate <- 0
koppen_data$temperate[koppen_data$koppen1=="Temperate"] <- 1
koppen_data$cold <- 0
koppen_data$cold[koppen_data$koppen1=="Cold"] <- 1
koppen_data$polar <- 0
koppen_data$polar[koppen_data$koppen1=="Polar"] <- 1

#Generate interaction terms

koppen_data[,"t_tropical"]         <- koppen_data$index*koppen_data$tropical
koppen_data[,"t_arid"]             <- koppen_data$index*koppen_data$arid
koppen_data[,"t_temperate"]        <- koppen_data$index*koppen_data$temperate
koppen_data[,"t_cold"]             <- koppen_data$index*koppen_data$cold
koppen_data[,"t_polar"]            <- koppen_data$index*koppen_data$polar


#Regressions

# Economic Damages
rq(data = koppen_data, formula = damage_scale ~ index +
     cgdpo +
     arid +
     temperate +
     cold +
     polar +
     t_arid +
     t_temperate +
     t_cold +
     t_polar, 
   tau = tau_vec) -> s8_60_damage_reg
if(flag_se!=0){
  set.seed(134)
  summary_s8_60_damage <-  summary.rqs(s8_60_damage_reg, se="boot", R=repl)
  plot_s8_60_damage <- plot(summary_s8_60_damage, level = 0.95)
}


#linear
lm(data = koppen_data, formula = damage_scale ~ index +
     cgdpo +
     arid +
     temperate +
     cold +
     polar +
     t_arid +
     t_temperate +
     t_cold +
     t_polar) -> s8_60_damage_lin


# Null Model
rq(data = koppen_data, formula = damage_scale ~ 1, 
   tau = tau_vec) -> s8_60_damage_null


r1s["koppen",]  <- 1 - (s8_60_damage_reg$rho/s8_60_damage_null$rho)

#### Interaction Term Deaths ####

#### 1960 
int_data <- disasters %>% 
  filter(decade >= 1960) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale)

#Create x as index, for easy interpretation of constant term
int_data[,"index"] <-int_data$year-1960



# Deaths
rq(data = int_data, formula = tot_deaths ~ index +
     pop + (index*pop), 
   tau = tau_vec) -> int_60_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_int_60_deaths <- summary.rqs(int_60_deaths_reg, se="boot", R=repl)
  plot_int_60_deaths <- plot(summary_int_60_deaths, level = 0.95)
}


#linear
lm(data = int_data, formula = tot_deaths ~ index +
     pop + (index*pop)) -> int_60_deaths_lin


# Null Model
rq(data = int_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> int_60_deaths_null

#save r1 to final table
r1s["deaths_int_60",] <- 1 - (int_60_deaths_reg$rho/int_60_deaths_null$rho)




# Deaths GDP
rq(data = int_data, formula = tot_deaths ~ index +
     pop + (index*pop) + cgdpo, 
   tau = tau_vec) -> intgdp_60_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_intgdp_60_deaths <- summary.rqs(intgdp_60_deaths_reg, se="boot", R=repl)
  plot_intgdp_60_deaths <- plot(summary_intgdp_60_deaths, level = 0.95)
}


#linear
lm(data = int_data, formula = tot_deaths ~ index +
     pop + (index*pop) + cgdpo) -> intgdp_60_deaths_lin


# Null Model
rq(data = int_data, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> intgdp_60_deaths_null

#save r1 to final table
r1s["deaths_gdpint_60",] <- 1 - (intgdp_60_deaths_reg$rho/intgdp_60_deaths_null$rho)






#### 1970 
int_data_70 <- disasters %>% 
  filter(decade >= 1970) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale)

#Create x as index, for easy interpretation of constant term
int_data_70[,"index"] <-int_data_70$year-1970



# Deaths
rq(data = int_data_70, formula = tot_deaths ~ index +
     pop + (index*pop), 
   tau = tau_vec) -> int_70_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_int_70_deaths <- summary.rqs(int_70_deaths_reg, se="boot", R=repl)
  plot_int_70_deaths <- plot(summary_int_70_deaths, level = 0.95)
}


#linear
lm(data = int_data_70, formula = tot_deaths ~ index +
     pop + (index*pop)) -> int_70_deaths_lin


# Null Model
rq(data = int_data_70, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> int_70_deaths_null


#save r1 to final table
r1s["deaths_int_70",] <- 1 - (int_70_deaths_reg$rho/int_70_deaths_null$rho)




# Deaths GDP
rq(data = int_data_70, formula = tot_deaths ~ index +
     pop + (index*pop) + cgdpo, 
   tau = tau_vec) -> intgdp_70_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_intgdp_70_deaths <- summary.rqs(intgdp_70_deaths_reg, se="boot", R=repl)
  plot_intgdp_70_deaths <- plot(summary_intgdp_70_deaths, level = 0.95)
}


#linear
lm(data = int_data_70, formula = tot_deaths ~ index +
     pop + (index*pop) + cgdpo) -> intgdp_70_deaths_lin


# Null Model
rq(data = int_data_70, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> intgdp_70_deaths_null

#save r1 to final table
r1s["deaths_gdpint_70",] <- 1 - (intgdp_70_deaths_reg$rho/intgdp_70_deaths_null$rho)






#### 1980 
int_data_80 <- disasters %>% 
  filter(decade >= 1980) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale)

#Create x as index, for easy interpretation of constant term
int_data_80[,"index"] <-int_data_80$year-1980



# Deaths
rq(data = int_data_80, formula = tot_deaths ~ index +
     pop + (index*pop), 
   tau = tau_vec) -> int_80_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_int_80_deaths <- summary.rqs(int_80_deaths_reg, se="boot", R=repl)
  plot_int_80_deaths <- plot(summary_int_80_deaths, level = 0.95)
}


#linear
lm(data = int_data_80, formula = tot_deaths ~ index +
     pop + (index*pop)) -> int_80_deaths_lin


# Null Model
rq(data = int_data_80, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> int_80_deaths_null


r1s["deaths_int_80",] <- 1 - (int_80_deaths_reg$rho/int_80_deaths_null$rho)




# Deaths GDP
rq(data = int_data_80, formula = tot_deaths ~ index +
     pop + (index*pop) + cgdpo, 
   tau = tau_vec) -> intgdp_80_deaths_reg
if(flag_se!=0){
  set.seed(134)
  summary_intgdp_80_deaths <- summary.rqs(intgdp_80_deaths_reg, se="boot", R=repl)
  plot_intgdp_80_deaths <- plot(summary_intgdp_80_deaths, level = 0.95)
}


#linear
lm(data = int_data_80, formula = tot_deaths ~ index +
     pop + (index*pop) + cgdpo) -> intgdp_80_deaths_lin


# Null Model
rq(data = int_data_80, formula = tot_deaths ~ 1, 
   tau = tau_vec) -> intgdp_80_deaths_null

#save r1 to final table
r1s["deaths_gdpint_80",] <- 1 - (intgdp_80_deaths_reg$rho/intgdp_80_deaths_null$rho)