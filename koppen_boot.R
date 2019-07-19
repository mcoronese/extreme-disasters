#### Packages ####
require("tidyverse")  # Ecosistema di manipolazione dati
library("lubridate")  # Manipolazione date/ore
library("stringr")    # Manipolazione stringhe
library("scales")     # Scale personalizzate grafico
library("cowplot")
library("reshape")
library("moments")    # For skewness and kurtosis
library("pwt9")       # Penn World Tables
library("quantreg")   # Regressione quantilica
library("readr")
library("wesanderson") #palettes
library("ggmap")      # Spatial data management


#### Preliminaries ####
theme_set(theme_minimal()) # set graphs theme

disasters  <- read_csv("data/disasters_koppen.csv", n_max=10902) #load geolocalized database

#add continents data
continents <- read_csv("data/continents.csv")
continents <- plyr::rename(continents, c("alpha-3" = "ISO"))

#Fix small bugs
continents[250:260, ]   <- NA
continents[250:260, 3]  <- c("SUN" ,"AZO", "YUG", "SPI" ,"CSK", "DDR", "DFR", "YMN", "YMD", "SCG" , "ANT")
continents[250:260, 11] <- c("AS" ,"EU", "EU", "EU" ,"EU",   "EU", "EU", "AS", "AS", "EU", "AM")
continents[which(is.na(continents$continent)),]$continent <- "AM"
disasters <- merge(disasters, continents[ , c("ISO", "continent")], by.x = c("ISO"), by.y = c("ISO"))

disasters$continent <- factor(disasters$continent, 
                              levels = c("AF", "AM", "AS", "EU", "OC", "SA"),
                              labels = c("Africa", "North America", "Asia", "Europe", "Oceania", "South America"))

# Add income class
income_class <- read_delim("data/income_class.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
#Fix small bugs
income_class[267:292, 1] =  c("AIA", "ANT", "AZO", "COD", "COK", "CSK", "DDR", "DFR", "GLP", "GUF", "MSR", "MTQ", "NIU", "PSE", "REU", "ROU", "SCG", "SHN", "SPI", "SUN", "TKL", "TLS", "WLF", "YMD", "YMN", "YUG")
income_class[267:292, 2] =  c("High income", "High income", "High income", "Low income", "High income", "High income", "High income", "High income", "Upper middle income", "Upper middle income", "Upper middle income", "High income", "Upper middle income", "Upper middle income", "High income", "Upper middle income", "Upper middle income", "Lower middle income", "High income", "Upper middle income", "Low income", "Lower middle income", "Upper middle income", "Lower middle income", "Lower middle income", "Upper middle income" )

# merge two data frames by ID
disasters <- merge(disasters, income_class, by="ISO")

#### PWT ####
data("pwt9.0") #load Penn World Table
pwt9.0$country=as.character(pwt9.0$country)
pwt9.0$isocode=as.character(pwt9.0$isocode)

#German Federal Republic assigned to Germany
pwt9.0[pwt9.0$country=="Germany",] ->germany #65 observations
#Serbia-Montenegro to Serbia
pwt9.0[pwt9.0$country=="Serbia",]  -> serbia #65 observations
#Yemens to Yemen
pwt9.0[pwt9.0$country=="Yemen",]   -> yemen #65 observations

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

# Which countries are present in EMDAT and not in PWT (res)
pwiso           <- pwt9.0$isocode %>% unique()
disiso          <- disasters$ISO %>% unique()
intersection    <- intersect(pwiso,disiso)
res             <- disiso[is.na(pmatch(disiso,intersection))]

sum(disasters$ISO %in% res) # Number of observations uncovered by PWT
pwt_missing     <- disasters[disasters$ISO %in% res,]
table(pwt_missing$country)
remove(disiso, pwiso, intersection, res)

pwt9.0          <- plyr::rename(pwt9.0, c("isocode" ="ISO"))
odisasters      <- disasters #original datasets with all observations
disasters       <- merge(disasters, pwt9.0[ , c("year", "ISO", "cgdpo", "ck", "pop")], by.x = c("ISO", "year"), by.y = c("ISO", "year"))

#Fixing typo in PWT (negative GDP)
disasters$cgdpo[disasters$index==1107] = -disasters$cgdpo[disasters$index==1107]

#Constuct normalized measures
disasters <- disasters %>% 
    mutate(
        damages_gdp = tot_damages/cgdpo,
        damages_capital = tot_damages/ck,
        damages_gdp_pc = tot_damages/(cgdpo/pop),
        damages_capital_pc = tot_damages/(ck/pop),
        deaths_pc = tot_deaths/pop,
        affected_pc = tot_affected/pop
    )    

#### ISO armonization ####
#Make countries with changing national boundaries comparable over time

#Trace the "old" state, adding it to location
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

#Change country names and ISOs
disasters <- disasters %>%
    mutate(
        # Group Ex Soviet Union Countries (Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Moldova (the Republic of), Russian Federation (the), Tajikistan, Turkmenistan, Ukraine, Uzbekistan) sotto lo stesso stato Soviet Union
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
        
        # Cecoslovacchia
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


disasters <- disasters %>% 
  mutate(
    gdp_percapita = cgdpo/pop,
    damages_percapita = tot_damages/(pop*1000), #million of dollars
    damage_scale = tot_damages/1000,
    affected_scale = tot_affected/1000,
    deaths_pc = tot_deaths/pop,
    affected_pc = tot_affected/(pop*1000)
  )  

#GDP in billions of dollars, for a more readable beta
disasters <- disasters %>% 
  mutate(
    cgdpo = cgdpo/1000
  )

#### Setup ####


#Specify vector of quantiles to use for quantile regressions
tau_vec<-c(0.50, 0.70, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79, 0.80, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99)


#set bootstrap replications
repl <- 1000

#### Koppen ####
koppen_data <- disasters %>% 
  filter(year >= 1960) %>%                   
  select(year, cgdpo, pop,
         damage_scale, 
         tot_deaths, 
         affected_scale,
         income_group, koppen1)

#Create index for a better interpretation of the constant term
koppen_data[,"index"] <-koppen_data$year-1960

#Genero dummy
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


#### Regressions ####
koppen_data<-koppen_data %>% filter(
  is.na(cgdpo)==0
)

#create design matrix 
koppen_data %>% select(
  index, cgdpo, arid, temperate, cold, polar, t_arid, t_temperate, t_cold, t_polar
) -> design_matrix
design_matrix <- add_column(design_matrix, const=1, .before = 1)
response <- koppen_data$damage_scale

#fit qr
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
   tau = tau_vec) -> koppen_qr

set.seed(134)
summary_koppen <-  summary.rqs(koppen_qr, se="boot", R=repl)

#### Check ####
##Check results are the same as with the koenker package

se_boot_check <- matrix(0, nrow=length(tau_vec), ncol = ncol(design_matrix))
colnames(se_boot_check) <- c("const",
                             "index",
                             "cgdpo",
                             "arid",
                             "temperate",
                             "cold",
                             "polar",
                             "t_arid",
                             "t_temperate",
                             "t_cold",
                             "t_polar" )

i=1
set.seed(134) #put it outside the loop for full comparability with koenker package
for(i in 1:length(tau_vec)){
  boot.rq(design_matrix, y= response, tau=tau_vec[i], R=repl) -> booted_check
  colnames(booted_check$B) <- c( "const",
                           "index",
                           "cgdpo",
                           "arid",
                           "temperate",
                           "cold",
                           "polar",
                           "t_arid",
                           "t_temperate",
                           "t_cold",
                           "t_polar" )
  j=1
  for (j in 1:ncol(se_boot_check)) {
    se_boot_check[i,j] <- sqrt(sum((booted_check$B[,j] - mean(booted_check$B[,j]))^2)/(repl-1))
  }
  
}

for(i in 1:length(summary_koppen)){
    print(all(round(summary_koppen[[i]]$coefficients[,"Std. Error"],9)==round(se_boot_check[i,],9)))
}
#it's the same with a precision of 9 digits

##### Bootstrapped regressions ######

se_boot <- matrix(0, nrow=length(tau_vec), ncol = ncol(design_matrix))
colnames(se_boot) <- c(  "const",
                         "index",
                         "cgdpo",
                         "arid",
                         "temperate",
                         "cold",
                         "polar",
                         "t_arid",
                         "t_temperate",
                         "t_cold",
                         "t_polar" )
i=1
set.seed(134) #put it outside the loop for full comparability with koenker package
for(i in 1:length(tau_vec)){
  boot.rq(design_matrix, y= response, tau=tau_vec[i], R=repl) -> booted
  colnames(booted$B) <- c( "const",
                           "index",
                           "cgdpo",
                           "arid",
                           "temperate",
                           "cold",
                           "polar",
                           "t_arid",
                           "t_temperate",
                           "t_cold",
                           "t_polar" )
  booted$B[,"arid"] <- booted$B[,"arid"] + booted$B[,"const"]
  booted$B[,"temperate"] <- booted$B[,"temperate"] + booted$B[,"const"]
  booted$B[,"cold"] <- booted$B[,"cold"] + booted$B[,"const"]
  booted$B[,"polar"] <- booted$B[,"polar"] + booted$B[,"const"]
  booted$B[,"t_arid"] <- booted$B[,"t_arid"] + booted$B[,"index"]
  booted$B[,"t_temperate"] <- booted$B[,"t_temperate"] + booted$B[,"index"]
  booted$B[,"t_cold"] <- booted$B[,"t_cold"] + booted$B[,"index"]
  booted$B[,"t_polar"] <- booted$B[,"t_polar"] + booted$B[,"index"]
  j=1
  for (j in 1:ncol(se_boot)) {
    se_boot[i,j] <- sqrt(sum((booted$B[,j] - mean(booted$B[,j]))^2)/(repl-1))
  }
  
}


##### create object for plotting (other script) ####
plot_koppen_boot <- array(0, c(11, 3, length(tau_vec)))
dimnames(plot_koppen_boot)[[2]] <- c("coefficients", "lower bd", "upper bd")
dimnames(plot_koppen_boot)[[1]] <- c( "const",
                                      "index",
                                      "cgdpo",
                                      "arid",
                                      "temperate",
                                      "cold",
                                      "polar",
                                      "t_arid",
                                      "t_temperate",
                                      "t_cold",
                                      "t_polar" )

plot(summary_koppen, level=0.95) -> plot_koppen

plot_koppen_boot["const",1,] <- plot_koppen["(Intercept)",1,1:31]
plot_koppen_boot["index",1,] <- plot_koppen["index",1,1:31]
plot_koppen_boot["cgdpo",1,] <- plot_koppen["cgdpo",1,1:31]
plot_koppen_boot["arid",1,] <- plot_koppen["arid",1,1:31] + plot_koppen["(Intercept)",1,1:31]
plot_koppen_boot["temperate",1,] <- plot_koppen["temperate",1,1:31] + plot_koppen["(Intercept)",1,1:31]
plot_koppen_boot["cold",1,] <- plot_koppen["cold",1,1:31] + plot_koppen["(Intercept)",1,1:31]
plot_koppen_boot["polar",1,] <- plot_koppen["polar",1,1:31] + plot_koppen["(Intercept)",1,1:31]
plot_koppen_boot["t_arid",1,] <- plot_koppen["t_arid",1,1:31] + plot_koppen["index",1,1:31]
plot_koppen_boot["t_temperate",1,] <- plot_koppen["t_temperate",1,1:31] + plot_koppen["index",1,1:31]
plot_koppen_boot["t_cold",1,] <- plot_koppen["t_cold",1,1:31] + plot_koppen["index",1,1:31]
plot_koppen_boot["t_polar",1,] <- plot_koppen["t_polar",1,1:31] + plot_koppen["index",1,1:31]


i=1
for (i in 1:11) {
  plot_koppen_boot[i,"lower bd",] = plot_koppen_boot[i,1,] - (se_boot[,i]*1.96)
  plot_koppen_boot[i,"upper bd",] = plot_koppen_boot[i,1,] + (se_boot[,i]*1.96)
}


#### Prepare Object for plotting

#prepare table for printing (selected quantiles)
plot_koppen_boot[c("index"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> index
plot_koppen_boot[c("t_arid"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> t_arid
plot_koppen_boot[c("t_temperate"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> t_temperate
plot_koppen_boot[c("t_cold"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> t_cold
plot_koppen_boot[c("t_polar"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> t_polar
plot_koppen_boot[c("cgdpo"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> cgdpo

variable <- c(rep("tropical_base", length(tau_vec)), rep("t_arid", length(tau_vec)),rep("t_temperate", length(tau_vec)),rep("t_cold", length(tau_vec)),rep("t_polar", length(tau_vec)),rep("cgdpo", length(tau_vec)) )
tau <- rep(tau_vec, 6)
model <- rep("climatic", length(tau))
index$type       <- factor(1)
t_arid$type       <- factor(0)
t_temperate$type <- factor(1)
t_cold$type <- factor(0)
t_polar$type <- factor(0)
cgdpo$type <- factor(0)


climatic_data <- rbind(index, t_arid, t_temperate, t_cold, t_polar, cgdpo)
climatic_data <- cbind(climatic_data, tau, variable, model)

climatic_data %>% dplyr::mutate(
    std_err = (coefficients - `lower bd`)/1.96
)  %>% mutate(
    t_value = coefficients/std_err,
    p_value = 2*pt(abs(t_value),summary_koppen[[1]]$rdf, lower.tail = F)
)  %>% filter(
    tau==0.7|
    tau==0.8|
    tau==0.9|
    tau==0.95|
    tau==0.99
) -> print_koppen

var_list <- as.character(unique(print_koppen$variable))

#write funtion for getting a vector of p values
p_stars <- function(x){
    y <- x
    for(i in 1:length(x)){
        if(x[i]<0.01){
            y[i]<-"***"
        }else{
            if(x[i]<0.05){
                y[i]<-"**"
            }else{
                if(x[i]<0.1){
                    y[i]<-"*"
                }else{
                    y[i]<-""
                }
            }
        }
    }
    return(y)
}


#compute p-values and create final table
for(j in 1:length(var_list)){
    coef <- print_koppen %>% filter(variable==var_list[j]) %>% 
        select(coefficients,std_err,p_value) %>% mutate(
        coefficients = paste0(round(coefficients,3), p_stars(p_value)),
        std_err = round(std_err,3)
    ) %>% select(coefficients, std_err) 
    
    coef <- as.matrix(coef)
    line <- c(t(coef))
    hline <- matrix(line,nrow = 2)
    
    if(j==1){
        final_table <- hline
    }else{
        final_table <- rbind(final_table, hline)
    }
}
rownames(final_table) <- c(rbind(var_list, rep("",length(var_list))))
colnames(final_table) <- c("70th", "80th", "90th", "95th", "99th")
rownames(final_table)[1] <- "t_tropical"

#### OLS ####

lm(data = koppen_data, formula = damage_scale ~ index +
       cgdpo +
       arid +
       temperate +
       cold +
       polar +
       t_arid +
       t_temperate +
       t_cold +
       t_polar) -> koppen_lin

summary_koppen_lin <-  summary(koppen_lin)

library(boot)
# function to obtain regression weights 
bs <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample 
    fit <- lm(formula, data=d)
    return(coef(fit)) 
} 

# bootstrapping with 1000 replications 
set.seed(134)
boot(data=koppen_data, statistic = bs, R=1000, sim = "ordinary", formula = damage_scale ~ index +
         cgdpo +
         arid +
         temperate +
         cold +
         polar +
         t_arid +
         t_temperate +
         t_cold +
         t_polar) -> ols_boot

#check coefficients are the same as with lm
round(cbind(ols_boot$t0, koppen_lin$coefficients),3) #they are the same

# compute standard errors
table_boot <- round(cbind(koppen_lin$coefficients,apply(ols_boot$t,2,sd),summary_koppen_lin$coefficients[,"Std. Error"]),3)
colnames(table_boot) <- c("coefficients", "boot_se", "se")

#compute p values
t_values <- table_boot[,"coefficients"]/table_boot[,"se"]
p_values <- 2*pt(abs(t_values),summary_koppen[[1]]$rdf, lower.tail = F)
boot_t_values <- table_boot[,"coefficients"]/table_boot[,"boot_se"]
boot_p_values <- 2*pt(abs(boot_t_values),summary_koppen[[1]]$rdf, lower.tail = F)

#round(cbind(p_values,boot_p_values),2) 

#now sum the bootstrapped estimates
boot_summed <- ols_boot
colnames(boot_summed$t) <- c("intercept",
                             "index",
                             "cgdpo",
                             "arid",
                             "temperate",
                             "cold",
                             "polar",
                             "t_arid",
                             "t_temperate",
                             "t_cold",
                             "t_polar")

boot_summed$t[,"arid"]        <- boot_summed$t[,"arid"]        + boot_summed$t[,"intercept"]
boot_summed$t[,"temperate"]   <- boot_summed$t[,"temperate"]   + boot_summed$t[,"intercept"]
boot_summed$t[,"cold"]        <- boot_summed$t[,"cold"]        + boot_summed$t[,"intercept"]
boot_summed$t[,"polar"]       <- boot_summed$t[,"polar"]       + boot_summed$t[,"intercept"]
boot_summed$t[,"t_arid"]      <- boot_summed$t[,"t_arid"]      + boot_summed$t[,"index"]
boot_summed$t[,"t_temperate"] <- boot_summed$t[,"t_temperate"] + boot_summed$t[,"index"]
boot_summed$t[,"t_cold"]      <- boot_summed$t[,"t_cold"]      + boot_summed$t[,"index"]
boot_summed$t[,"t_polar"]     <- boot_summed$t[,"t_polar"]     + boot_summed$t[,"index"]

koppen_sum_coef <- summary_koppen_lin$coefficients[,"Estimate"]

koppen_sum_coef["arid"]        <- koppen_sum_coef["arid"]        + koppen_sum_coef["(Intercept)"]
koppen_sum_coef["temperate"]   <- koppen_sum_coef["temperate"]   + koppen_sum_coef["(Intercept)"]
koppen_sum_coef["cold"]        <- koppen_sum_coef["cold"]        + koppen_sum_coef["(Intercept)"]
koppen_sum_coef["polar"]       <- koppen_sum_coef["polar"]       + koppen_sum_coef["(Intercept)"]
koppen_sum_coef["t_arid"]      <- koppen_sum_coef["t_arid"]      + koppen_sum_coef["index"]
koppen_sum_coef["t_temperate"] <- koppen_sum_coef["t_temperate"] + koppen_sum_coef["index"]
koppen_sum_coef["t_cold"]      <- koppen_sum_coef["t_cold"]      + koppen_sum_coef["index"]
koppen_sum_coef["t_polar"]     <- koppen_sum_coef["t_polar"]     + koppen_sum_coef["index"]


#compute standard error
table_boot_sum <- round(cbind(koppen_sum_coef,apply(boot_summed$t,2,sd)),3)
colnames(table_boot_sum) <- c("coefficients", "boot_se")
#compute p-values and prepare table
t_values_sum <- table_boot_sum[,"coefficients"]/table_boot_sum[,"boot_se"]
p_values_sum <- 2*pt(abs(t_values_sum),summary_koppen[[1]]$rdf, lower.tail = F)
table_boot_sum[,"coefficients"] <- paste0(table_boot_sum[,"coefficients"], p_stars(p_values_sum))

#add ols to final table
OLS <- rbind(table_boot_sum["index",],table_boot_sum["t_arid",],table_boot_sum["t_temperate",],
      table_boot_sum["t_cold",],table_boot_sum["t_polar",],table_boot_sum["cgdpo",])
OLS<-c(t(OLS))
final_table <- cbind(final_table, OLS)

#add fit quality
load("simul/final/estimation_tidy_correct.RData")
fit_quality <- round(c(r1s["koppen",], summary_koppen_lin$adj.r.squared),3)
final_table <- rbind(final_table, fit_quality)


#plot_koppen_boot["t_temperate",,] #dall'ottantwsimo in poi significatico al 95%
#plot_koppen_boot["index",,] #dal 70 in poi, solo non significativto da 89 a 95, ma significativo al 10%.
#plot_koppen_boot["t_cold",,] #mai dal 70 in poi, solo dal 90 al 95. 
#plot_koppen_boot["t_arid",,] #dal 70 al 75, dal8 7 all'89. 
#