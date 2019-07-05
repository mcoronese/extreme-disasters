#CScript for charts related to human losses

#### Pacchetti ####
require("tidyverse")  # Ecosistema di manipolazione dati
library("lubridate")  # Manipolazione date/ore
library("stringr")    # Manipolazione stringhe
library("scales")     # Scale personalizzate grafico
library("cowplot")
library("reshape")
library("plotly")     # 3D graphs
library("moments")    # For skewness and kurtosis
library("pwt9")       # Penn World Tables
library("quantreg")   # Regressione quantilica
library("readr")
library("wesanderson") #palettes
library("ggmap")      # Spatial data management
library("grid")
library(gridExtra)


#load data
load("simul/deaths_int.RData")
load("simul/final_rescaled_complete.RData")
#load re-bootstrapped results analysis by climatic zones
load("simul/plot_distype_boot_1k.Rda")
load( "simul/plot_income_boot_1k.Rda")
load( "simul/plot_koppen_boot_1k.Rda")

theme_set(theme_minimal())

#### Palette ####

pal_income          <- c("High"   = "#ca0020",
                         "Upper Middle" = "#f4a582",
                         "Lower Middle" = "#92c5de",
                         "Low"       = "#0571b0")

pal_distype         <- c("Drought"   = "#a6611a",
                         "Extreme Temperature" = "#dfc27d",
                         "Flood" = "#80cdc1",
                         "Storm"       = "#018571")

pal_koppen          <- c("Temperate"  ="#7CAE00", 
                         "Cold"       ="#00BFC4", 
                         "Arid"       ="#F8766D", 
                         "Tropical"="#C77CFF") 

diverging_palette   <- c("2010"="#b2182b",
                         "2000"="#ef8a62",
                         "1990"="#fddbc7",
                         "1980"="#d1e5f0",
                         "1970"="#67a9cf",
                         "1960"="#2166ac")

deaths_palette      <- c("1960"="#8c510a",
                         "1970"="#d8b365",
                         "1980"="#f6e8c3",
                         "1990"="#c7eae5",
                         "2000"="#5ab4ac",
                         "2010"="#01665e")

#alphas 
alpha_density       <- c("2010"="0.05",
                         "2000"="0.13",
                         "1990"="0.23",
                         "1980"="0.4",
                         "1970"="0.7",
                         "1960"="1")

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

#### Function ####

#Human numbers: usa suffixes instead of huge numbers
human_numbers <- function(x = NULL, smbl ="", signif = 1){
    humanity <- function(y){
        
        if (!is.na(y)){
            tn <- round(abs(y) / 1e12, signif)
            b <- round(abs(y) / 1e9, signif)
            m <- round(abs(y) / 1e6, signif)
            k <- round(abs(y) / 1e3, signif)
            
            if ( y >= 0 ){
                y_is_positive <- ""
            } else {
                y_is_positive <- "-"
            }
            
            if ( k < 1 ) {
                paste0( y_is_positive, smbl, round(abs(y), signif ))
            } else if ( m < 1){
                paste0 (y_is_positive, smbl,  k , "k")
            } else if (b < 1){
                paste0 (y_is_positive, smbl, m ,"m")
            }else if(tn < 1){
                paste0 (y_is_positive, smbl, b ,"bn")
            } else {
                paste0 (y_is_positive, smbl,  comma(tn), "tn")
            }
        } else if (is.na(y) | is.null(y)){
            "-"
        }
    }
    
    sapply(x,humanity)
}

#To label facetd
graph_labeller <- function(variable,value){
    return(label_names[value])
}

#In case of two-dimensional facetting
plot_labeller <- function(variable,value){
    if (variable=='measure') {
        return(facet1_names[value])
    } else {
        return(facet2_names[value])
    }
}

#### Data manipulation Descriptive ####


#Generate data for densities
odisasters_decade_deaths <- odisasters %>%
    filter(decade >= 1960) %>%
    select(decade, tot_deaths) %>% 
    gather("measure", "value", -decade)

odisasters_decade_deaths$decade <- factor(odisasters_decade_deaths$decade, 
                                          levels = c("1960","1970","1980","1990","2000","2010"))

#For Boxplot
odisasters_year_deaths <- odisasters %>% 
    filter(year >= 1960) %>% 
    select(year, tot_deaths) %>% 
    gather("measure", "value", -year)  

disasters_boxplot_deaths <- odisasters_year_deaths %>% 
    filter(value>0) %>% 
    group_by(year) %>%
    summarise(
        q0  = min(value),
        q25 = quantile(value, probs= 0.25, na.rm = TRUE),
        q50 = quantile(value, probs= 0.50, na.rm = TRUE),
        q75 = quantile(value, probs= 0.75, na.rm = TRUE),
        q90 = quantile(value, probs= 0.90, na.rm = TRUE),
        q99 = quantile(value, probs= 0.99, na.rm = TRUE),
        q100 = max(value)
    ) %>% mutate(
        decade = case_when(
            year>=1960 & year<1970 ~ 1960,
            year>=1970 & year<1980 ~ 1970,
            year>=1980 & year<1990 ~ 1980,
            year>=1990 & year<2000 ~ 1990,
            year>=2000 & year<2010 ~ 2000,
            year>=2010 ~ 2010
        )
    )

disasters_boxplot_deaths$decade <- factor(disasters_boxplot_deaths$decade, 
                                          levels = c("2010","2000","1990","1980","1970","1960"))

#### Data Manipulation Quantiles ####

#Quantile reg graph
plot_s1_60_deaths["index",,1:31] %>% t() %>% as.data.frame() -> base_deaths_data
variable                    <- rep("trend", length(tau_vec))
tau                         <- tau_vec
model                       <- rep("baseline_deaths", length(tau))
base_deaths_data            <- cbind(base_deaths_data, tau, variable, model)

base_deaths_data$model      <- factor(base_deaths_data$model,
                                      levels = "baseline_deaths",
                                      labels = "Global Deaths")

base_deaths_data$variable   <- factor(base_deaths_data$variable,
                                      levels = "trend",
                                      labels = "Quantile Regression")

lin_deaths                  <- rep(s1_60_deaths_lin$coefficients["index"], nrow(base_deaths_data))
lin_lb_deaths               <- rep(as.numeric(confint(s1_60_deaths_lin, level = 0.95)["index",1]), nrow(base_deaths_data))
lin_ub_deaths               <- rep(as.numeric(confint(s1_60_deaths_lin, level = 0.95)["index",2]), nrow(base_deaths_data))

lin_deaths_data             <- cbind(base_deaths_data, lin_deaths, lin_lb_deaths, lin_ub_deaths)

lin_deaths_data$variable    <- factor(lin_deaths_data$variable,
                                      levels = "Quantile Regression",
                                      labels = "OLS")

#Quantile reg with interaction graph
tau_vec         <- seq(0.5,0.99, by=0.01)
plot_int_60_deaths["index",,1:50] %>% t() %>% as.data.frame() -> int_deaths_data
variable        <- rep("trend", length(tau_vec))
tau             <- tau_vec
model           <- rep("int_deaths", length(tau))
int_deaths_data <- cbind(int_deaths_data, tau, variable, model)

int_deaths_data$model       <- factor(int_deaths_data$model,
                                      levels = "int_deaths",
                                      labels = "Global Deaths")

int_deaths_data$variable    <- factor(int_deaths_data$variable,
                                      levels = "trend",
                                      labels = "Quantile Regression")

lin_int_deaths                  <- rep(int_60_deaths_lin$coefficients["index"], nrow(int_deaths_data))
lin_int_lb_deaths               <- rep(as.numeric(confint(int_60_deaths_lin, level = 0.95)["index",1]), nrow(int_deaths_data))
lin_int_ub_deaths               <- rep(as.numeric(confint(int_60_deaths_lin, level = 0.95)["index",2]), nrow(int_deaths_data))

lin_int_deaths_data             <- cbind(int_deaths_data, lin_int_deaths, lin_int_lb_deaths, lin_int_ub_deaths)

lin_int_deaths_data$variable    <- factor(lin_int_deaths_data$variable,
                                          levels = "Quantile Regression",
                                          labels = "OLS")

#### Data Manipulation Groups ####

# Quantiles by group
disasters_year_dis <- disasters %>% 
    filter(year >= 1960) %>% 
    select(year, deaths_pc, income_group, dis_type)

disasters_year_dis_quant <- disasters_year_dis %>% 
    group_by(year, income_group, dis_type) %>%
    summarise(
        q50 = quantile(deaths_pc, probs= 0.50, na.rm = TRUE),
        mean = mean(deaths_pc),
        max = max(deaths_pc),
        q99 = quantile(deaths_pc, probs= 0.99, na.rm = TRUE)
    ) %>%
    gather(quantiles, value, q50:q99)

#Fix factor levels
disasters_year_dis_quant$dis_type       <- factor(disasters_year_dis_quant$dis_type, 
                                                  levels = c("Drought","Extreme temperature","Flood", "Landslide", "Storm", "Wildfire"),
                                                  labels = c("Drought","Extreme Temp.","Flood", "Landslide", "Storm", "Wildfire"))
disasters_year_dis_quant$quantiles      <- factor(disasters_year_dis_quant$quantiles, 
                                                  levels = c("q50", "mean", "max", "q99"),
                                                  labels = c("q50", "mean", "max", "q99"))
disasters_year_dis_quant$income_group   <- factor(disasters_year_dis_quant$income_group, 
                                                  levels = c("High income", "Upper middle income", "Lower middle income", "Low income"),
                                                  labels = c("High", "Upper Middle", "Lower Middle", "Low"))

#### Graphs ####

# Graph Settings
min_tau   <- 0.80 #minimum tau displayed
line_size <- 1

theme_set(theme_light())

disasters_boxplot_deaths %>% ggplot(aes(x=year)) +
    geom_boxplot(aes(ymin = q0, lower = q25, middle = q50, upper = q75, ymax = q90, group=year, fill=decade), stat = "identity", alpha=.8, size=.3) +
    geom_point(aes(x=year, y=q90), size=.3) +
    geom_smooth(aes(x=year, y=q90), method = "loess", se=F, size=.6, linetype=2, color="red") +
    scale_x_continuous(breaks=c(1960,1970, 1980, 1990, 2000, 2010)) +
    scale_fill_manual(breaks=c(1960,1970, 1980, 1990, 2000, 2010), values=deaths_palette) +
    theme_light() +
    xlab("Year") +
    ylab("Deaths") +
    ggtitle("A") +
    guides(fill=guide_legend(title="Decade", nrow=1)) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        text=element_text(size=20),
        legend.key.size = unit(1.4, "cm")
    ) -> mainplot_box2

odisasters_decade_deaths %>% 
    filter(value>0) %>% 
    ggplot(aes(x = value, group=decade), color="black") +
    geom_density(aes(alpha=decade, fill=decade), color="black", alpha=.8, size=.4, linetype=3, show.legend = F) +
    scale_alpha_manual(values = alpha_density) +
    scale_fill_manual( values=deaths_palette) +
    coord_cartesian(xlim = c(1000,50000), ylim=c(0,0.15))+
    scale_x_log10() +
    xlab("Log Deaths") +
    ggtitle("Kernel Density Estimates by Decade,\nRight Tails") +
    theme(
        #legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification=c(1,0), legend.position=c(.99,.37),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size=17), 
        plot.title = element_text(size=17)
    )-> subplot_box2

#Quantile reg graph with interaction term
int_deaths_data%>% filter(tau>=min_tau) %>%  ggplot() + 
    geom_line(aes(x=tau, y=coefficients, color=variable), size=line_size) +
    geom_ribbon(data=lin_int_deaths_data[which(lin_int_deaths_data$tau>=min_tau),], 
                aes(x= tau, ymin=`lower bd`, ymax=`upper bd`), alpha=.2) + #confidence interval for baseline
    geom_line(data=lin_int_deaths_data[which(lin_int_deaths_data$tau>=min_tau),], #linear estimate baseline
              aes(x=tau, y=lin_int_deaths, color=variable), size=0.4) +
    geom_line(data=lin_int_deaths_data[which(lin_int_deaths_data$tau>=min_tau),], #lower bound linear baseline
              aes(x=tau, y=lin_int_lb_deaths, color=variable), linetype=3) +
    geom_line(data=lin_int_deaths_data[which(lin_int_deaths_data$tau>=min_tau),], #upper bound linear baseline
              aes(x=tau, y=lin_int_ub_deaths, color=variable), linetype=3) +
    scale_color_discrete(breaks = c("Quantile Regression", "OLS")) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    coord_cartesian(xlim = c(0.8085, 0.9815)) +
    xlab("Quantile") +
    ylab("Time Trend") +
    ggtitle("B") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        text=element_text(size=20),
        legend.key.size = unit(1.4, "cm")
    ) -> quantile_reg_graph_int2

mainplot_box2 + annotation_custom(ggplotGrob(subplot_box2), xmin = 1982, xmax=2014, ymin=700, ymax=2210) -> plot_deaths2
plot_deaths2

grid.arrange(plot_deaths2, quantile_reg_graph_int2, nrow=1) #save as 7.2X17.53 

#Quantile by group 
disasters_year_dis_quant %>% mutate(
    dis_type=case_when(
        dis_type=="Drought" ~ "Drought",
        dis_type=="Extreme Temp." ~ "Extreme Temperature",
        dis_type=="Flood" ~ "Flood",
        dis_type=="Landslide" ~ "Landslide",
        dis_type=="Storm" ~ "Storm",
        dis_type=="Wildfire" ~ "Wildfire"
    )
) %>% 
    filter(quantiles=="q99") %>% 
    filter(value>0) %>% 
    ggplot(aes(x=year, y=value)) +
    facet_wrap( ~ dis_type, scales="free_y", nrow = 2) +
    geom_point(aes(color=income_group), alpha=.5, size=.7) +
    geom_smooth(method = lm, aes(color=income_group), linetype="dashed", size=.8, se=F) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=9, face = "bold",  margin = margin(3, 0, 6, 0) ),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5, shape = "3"))) +
    ylab("Deaths per Million Inhabitants (Yearly 99th Quantile)") +
    xlab("Year") +
    scale_color_manual(values=pal_income) +
    scale_x_continuous(breaks=c(seq(1960,2010,20),2015)) + 
    scale_y_log10(labels=trans_format('log10',math_format(10^.x, format=force)))-> deaths_groups_99





