#Script for charts related to economic damages

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
library("grid")       # Inset graphs 
library("gridExtra")

#load data global
load("simul/final_rescaled_complete.RData")
#load re-bootstrapped results analysis by climatic zones
load("simul/plot_koppen_boot_1k.Rda")

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


#### Palettes ####

diverging_palette   <- c("2010"="#b2182b",
                         "2000"="#ef8a62",
                         "1990"="#fddbc7",
                         "1980"="#d1e5f0",
                         "1970"="#67a9cf",
                         "1960"="#2166ac")

#alphas 
alpha_density       <- c("2010"="0.05",
                         "2000"="0.13",
                         "1990"="0.23",
                         "1980"="0.4",
                         "1970"="0.7",
                         "1960"="1")

pal_income          <- c("High"         = "#ca0020",
                         "Upper Middle" = "#f4a582",
                         "Lower Middle" = "#92c5de",
                         "Low"          = "#0571b0")

pal_distype         <- c("Drought"            = "#a6611a",
                         "Extreme Temperature" = "#dfc27d",
                         "Flood"               = "#80cdc1",
                         "Storm"               = "#018571")

pal_koppen          <- c("Temperate"  ="#7CAE00", 
                         "Cold"       ="#00BFC4", 
                         "Arid"       ="#F8766D",   
                         "Tropical"   ="#C77CFF") 

#### Data Manipulation Quantile Regression ####

plot_s1_60_damage["index",,1:31] %>% t() %>% as.data.frame() -> base_data
variable    <- rep("trend", length(tau_vec))
tau         <- tau_vec
model       <- rep("baseline", length(tau))
base_data   <- cbind(base_data, tau, variable, model)

base_data$model     <- factor(base_data$model,
                              levels = "baseline",
                              labels = "Global")

base_data$variable  <- factor(base_data$variable,
                              levels = "trend",
                              labels = "Quantile Regression")

# Climatic zones data
plot_koppen_boot[c("index"),,1:length(tau_vec)] %>% t() %>% as.data.frame()       -> index
plot_koppen_boot[c("t_arid"),,1:length(tau_vec)] %>% t() %>% as.data.frame()      -> t_arid
plot_koppen_boot[c("t_temperate"),,1:length(tau_vec)] %>% t() %>% as.data.frame() -> t_temperate
plot_koppen_boot[c("t_cold"),,1:length(tau_vec)] %>% t() %>% as.data.frame()      -> t_cold

variable <- c(rep("tropical_base", length(tau_vec)), rep("t_arid", length(tau_vec)),rep("t_temperate", length(tau_vec)),rep("t_cold", length(tau_vec)) )
tau      <- rep(tau_vec, 4)
model    <- rep("climatic", length(tau))


index$type        <- factor(1)
t_arid$type       <- factor(0)
t_temperate$type  <- factor(1)
t_cold$type       <- factor(0)


climatic_data <- rbind(index, t_arid, t_temperate, t_cold)
climatic_data <- cbind(climatic_data, tau, variable, model)

climatic_data$model <- factor(climatic_data$model,
                              levels = "climatic",
                              labels = "Koppen-Geiger Zone")

climatic_data$variable <- factor(climatic_data$variable,
                                 levels = c("tropical_base","t_temperate", "t_cold", "t_arid"),
                                 labels = c("Tropical","Temperate", "Cold", "Arid"))

#separate geoms data for baseline

lin                 <- rep(s1_60_damage_lin$coefficients["index"], nrow(base_data))
lin_lb              <- rep(as.numeric(confint(s1_60_damage_lin, level = 0.95)["index",1]), nrow(base_data))
lin_ub              <- rep(as.numeric(confint(s1_60_damage_lin, level = 0.95)["index",2]), nrow(base_data))
lin_data            <- cbind(base_data, lin, lin_lb, lin_ub)
lin_data$variable   <- factor(lin_data$variable,
                              levels = "Quantile Regression",
                              labels = "OLS")

#### Data manipulation descriptive ####

theme_set(theme_light())

#Generate data for densities
odisasters_decade_damages <- odisasters %>%
    filter(decade >= 1960) %>%                   # Selezionare sottoinsieme osservazioni
    select(decade, tot_damages) %>% 
    mutate(
        tot_damages = tot_damages/1000000
    ) %>% 
    gather("measure", "value", -decade)

odisasters_decade_damages$decade <- factor(odisasters_decade_damages$decade, 
                                           levels = c("2010","2000","1990","1980","1970","1960"))

#For Boxplots

odisasters_year_damages <- odisasters %>% 
    filter(year >= 1960) %>% 
    select(year, tot_damages) %>% 
    mutate(
        tot_damages = tot_damages/1000000
    ) %>% 
    gather("measure", "value", -year)  

disasters_boxplot_damages <- odisasters_year_damages %>% 
    filter(value>0) %>% 
    group_by(year) %>%  # Raggruppa per anno
    summarise(
        q0  = min(value),
        q50 = quantile(value, probs= 0.50, na.rm = TRUE),
        q75 = quantile(value, probs= 0.75, na.rm = TRUE),
        q80 = quantile(value, probs= 0.80, na.rm = TRUE),
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

disasters_boxplot_damages$decade <- factor(disasters_boxplot_damages$decade, 
                                           levels = c("2010","2000","1990","1980","1970","1960"))


#### Graphs ####

#settings
theme_set(theme_minimal())
min_tau = 0.80 #minimum tau displayed
line_size = 1

#climatic ribbon data
climatic_ribbon <- climatic_data %>% 
    filter(tau>=min_tau) %>% 
    filter(type==1)


climatic_glued <- climatic_data %>% 
    filter(tau>=min_tau) %>% 
    mutate(`lower bd`= case_when(
        type==0 ~ coefficients,
        type==1 ~ `lower bd`
    ),
    `upper bd`= case_when(
        type==0 ~ coefficients,
        type==1 ~ `upper bd`
    )
    )

disasters_boxplot_damages %>% ggplot(aes(x=year)) +
    geom_boxplot(aes(ymin = q0, lower = q50, middle = q75, upper = q90, ymax = q99, group=year, fill=decade), stat = "identity", alpha=.8, size=.3) +
    geom_point(aes(x=year, y=q99), size=.3) +
    geom_smooth(aes(x=year, y=q99), method = "loess", se=F, size=.6, linetype=2, color="red") +
    scale_x_continuous(breaks=c(1960,1970, 1980, 1990, 2000, 2010)) +
    scale_fill_manual(breaks=c(1960,1970, 1980, 1990, 2000, 2010), values=diverging_palette) +
    theme_light() +
    xlab("Year") +
    ylab("Economic Damages (Billions US$)") +
    ggtitle("A") +
    guides(fill=guide_legend(title="Decade", nrow=1)) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        text=element_text(size=20),
        legend.key.size = unit(1.4, "cm")
    ) -> mainplot_box_damages

odisasters_decade_damages %>% 
    filter(value>0) %>% 
    ggplot(aes(x = value, group=decade), color="black") +
    geom_density(aes(alpha=decade, fill=decade), color="black", alpha=.8, size=.4, linetype=3, show.legend = F) +
    scale_alpha_manual(values = alpha_density) +
    scale_fill_manual( values=diverging_palette) +
    coord_cartesian(xlim = c(10,111.5), ylim=c(0,0.02))+
    scale_x_log10() +
    xlab("Log Economic Damages (Billions US$)") +
    ggtitle("Kernel Density Estimates by Decade, Right Tails") +
    theme(
        legend.title = element_blank(),
        legend.justification=c(1,0), legend.position=c(.99,.37),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size=17), 
        plot.title = element_text(size=17)
    )-> subplot_box_damages

#together
mainplot_box_damages + annotation_custom(ggplotGrob(subplot_box_damages), xmin = 1960, xmax=1988, ymin=9, ymax=21) -> plot_damages

base_data%>% filter(tau>=min_tau) %>%  ggplot() + 
    geom_line(aes(x=tau, y=coefficients, color=variable), size=line_size) +
    facet_wrap(~model, scales = "free_y") +
    geom_ribbon(data=lin_data[which(lin_data$tau>=min_tau),], 
                aes(x= tau, ymin=`lower bd`, ymax=`upper bd`), alpha=.2) + #confidence interval for baseline
    geom_line(data=lin_data[which(lin_data$tau>=min_tau),], #linear estimate baseline
              aes(x=tau, y=lin, color=variable), size=0.4) +
    geom_line(data=lin_data[which(lin_data$tau>=min_tau),], #lower bound linear baseline
              aes(x=tau, y=lin_lb, color=variable), linetype=3) +
    geom_line(data=lin_data[which(lin_data$tau>=min_tau),], #upper bound linear baseline
              aes(x=tau, y=lin_ub, color=variable), linetype=3) +
    scale_y_continuous(breaks = c(0,10,20,30,40), limits = c(-4.4,42)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    coord_cartesian(xlim = c(0.8085, 0.9815)) +
    scale_color_discrete(breaks = c("Quantile Regression", "OLS"), labels = c("Quantile Regression", "Ordinary Least Squares")) +
    xlab("Quantile") +
    ylab("Time Trend (Millions US$ per year)") +
    ggtitle("B") +
    theme_light() +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        text=element_text(size=20), 
        legend.key.size = unit(1.4, "cm")
    ) ->global_quantile

damages_graph <- grid.arrange(plot_damages, global_quantile, nrow=1) #save it as a 3/2 half A4, that is 6.2 x 17.53. Actually, 7.2X17.53 makes inset looks nicer


climatic_data %>% filter(tau>=min_tau) %>% ggplot() + 
    geom_line(aes(x=tau, y=coefficients, color=variable, linetype =type), size=line_size) +
    geom_ribbon(data= climatic_ribbon, aes(x=tau, ymin=`lower bd`, ymax= `upper bd`, fill=variable), show.legend = F, alpha=.3) +
    facet_wrap(~model, scales = "free_y")+
    scale_color_manual(values = pal_koppen) +
    scale_fill_manual(values = pal_koppen) +
    scale_y_continuous(breaks = c(0,20,40,60,80), limits = c(-8.5,84)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    coord_cartesian(xlim = c(0.8085, 0.9815)) +
    xlab("Quantile") +
    ylab("Time Trend (Millions US$ per year)") +
    guides(linetype=FALSE) +
    theme_light()+
    theme(
        legend.title = element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.15,0.78),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        text=element_text(size=14)
    ) -> climatic

climatic_data %>% filter(tau>=min_tau) %>% ggplot() + 
    geom_line(aes(x=tau, y=coefficients, color=variable, linetype =type), size=line_size*0.6) +
    geom_ribbon(data= climatic_ribbon, aes(x=tau, ymin=`lower bd`, ymax= `upper bd`, fill=variable), show.legend = F, alpha=.3) +
    facet_wrap(~model, scales = "free_y")+
    scale_color_manual(values = pal_koppen) +
    scale_fill_manual(values = pal_koppen) +
    scale_y_continuous(breaks = c(0,20,40,60,80), limits = c(-8.5,84)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    coord_cartesian(xlim = c(0.8085, 0.9815)) +
    xlab("Quantile") +
    ylab("Time Trend (Millions US$ per year)") +
    guides(linetype=FALSE) +
    theme_light(base_size = 7)+
    theme(
        legend.title = element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.15,0.78),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        text=element_text(size=7.5),
        legend.key.size = unit(0.8,"line")
    ) -> climatic_real

climatic_glued %>% filter(tau>=min_tau) %>% ggplot() + 
    geom_line(aes(x=tau, y=coefficients, color=variable, linetype =type), size=line_size) +
    geom_ribbon(aes(x=tau, ymin=`lower bd`, ymax= `upper bd`, fill=variable), alpha=.3) +
    facet_wrap(~model, scales = "free_y")+
    scale_color_manual(values = pal_koppen) +
    scale_fill_manual(values = pal_koppen) +
    scale_y_continuous(breaks = c(0,20,40,60,80), limits = c(-8.5,84)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    coord_cartesian(xlim = c(0.8085, 0.9815)) +
    xlab("Quantile") +
    ylab("Time Trend (Millions US$ per year)") +
    guides(linetype=FALSE) +
    theme_light()+
    theme(
        legend.title = element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.15,0.78),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        text=element_text(size=14)
    )  -> climatic2

climatic_glued %>% filter(tau>=min_tau) %>% ggplot() + 
    geom_line(aes(x=tau, y=coefficients, color=variable, linetype =type), size=line_size*0.6) +
    geom_ribbon(aes(x=tau, ymin=`lower bd`, ymax= `upper bd`, fill=variable), alpha=.3) +
    facet_wrap(~model, scales = "free_y")+
    scale_color_manual(values = pal_koppen) +
    scale_fill_manual(values = pal_koppen) +
    scale_y_continuous(breaks = c(0,20,40,60,80), limits = c(-8.5,84)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    coord_cartesian(xlim = c(0.8085, 0.9815)) +
    xlab("Quantile") +
    ylab("Time Trend (Millions US$ per year)") +
    guides(linetype=FALSE) +
    theme_light(base_size = 7)+
    theme(
        legend.title = element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.15,0.78),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        text=element_text(size=7.5),
        legend.key.size = unit(0.8,"line")
    )  -> climatic2_real


ggsave("/Users/matteo/Desktop/devil_graphs/damages_graph.pdf", damages_graph, width=17.53, height=7.2, dpi=500, units="in")
#ggsave("/Users/matteo/Desktop/damages_graph.pdf", damages_graph, width=17.8, height=7.3, dpi=500, units="cm", scale = 2.6)
ggsave("/Users/matteo/Desktop/devil_graphs/climatic.pdf", climatic, width=5.83, height=4.13, dpi=500, units="in")
ggsave("/Users/matteo/Desktop/devil_graphs/climatic_real.pdf", climatic_real, width=8.7, height=6.16, dpi = 500, units="cm")
#ggsave("/Users/matteo/Desktop/climatic.pdf", climatic, width=8.7, height=12.32, dpi=500, units="cm", scale = 1.8)
ggsave("/Users/matteo/Desktop/devil_graphs/climatic2.pdf", climatic2, width=5.83, height=4.13, dpi=500, units="in")
ggsave("/Users/matteo/Desktop/devil_graphs/climatic2_real.pdf", climatic2_real, width=8.7, height=6.16, dpi=500, units="cm")

