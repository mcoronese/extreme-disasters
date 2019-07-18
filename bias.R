# Bias Robustness Check

#### Packages ####
library("quantreg")
require("tidyverse")
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
library("wesanderson")# Palettes
library("ggmap")      # Spatial data management
library("grid")       # Inset graphs 
library("gridExtra")


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


#### SETUP ####

yearly_draws <- 318
years <- 55
mc <- 500

tau_vec <- c(0.50, 0.70, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79, 0.80, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99)

#### BASELINE ####
# No shift in distribution

disasters %>% filter(year>=2000) %>%  select(tot_damages) -> empirical_dist 
empirical_dist$tot_damages <- empirical_dist$tot_damages/1000 #Million of Dollars

disasters %>% filter(year>=2000) %>% group_by(year) %>% 
    summarise(
        number=n()
    ) %>% summarise(
        mean=mean(number)
    ) #318 observations on average

p<-1
for(p in 1:mc){
    set.seed(p)
    print(p)
    damage <- data.frame(obs=sample(empirical_dist$tot_damages,yearly_draws), year=0)
    i<-2
    for (i in 2:years) {
        aux <- data.frame(obs=sample(empirical_dist$tot_damages,yearly_draws), year=i-1)
        damage <- rbind(damage, aux)
    }
    
    rq(data = damage, formula = obs ~ year, 
       tau = tau_vec) -> rq_obj
    if(p==1){
        coeff_matrix_base <- rq_obj$coefficients["year",]
    } else{
        row <- rq_obj$coefficients["year",]
        coeff_matrix_base <- rbind(coeff_matrix_base, row)
    }
    
}



#### TRUNCATED ####

p <- 1
for (p in 1:mc) {
    set.seed(p)
    damage <- data.frame(obs=sample(empirical_dist$tot_damages,yearly_draws), year=0)
    i=2
    for (i in 2:years) {
        aux <- data.frame(obs=sample(empirical_dist$tot_damages,yearly_draws), year=i-1)
        damage <- rbind(damage, aux)
    }
    
    cut <- quantile(damage[damage$year==0,]$obs, 0.5)
    if(cut==0){
        which_cut <- 0.5*length(damage[damage$year==0,]$obs)
        damage %>% filter(year==0) -> interm
        sorted <- interm[order(interm$obs),]
        partial <- sorted[which_cut:nrow(sorted),]
    }else{
        partial <- damage %>% filter(year==0 & obs > cut)
    }
    censored <- partial
    
    i=2
    for (i in 2:21) { #like 1960 to 1980, 20 years
        cut <- quantile(damage[damage$year==(i-1),]$obs, (0.5 - (0.02*(i-1) ) ) )
        if(cut==0){
            which_cut <- (0.5 - (0.02*(i-1) ) )*length(damage[damage$year==(i-1),]$obs)
            damage %>% filter(year==(i-1)) -> interm
            sorted <- interm[order(interm$obs),]
            partial <- sorted[which_cut:nrow(sorted),]
        }else{
            partial <- damage %>% filter(year==(i-1) & obs > cut)
        }
        censored <- rbind(censored, partial)
    }
    
    i=22
    for (i in 22:31) { #like 1981 to 1990, 10 years
        cut <- quantile(damage[damage$year==(i-1),]$obs, (0.1 - (0.005*(i-21) ) ) )
        if(cut==0){
            which_cut <- (0.1 - (0.005*(i-21) ) )*length(damage[damage$year==(i-1),]$obs)
            damage %>% filter(year==(i-1)) -> interm
            sorted <- interm[order(interm$obs),]
            partial <- sorted[which_cut:nrow(sorted),]
        }else{
            partial <- damage %>% filter(year==(i-1) & obs > cut)
        }
        censored <- rbind(censored, partial)
    }
    
    i=32
    for (i in 32:years) { #like 1981 to 2014
        cut <- quantile(damage[damage$year==(i-1),]$obs, 0.05 )
        if(cut==0){
            which_cut <- 0.05*length(damage[damage$year==(i-1),]$obs)
            #sorted <- sort(damage, decreasing = FALSE)
            damage %>% filter(year==(i-1)) -> interm
            sorted <- interm[order(interm$obs),]
            partial <- sorted[which_cut:nrow(sorted),]
        }else{
            partial <- damage %>% filter(year==(i-1) & obs > cut)
        }
        censored <- rbind(censored, partial)
    }
    
    
    rq(data = censored, formula = obs ~ year, 
       tau = tau_vec) -> rq_trunc_obj
    
    if(p==1){
        coeff_matrix_trunc <- rq_trunc_obj$coefficients["year",]
    } else{
        row <- rq_trunc_obj$coefficients["year",]
        coeff_matrix_trunc <- rbind(coeff_matrix_trunc, row)
    }
    
}

#### TRUNC LOGISTIC ####

plogis(0,0,13) #year 0 is 1960
plogis(30,0,13) # with mean 0 and sd 13, we have 50% in 1960, 66% in 1970, 79% in 1980, 90% in 1990, 96% nowadays

#set parameters of logistic
mean_log <- 0
sd_log <- 13

p <- 1
for (p in 1:mc) {
    set.seed(p)
    damage <- data.frame(obs=sample(empirical_dist$tot_damages,yearly_draws), year=0)
    i=2
    for (i in 2:years) {
        aux <- data.frame(obs=sample(empirical_dist$tot_damages,yearly_draws), year=i-1)
        damage <- rbind(damage, aux)
    }
    
    cut <- quantile(damage[damage$year==0,]$obs, plogis(0,mean_log,sd_log))
    if(cut==0){
        which_cut <- plogis(0,mean_log,sd_log)*length(damage[damage$year==0,]$obs)
        damage %>% filter(year==0) -> interm
        sorted <- interm[order(interm$obs),]
        partial <- sorted[(which_cut+1):nrow(sorted),]
    }else{
        partial <- damage %>% filter(year==0 & obs > cut)
    }
    log_censored <- partial
    
    j=2
    for (j in 2:years) {
        cut <- quantile(damage[damage$year==(j-1),]$obs, 1 - plogis((j-1),mean_log,sd_log)  )
        if(cut==0){
            which_cut <- (1 - plogis((j-1),mean_log,sd_log))*length(damage[damage$year==(j-1),]$obs)
            damage %>% filter(year==(j-1)) -> interm
            sorted <- interm[order(interm$obs),]
            partial <- sorted[which_cut:nrow(sorted),]
        }else{
            partial <- damage %>% filter(year==(j-1) & obs > cut)
        }
        log_censored <- rbind(log_censored, partial)
    }
    
    
    rq(data = log_censored, formula = obs ~ year, 
       tau = tau_vec) -> rq_log_cens_obj
    
    if(p==1){
        coeff_matrix_log_censored <- rq_log_cens_obj$coefficients["year",]
    } else{
        row <- rq_log_cens_obj$coefficients["year",]
        coeff_matrix_log_censored <- rbind(coeff_matrix_log_censored, row)
    }
    
}


#### MATRIX PLOTTING ####
#construct matrixes for final plotting

load("~/Google Drive/Disasters/empirical_simulation.RData")

#base
# evaluate mean and mc se of estimate coefficients

mean_beta <- array(NA, length(tau_vec))
se_beta   <- array(NA, length(tau_vec))

j <- 1
for (j in 1:length(tau_vec)) {
    mean_beta[j] <- mean(coeff_matrix_base[,j])
    se_beta[j] <- sqrt(sum((coeff_matrix_base[,j] - mean(coeff_matrix_base[,j]))^2)/(mc-1))
}

plot_base <- matrix(NA, nrow = length(tau_vec), ncol= 5 )

plot_base[,1] <- mean_beta
plot_base[,2] <- se_beta
plot_base[,3] <- plot_base[,1] - plot_base[,2]*1.96 
plot_base[,4] <- plot_base[,1] + plot_base[,2]*1.96
plot_base[,5] <- tau_vec


plot_base <- as.data.frame(plot_base)

#trunc
# evaluate mean and mc se of estimate coefficients

mean_beta <- array(NA, length(tau_vec))
se_beta   <- array(NA, length(tau_vec))

j <- 1
for (j in 1:length(tau_vec)) {
    mean_beta[j] <- mean(coeff_matrix_trunc[,j])
    se_beta[j] <- sqrt(sum((coeff_matrix_trunc[,j] - mean(coeff_matrix_trunc[,j]))^2)/(mc-1))
}

plot_trunc <- matrix(NA, nrow = length(tau_vec), ncol= 5 )

plot_trunc[,1] <- mean_beta
plot_trunc[,2] <- se_beta
plot_trunc[,3] <- plot_trunc[,1] - plot_trunc[,2]*1.96 
plot_trunc[,4] <- plot_trunc[,1] + plot_trunc[,2]*1.96
plot_trunc[,5] <- tau_vec


plot_trunc <- as.data.frame(plot_trunc)
#log_censored
# evaluate mean and mc se of estimate coefficients

mean_beta <- array(NA, length(tau_vec))
se_beta   <- array(NA, length(tau_vec))

j <- 1
for (j in 1:length(tau_vec)) {
    mean_beta[j] <- mean(coeff_matrix_log_censored[,j])
    se_beta[j] <- sqrt(sum((coeff_matrix_log_censored[,j] - mean(coeff_matrix_log_censored[,j]))^2)/(mc-1))
}

plot_log_censored <- matrix(NA, nrow = length(tau_vec), ncol= 5 )

plot_log_censored[,1] <- mean_beta
plot_log_censored[,2] <- se_beta
plot_log_censored[,3] <- plot_log_censored[,1] - plot_log_censored[,2]*1.96 
plot_log_censored[,4] <- plot_log_censored[,1] + plot_log_censored[,2]*1.96
plot_log_censored[,5] <- tau_vec


plot_log_censored <- as.data.frame(plot_log_censored)


#### Graphs ####

a1 <- cbind(plot_base, "base")
colnames(a1)[colnames(a1)=="\"base\""] <- "sim"
a2 <- cbind(plot_trunc, "trunc")
colnames(a2)[colnames(a2)=="\"trunc\""] <- "sim"
a3 <- cbind(plot_log_censored, "log_censored")
colnames(a3)[colnames(a3)=="\"log_censored\""] <- "sim"
adv_plot <- rbind(a1,a2,a3)

adv_plot$sim <- factor(adv_plot$sim,
                       levels = c("base", "trunc", "log_censored"),
                       labels = c("True Model", "Piecewise Linear Truncation", "Logistic Truncation")
)

adv_plot$sim = with(adv_plot, factor(sim, levels = rev(levels(sim))))

pal <- c("True Model" =                  "#F8766D", 
         "Piecewise Linear Truncation" = "#00BA38",
         "Logistic Truncation" =         "#619CFF")

pal2 <- c("True Model" =                  "#00BA38", 
          "Logistic Truncation" =         "#F8766D")



adv_plot %>% filter(V5>=0.8) %>% filter(sim!="Piecewise Linear Truncation") %>% 
    ggplot() +
    geom_line(aes(x=V5, y= V1, color=sim), size=0.6) +
    geom_ribbon(aes(x=V5, ymin=V3, ymax=V4, fill=sim), alpha=0.3) +
    xlab("Quantile") + 
    ylab("Time Trend") +
    coord_cartesian(xlim = c(0.808, 0.982)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    scale_fill_manual(values = pal2) +
    scale_color_manual(values = pal2) +
    theme_light() +
    guides(fill = guide_legend(reverse = TRUE), color= guide_legend(reverse = TRUE)) +
    theme_light(base_size = 7) +
    theme(#legend.position = "bottom",
          legend.position = c(0.2,0.2),
          legend.title = element_blank(),
          panel.grid = element_blank(),
          text=element_text(size=7.5),
          legend.key.size = unit(0.8,"line")
    ) -> g_log_real


adv_plot %>% filter(V5>=0.8) %>% filter(sim!="Piecewise Linear Truncation") %>% 
    ggplot() +
    geom_line(aes(x=V5, y= V1, color=sim), size=1) +
    geom_ribbon(aes(x=V5, ymin=V3, ymax=V4, fill=sim), alpha=0.3) +
    xlab("Quantile") + 
    ylab("Time Trend") +
    coord_cartesian(xlim = c(0.808, 0.982)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    scale_fill_manual(values = pal2) +
    scale_color_manual(values = pal2) +
    theme_light() +
    guides(fill = guide_legend(reverse = TRUE), color= guide_legend(reverse = TRUE)) +
    theme(#legend.position = "bottom",
        legend.position = c(0.2,0.2),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        text=element_text(size=14)
    ) -> g_log #save it as a6

pal3 <- c("True Model" =                  "#619CFF", 
          "Piecewise Linear Truncation" = "#F8766D")

adv_plot %>% filter(V5>=0.8) %>% filter(sim!="Logistic Truncation") %>% 
    ggplot() +
    geom_line(aes(x=V5, y= V1, color=sim), size=0.6) +
    geom_ribbon(aes(x=V5, ymin=V3, ymax=V4, fill=sim), alpha=0.3) +
    xlab("Quantile") + 
    ylab("Time Trend") +
    coord_cartesian(xlim = c(0.808, 0.982)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    scale_fill_manual(values = pal3) +
    scale_color_manual(values = pal3) +
    guides(fill = guide_legend(reverse = TRUE), color= guide_legend(reverse = TRUE)) +
    theme_light(base_size = 7) +
    theme(#legend.position = "bottom",
          legend.position = c(0.25,0.2),
          legend.title = element_blank(),
          panel.grid = element_blank(),
          text=element_text(size=7.5),
          legend.key.size = unit(0.8,"line")
    ) -> g_piece_real #save it as a6

adv_plot %>% filter(V5>=0.8) %>% filter(sim!="Logistic Truncation") %>% 
    ggplot() +
    geom_line(aes(x=V5, y= V1, color=sim), size=1) +
    geom_ribbon(aes(x=V5, ymin=V3, ymax=V4, fill=sim), alpha=0.3) +
    xlab("Quantile") + 
    ylab("Time Trend") +
    coord_cartesian(xlim = c(0.808, 0.982)) +
    scale_x_continuous(breaks=c(0.8,0.85,0.9,0.95),
                       labels=c("80th", "85th", "90th", "95th")) +
    scale_fill_manual(values = pal3) +
    scale_color_manual(values = pal3) +
    guides(fill = guide_legend(reverse = TRUE), color= guide_legend(reverse = TRUE)) +
    theme_light() +
    theme(#legend.position = "bottom",
        legend.position = c(0.25,0.2),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        text=element_text(size=14)
    ) -> g_piece #save it as a6



ggsave("/Users/matteo/Desktop/devil_graphs/bias.pdf", g_log, width=5.83, height=4.13, dpi=500, units="in")
ggsave("/Users/matteo/Desktop/devil_graphs/bias_real.pdf", g_log_real, width=8.7, height=6.16, dpi=500, units="cm")
ggsave("/Users/matteo/Desktop/devil_graphs/piece_bias.pdf", g_piece, width=5.83, height=4.13, dpi=500, units="in")
ggsave("/Users/matteo/Desktop/devil_graphs/piece_bias_real.pdf", g_piece_real, width=8.7, height=6.16, dpi=500, units="cm")
