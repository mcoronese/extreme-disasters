#load("simul/new/final_estimation_1k.RData") #to access all quantiles
#load("simul/new/final_estimation_1k_tidy.RData")
library(xlsx)
load("simul/new/estimation_tidy_correct.RData")


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


#list of regressions (names)

regression_list<-c("s1_60_damage",#BASELINE
                   "s1_70_damage",
                   "s1_80_damage",
                   "s3_60_damage",#WITH POP
                   "s3_70_damage",
                   "s3_80_damage",
                   "s4_60",#WITH GDP PER CAPITA
                   "s8_60_damage", #CLIMATIC (also separate script)
                   "s5_60", #interaction term
                   "int_60_deaths", #DEATHS BASELINE
                   "int_70_deaths",
                   "int_80_deaths",
                   "intgdp_60_deaths", #DEATHS WITH GDP
                   "intgdp_70_deaths",
                   "intgdp_80_deaths")

r1_names<- c("base_damage_60",
             "base_damage_70",
             "base_damage_80",
             "pop_damage_60",
             "pop_damage_70",
             "pop_damage_80",
             "gdp_per_capita",
             "koppen",
             "interaction_term",
             "deaths_int_60",
             "deaths_int_70",
             "deaths_int_80",
             "deaths_gdpint_60",
             "deaths_gdpint_70",
             "deaths_gdpint_80")


#create empty matrix
for(j in 1:length(regression_list)){
    quantile_reg <- get(paste0("summary_",regression_list[j]))
    
    #create the table for this particular regression
    n_col <- length(quantile_reg) +1 # number of quantiles
    n_row <- nrow(quantile_reg[[1]]$coefficients)*2 +1 #number of variables
    names_col <-  c("70th", "80th", "90th", "95th", "99th", "OLS") #colnames 
    names_row <- as.vector(rownames(quantile_reg[[1]]$coefficients)) #rownames
    names_row <- c(rbind(names_row, rep("",n_row/2)), "r1") #insert empty spaces for stderror rows
    local_table <- matrix(NA, nrow = n_row, ncol = n_col, dimnames = list(names_row, names_col))
    
    #scroll quantiles and write them tidy in the table
    for(i in 1:length(quantile_reg)){
        quantile_reg[[i]]$coefficients[,"Pr(>|t|)"] -> local_p_values
        values <- paste0(round(quantile_reg[[i]]$coefficients[,"Value"],3),p_stars(local_p_values))
        std_error <- round(quantile_reg[[i]]$coefficients[,"Std. Error"],3)
        line <- c(t(cbind(values,std_error)))
        local_table[1:length(line),i] <- line
        #add r1 line
        local_table[nrow(local_table),i]<- round(r1s[r1_names[j],i],3)
    }
    
    #add OLS data
    linear_reg <- summary(get(paste0(regression_list[j],"_lin")))
    linear_reg$coefficients[,"Pr(>|t|)"] -> lin_p_values
    lin_values <- paste0(round(linear_reg$coefficients[,"Estimate"],3),p_stars(lin_p_values))
    lin_std_error <- round(linear_reg$coefficients[,"Std. Error"],3)
    lin_line <- c(t(cbind(lin_values,lin_std_error)))
    local_table[1:length(line),"OLS"] <- lin_line
    #add r2 line
    local_table[nrow(local_table),"OLS"]<- round(linear_reg$r.squared,3)
    
    #save table with proper naming
    assign(paste0(regression_list[j],"_table"), local_table)
    
    
    if(j==1){
        total_table <- rbind(r1_names[j],local_table)
    }else{
        total_table <- rbind(total_table, rbind(r1_names[j],local_table))
    }
}

write.csv2(total_table,"tables.csv")  

write.table(total_table, "tables.csv", sep='\t')

summary(intgdp_60_deaths_lin)
