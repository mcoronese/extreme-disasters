#This scripts generates two versions of the Figure 1 in the paper

library(tidyverse)
library(quantreg)
library(gridExtra)
library(evd)
library(gridExtra)

#### Settings ####

theme_set(theme_minimal())
set.seed(1)

year_palette  <- c("2"="#b2182b",
                   "1"="#2166ac")

stress_alpha  <- c("2"="0.5",
                   "1"="0.5")

palette       <- c( "2"="#F8766D",
                    "1"="#C77CFF")


start_mean <- 2.7
end_mean   <- 3.4
var        <- 0.5
shapem     <-0


#### Damages Graph ####

g_means <- c(start_mean,end_mean) #means for graphical representations, only two are sufficient, initial and final

for (t in 1:length(g_means)) { #generate data for damage distributions
    damage <- exp(rgev(100000,loc=g_means[t], scale=var, shape = shapem))
    Year <- t
    damages <- cbind(damage, Year)
    if(t==1){
        dam_func <- damages
    }else dam_func <- rbind(dam_func, damages)
}
dam_func      <- dam_func %>% as.data.frame()
dam_func$Year <- factor(dam_func$Year) #year as factor


#GRAPH Damages distributions with geom_density
dam_func %>% filter(damage<800) %>% 
    ggplot() +
    geom_density(aes(x=damage, color=Year, fill=Year), alpha=0.1) +
    scale_fill_manual(values=year_palette, guide = guide_legend(direction = "horizontal")) +
    scale_color_manual(values=year_palette, guide = guide_legend(direction = "horizontal")) +
    coord_flip() +
    ggtitle("C") +
    xlab("Damages") +
    ylab("Density") +
    theme(
        panel.grid = element_blank(),
        legend.position = "none",#c(0.55,0.2),
        axis.line.x = element_line(arrow=arrow(length = unit(0.2, "cm"))),
        axis.line.y = element_line(arrow=arrow(length = unit(0.2, "cm"))),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white")
    )-> damage_distr
damage_distr

aux1 <- density(dam_func[dam_func$Year==1,]$damage, n=5000000)
dam_dens<- as.data.frame(cbind(aux1$x,aux1$y,1))
aux2 <- density(dam_func[dam_func$Year==2,]$damage, n=5000000)
dam_dens2<- as.data.frame(cbind(aux2$x,aux2$y,2))
dam_dens <- rbind(dam_dens, dam_dens2)
colnames(dam_dens) <- c("xis", "yis", "Year")
dam_dens$Year <- factor(dam_dens$Year)


res_damages <- 40

##### Stressor #####

display_l <- 1
display_r <- 2.1

for (i in 1:length(g_means)) { #generate data
    yis <- dgev(seq(g_means[i]-display_l, g_means[i]+display_r, 0.01), g_means[i], var, shape = shapem)
    xis <- seq(g_means[i]-display_l, g_means[i]+display_r, 0.01)
    Year <- i
    density <- cbind(yis,xis, Year)
    if(i==1){
        densities <- density
    }else{
        densities <- rbind(densities, density)
    }
    
}
densities <- densities %>% as.data.frame()
densities$Year <- factor(densities$Year)

arrow_df <- data.frame(x1 = 2.8, x2 = 5.2, y1 = 0.415, y2 = 0.415)

densities %>%
    ggplot() + 
    geom_line(aes(x=xis, y=yis, color=Year)) +
    geom_ribbon(aes(ymin=0, ymax=yis, x=xis, fill=Year), alpha=0.5) +
    geom_segment(data=arrow_df, aes(x = x1, y = y1, xend = x2, yend = y2),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    annotate("text", x=4, y=0.435, label= "time") +
    scale_fill_manual(values = year_palette) +
    scale_color_manual(values = year_palette) +
    ggtitle("A") +
    xlab("Stressor (e.g Temperature, Sea Level)")+
    ylab("Density")+
    theme(legend.position = "none",
          axis.line.x = element_line(arrow=arrow(length = unit(0.2, "cm"))),
          axis.line.y = element_line(arrow=arrow(length = unit(0.2, "cm"))),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "white"),
          panel.grid = element_blank()) -> stresser

stresser

##### Transfer ####

x_trans<- seq(0, 100, 0.01)
y_trans<- exp(x_trans)

trans_data <- cbind(x_trans, y_trans) %>% as.data.frame()

limit_y_damage <- 1500 #set upper limit to better show the distributions

#TRANSFER Graph
trans_data %>% filter(y_trans< limit_y_damage) %>% 
    ggplot() +
    geom_line(aes(x=x_trans, y=y_trans)) +
    ggtitle("B") +
    xlab("Stressor (e.g Temperature, Sea Level)") +
    ylab("Damages") +
    theme(
        panel.grid = element_blank(),
        axis.line.x = element_line(arrow=arrow(length = unit(0.2, "cm"))),
        axis.line.y = element_line(arrow=arrow(length = unit(0.2, "cm"))),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white")
    ) -> transfer

transfer


#### Complete Graph ####
limit_y_damage <- 200

g_trans <- trans_data %>% filter(y_trans< limit_y_damage)
g_dam_dens <- dam_dens %>% filter(xis < limit_y_damage)

#realign lower bound of polygon area to be colored with axis
max1<- max(g_dam_dens[g_dam_dens$Year==1,]$xis)
max54<- max(g_dam_dens[g_dam_dens$Year==2,]$xis)

g_dam_dens[g_dam_dens$xis==max1,]$yis <- 0
g_dam_dens[g_dam_dens$xis==max54,]$yis <- 0

min1<- min(g_dam_dens[g_dam_dens$Year==1,]$xis)
min54<- min(g_dam_dens[g_dam_dens$Year==2,]$xis)

g_dam_dens[g_dam_dens$xis==min1,]$yis <- 0
g_dam_dens[g_dam_dens$xis==min54,]$yis <- 0

res_damages <- 40
res_stress <- 40
det_stress <- 0
det_damages <- 0

#arrows for cartesian axis
arrows <- as.data.frame(cbind(c(0,0,0,0),c(0,0,-res_damages*1.1*max(g_dam_dens$yis),max(densities$xis))*1.1,c(0,0,0,0),c(limit_y_damage*1.1,-max(densities$yis*res_stress)*1.1,0,0),c(1,2,3,4)))
colnames(arrows) <- c("x","xend","y","yend","arr")
arrows2 <- as.data.frame(cbind(c(0,0),c(0,max(densities$xis)*1.1),c(0,0),c(limit_y_damage*1.1,0),c(1,2)))
colnames(arrows2) <- c("x","xend","y","yend","arr")

arrow_time <- data.frame(x1 = start_mean -0.2, x2 = end_mean +0.2, y1 = -36, y2 = -36)

mrg_title <- -7#-1.2

ggplot() +
    geom_line(data=g_trans,aes(x=x_trans, y=y_trans)) +
    geom_line(data=densities,aes(x=xis, y=-res_stress*yis - det_stress, color=Year)) +
    geom_ribbon(data=densities,aes(ymin=-det_stress, ymax=-res_stress*yis -det_stress, x=xis, fill=Year), alpha=0.5) +
    geom_path(data=g_dam_dens,aes(x=res_damages*yis-det_damages, y=xis, color=Year)) +
    geom_polygon(data=g_dam_dens,aes(x=res_damages*yis-det_damages, y=xis, fill=Year), alpha=0.5) +
    geom_segment(data=arrows2,aes(x=x, xend=xend, y=y, yend=yend,group=arr),arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(data=arrow_time,aes(x=x1, xend=x2, y=y1, yend=y2),arrow = arrow(length = unit(0.2, "cm"))) +
    geom_rect(aes(xmin = 0.7, xmax = 3.9, ymin = 90, ymax = 200),   color = "grey", fill="white", size=0.2) +
    annotate("text", x=(end_mean + start_mean)/2, y=-43.5, label= "Time", size=4) +
    scale_color_manual(values = year_palette) +
    scale_fill_manual(values = year_palette) +
    annotate("text", x=3.8, y=9, label= "Stressor", size=4.6) + 
    annotate("text", x=-0.25, y=arrows2[1,4]/2,label= "Damages", angle=90, size=4.6) +
    annotate("text", x=4.7, y=93, label= "Damage Function", angle=64, size=4) +
    ggtitle("   A") +
    theme(
        panel.grid = element_blank(),
        plot.title=element_text(margin=margin(t = 0, r = 0, b = mrg_title, l = 0, unit = "pt"), hjust = 0.005),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white"),
        axis.title = element_blank(),
        title=element_text(size=14)
    ) -> cartesian_mech

#### Reversed Graph 

#realign polygon also for stressors (now on y axis)
d_max1<- max(densities[densities$Year==1,]$xis)
d_max54<- max(densities[densities$Year==2,]$xis)

densities[densities$xis==d_max1,]$yis <- 0
densities[densities$xis==d_max54,]$yis <- 0

d_min1<-  min(densities[densities$Year==1,]$xis)
d_min54<- min(densities[densities$Year==2,]$xis)

densities[densities$xis==d_min1,]$yis <- 0
densities[densities$xis==d_min54,]$yis <- 0

ggplot() +
    geom_line(data=g_trans,aes(y=x_trans, x=y_trans)) +
    geom_line(data=g_dam_dens,aes(y=res_damages*yis-det_damages, x=xis, color=Year)) +
    geom_ribbon(data=g_dam_dens,aes(ymin=-det_damages, ymax=res_damages*yis-det_damages, x=xis, fill=Year), alpha=0.5) +
    geom_path(data=densities,aes(y=xis, x=-res_stress*yis - det_stress, color=Year)) +
    geom_polygon(data=densities,aes(y=xis, x=-res_stress*yis - det_stress, fill=Year), alpha=0.5) +
    geom_segment(data=arrows2,aes(y=x, yend=xend, x=y, xend=yend,group=arr),arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(data=arrow_time,aes(y=x1, yend=x2, x=y1, xend=y2),arrow = arrow(length = unit(0.2, "cm"))) +
    geom_rect(aes(ymin = 0.9, ymax = 3.5, xmin = 70, xmax = 200),   color = "grey", fill="white", size=0.2) + #to fix
    annotate("text", y=(end_mean + start_mean)/2, x=-43.5, label= "Time", size=4, angle = 90) +
    scale_color_manual(values = year_palette) +
    scale_fill_manual(values = year_palette) +
    annotate("text", y=3.8, x=9, label= "Stressor", size=4.6, angle=90) + 
    annotate("text", y=-0.25, x=arrows2[1,4]/2,label= "Damages", size=4.6) +
    annotate("text", y=4.7, x=93, label= "Damage Function", angle=21, size=4) +
    ggtitle("   A") +
    theme(
        panel.grid = element_blank(),
        plot.title=element_text(margin=margin(t = 0, r = 0, b = mrg_title, l = 0, unit = "pt"), hjust = 0.005),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white"),
        axis.title = element_blank(),
        title=element_text(size=14)
    ) -> cartesian_mech_rev 


#### Inset ####

display_l <- 1
display_r <- 3.5

for (i in 1:length(g_means)) { #generate data
    yis <- dgev(seq(g_means[i]-display_l, g_means[i]+display_r, 0.01), g_means[i], var, shape = shapem)
    xis <- seq(g_means[i]-display_l, g_means[i]+display_r, 0.01)
    Year <- i
    density <- cbind(yis,xis, Year)
    if(i==1){
        ins_dens <- density
    }else{
        ins_dens <- rbind(ins_dens, density)
    }
    
}
ins_dens <- ins_dens %>% as.data.frame()
ins_dens$Year <- factor(ins_dens$Year)

# trick for colors which messes up a little bit the whole ribbon but put the right colors in the tails, in order to have the same as others panels
range <- (g_means[2]-g_means[1])/2
ins_dens$min <- 0
max_x_1 <- max(ins_dens[ins_dens$Year==1,"xis"])
hh <- ins_dens[ins_dens$Year==1 & ins_dens$xis>(g_means[1] +range),"yis"]
ins_dens[ins_dens$Year==2 & ins_dens$xis<=max_x_1 & ins_dens$xis>= (g_means[1] +range),]$min <- hh 

ins_dens %>%
    ggplot() + 
    geom_line(aes(x=xis, y=yis, color=Year)) +
    geom_ribbon(aes(ymin=min, ymax=yis, x=xis, fill=Year), alpha=0.5) +
    scale_fill_manual(values = year_palette) +
    scale_color_manual(values = year_palette) +
    coord_cartesian(xlim = c(3.9,6), ylim=c(0.02,0.3))+
    xlab("Log Damages (Right Tails)")+
    ylab("Density")+
    theme(legend.position = "none",#c(0.15,0.8),
          axis.line.x = element_line(arrow=arrow(length = unit(0.2, "cm"))),
          axis.line.y = element_line(arrow=arrow(length = unit(0.2, "cm"))),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "white"),
          axis.title.x = element_text(size=10.5, margin = margin(t=-4)), #9 was baseline
          axis.title.y = element_text(size=10.5, margin = margin(r=2)), #9 was baseline
          panel.grid = element_blank()
    ) -> inset

# Compose the final upper graph
cartesian_mech + annotation_custom(ggplotGrob(inset), xmin= 0.7, xmax=3.9, ymin=90, ymax=200) -> mechanics
mechanics

cartesian_mech_rev + annotation_custom(ggplotGrob(inset), ymin= 0.9, ymax=3.5, xmin=70, xmax=200) -> mechanics_rev
mechanics_rev


#### Quantile Regression ####
means <- seq(start_mean,start_mean+(0.13*54), length.out = 55) 

#Regenerate data with the same method but less observations, otherwise takes too long (10000 observations previously used to obtain smoother figures)
for (t in 1:length(means)) { #generate data for damage distributions
    damage <- exp(rgev(1000, means[t], var, shapem))
    Year <- t
    damages <- cbind(damage, Year)
    if(t==1){
        dam_quantile <- damages
    }else dam_quantile <- rbind(dam_quantile, damages)
}
dam_quantile <- dam_quantile %>% as.data.frame()


tau_vec = c(seq(0.5, 0.99, 0.01))
rq(dam_quantile$damage ~ dam_quantile$Year, tau = tau_vec) -> quantile_reg
plot(quantile_reg)

# arrange data to plot them
quantile_reg$coefficients -> data_quantile
colnames(data_quantile) <- c(seq(0.5, 0.99, 0.01))
data_quantile <- data_quantile[-1,]
data_quantile %>% as.data.frame() -> data_quantile

plot(quantile_reg) -> data_ols
ols <- data_ols[2,ncol(data_ols)]

colnames(data_quantile) <- "value"
data_quantile$variable <- "Quantile Regression"
data_quantile$quantile <- tau_vec

ols_to_bind <- as.data.frame(rep(ols, nrow(data_quantile)))
colnames(ols_to_bind) <- "value"
ols_to_bind$variable <- "Ordinary Least Squares"
ols_to_bind$quantile <- tau_vec

gg_quantile_data <- rbind(data_quantile, ols_to_bind)


#Graph Quantile Regressions
gg_quantile_data %>% filter(quantile>=0.8) %>% ggplot() +
    geom_line(aes(x=quantile, y=value, linetype=variable, size=variable)) +
    scale_color_discrete(guide = guide_legend(direction = "horizontal")) +
    scale_linetype_manual(values = c("Ordinary Least Squares"=2,"Quantile Regression"=1)) +
    scale_size_manual(values = c("Ordinary Least Squares"=0.5,"Quantile Regression"=0.8)) +
    scale_y_continuous(limits = c(200,NA)) +
    theme_light() +
    xlab("Quantile") +
    ylab("Time Trend") +
    ggtitle("B") +
    theme(legend.position = c(0.4,0.7),
          legend.title = element_blank(),
          plot.title=element_text(hjust = -0.045),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size=13)
    ) -> quantile_graph

quantile_graph


#### Final charts ####

lay3 <- rbind(c(1,1),
              c(1,1),
              c(2,2))

#order: rop, right, bottom, left. Default values all at 5.5
cut_mechanics      <- mechanics      + theme(plot.margin = margin(t = 5.5, r = -8.5, b = -7, l = -13.5, unit = "pt"))
cut_mechanics_rev  <- mechanics_rev  + theme(plot.margin = margin(t = 5.5, r = -8.5, b = -7, l = -13.5, unit = "pt"))
cut_quantile_graph <- quantile_graph + theme(plot.margin = margin(t = -7, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))

final     <- grid.arrange(cut_mechanics, cut_quantile_graph, layout_matrix = lay3, padding=400) #SAVE AS 7,8 x 5,51
#save with required proportions
ggsave("/Users/matteo/Desktop/devil_graphs/gev_damage.pdf", final, width=8.7, height=12.32, dpi=500, units="cm", scale=1.8)

final_rev <- grid.arrange(cut_mechanics_rev, cut_quantile_graph, layout_matrix = lay3) #SAVE AS 7,8 x 5,51
#save with required proportions
ggsave("/Users/matteo/Desktop/devil_graphs/gev_damage_rev.pdf", final_rev, width=8.7, height=12.32, dpi=500, units="cm")




