
####################
# necessary packages
library(data.table)
library(ggplot2)
library(ggrepel)

################
# analytic model
risk <- function(d,r){ 
  return(exp((-pi*(d)*(r^2))))
}

################
# build Figure 2

density <- seq(0,3,0.1) # set up range of densities of WPs to consider
radii <- c(0.1,0.25,0.5,0.75,1) # radii to visualize
dt <- data.table(expand.grid(density, radii)) # create dataframe
colnames(dt) <- c("density", 'radius') # name columns with useful labels
dt[,radiusMetres:=radius*1000,]
dt[,probNoProtector:=risk(density,radius),] # use function to calculate prob. of being alone when threat arrives (i.e., not having WP within radius)
  
g <- ggplot(dt,aes(x=density,y=probNoProtector)) +
  geom_line(aes(color=radiusMetres, group=radiusMetres), linewidth=0.8) +
  # scale_color_gradient(name='Distance to threat (m)', low='firebrick2',high='grey')+
  scale_color_gradient(name=expression("(" * r * ")" ~ "Distance to threat (m)"), low='firebrick2',high='grey')+
  geom_text_repel(data=dt[density==mean(dt$density),,],aes(label=paste(as.character(radiusMetres),'m',sep='')), point.padding=0.5)+
  # labs(x=bquote('Density of protectors '(km^2)), y="Probability of having no protectors within range") +
  labs(y = expression("(" * rho * ")" ~ "Probability of having no protectors within range " ~ "(" * km^-2 * ")"),
       x = expression("(" * d * ")" ~ "Density of protectors"))+
  theme_classic()

png('Figure2.png', width=8, height=6, units='in', res=800)
g
dev.off()








