library(tidyverse)
iris %>%
  
  # Scatter plot
  ggplot(mapping=aes(x=Petal.Length, y=Petal.Width, col=Species, pch=Species)) +
  geom_point() +
  scale_colour_manual(values=c("steelblue","orange","salmon"), name="Species") +
  scale_shape_manual(values=15:17, name="Species") +
  xlab("Petal length") +
  ylab("Petal width") +
  labs(title="Scatterplot of petal length and width in the iris dataset",caption="Figure produced during the R Training @ MLW, using ggplot2.") +
  theme_light()  

#pch = plot character
#col = color

# Line graphs
iris %>%  
  ggplot(mapping=aes(x=Petal.Length,y=Petal.Width,col=Species,pch=Species)) +
  geom_line() +
  scale_colour_manual(values=c("steelblue","orange","salmon"),name="Species") +
  scale_shape_manual(values=15:17,name="Species") +
  xlab("Petal length") +
  ylab("Petal width") +
  labs(title="Line graph of petal length and width in the iris dataset",caption="Figure produced during the MLW R training, using ggplot2.") +
  theme_light()


# Box and jitter plot
iris %>%  
  ggplot(mapping=aes(x=Species,y=Petal.Width,col=Species)) +
  geom_boxplot() +
  geom_jitter(height=0,width=0.2) +
  scale_colour_manual(values=c("steelblue","orange","salmon")) +
  scale_fill_manual(values=c("steelblue","orange","salmon")) +
  xlab("Species") +  
  ylab("Petal width") +
  labs(title="Box and jitter plot of petal width against species in the iris dataset",caption="Figure produced during the MLW R training, using ggplot2.")

# Generate some data
typesTmp<- paste(sep="","type",1:3)
type<-factor(sample(typesTmp,
                    size=1000,
                    replace=T,
                    prob=c(0.45,0.3,0.25)))
x1<-rbinom(1000,size=1,prob=0.25)
#n = number of observations
# size = number of trials

x2<-rpois(n=1000,lambda=ifelse(type=="type3",6,4))


dat<-data.frame(type,x1,x2) %>%
  mutate(x3=ifelse(type=="type1",
                   rnorm(sum(type=="type1"),mean=-2),
                   ifelse(type=="type2",
                          rnorm(sum(type=="type2"),mean=2),
                          runif(sum(type=="type3"))
                          )
                   )
         ) %>%
  mutate(x4=rnorm(n(),mean=x3))

# Bar plot
ggplot(data=dat,mapping=aes(x=x1)) +
  geom_bar()

ggplot(data=dat,mapping=aes(x=x2)) +
  geom_bar()

ggplot(data=dat,mapping=aes(x=type)) +
  geom_bar()

dat %>%
  count(type,x2) %>%
  complete(type,x2,fill=list(n=0)) %>%
  ggplot(mapping=aes(fill=type,y=n,x=x2)) +
  geom_bar(position="dodge",stat="identity") +
  theme(text = element_text(size=20))

#Changing a few things on the plot
ggplot(data=dat,mapping=aes(x=x2)) + 
  geom_bar() +
  coord_flip() +
  ggtitle("Barplot for variable x2") +
  xlab("values") +
  ylab("count") +
  theme(text = element_text(size=20))

# Flipping using polar
ggplot(data=dat,mapping=aes(x=x2)) +
  geom_bar() + coord_polar(start=0) + theme_minimal() +
  theme(
    axis.text = element_blank(),
             axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm") # removing unnecessary margins
    ) +
  ylim(-100,250) # dtermines size of the inner radius
                                         
# Pie chart
ggplot(dat, mapping=aes(x=factor(1), fill=factor(type))) +
  geom_bar(width = 1) +
  coord_polar("y") + 
  xlab("") +
  theme_void()


# Histograms
ggplot(data=dat,mapping=aes(x=x3)) +
  geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


ggplot(data=dat,mapping=aes(x=x3)) +
  geom_histogram(binwidth=0.1)

ggplot(data=dat,mapping=aes(x=x3,stat(density))) +
  geom_histogram(binwidth=0.15)

ggplot(data=dat,mapping=aes(x=x3,stat(density))) +
  geom_histogram(binwidth=0.15) +
  coord_cartesian(xlim = c(-10, 10)) +
  ggtitle("Histogram") +
  theme_light() +
  theme(text=element_text(size=24))

# Adding kernel density
ggplot(data=dat,mapping=aes(x=x3,stat(density))) +
  geom_histogram(binwidth=0.15) +
  geom_density(bw="SJ",col="blue",lwd=1)

# Stratifying by a variable
ggplot(data=dat,mapping=aes(x=x3,fill=type)) +
  geom_histogram(binwidth=0.15,position="dodge")

ggplot(data=dat,mapping=aes(x=x3,fill=type)) +
  geom_histogram(binwidth=0.15,position="dodge") +
  scale_fill_manual(values=c("steelblue","orange","salmon"))

# Covariation figures
ggplot(data=dat,mapping=aes(x=type,y=x3)) +
  geom_boxplot()

ggplot(data=dat,mapping=aes(x=type,y=x3)) +
  geom_boxplot() +
  geom_jitter(width=0.25,height=0,alpha=0.3)

ggplot(data=dat,mapping=aes(x=type,y=x3)) +
  geom_boxplot() +
  coord_flip()

# Violine plots
ggplot(data=dat,mapping=aes(x=type,y=x3,fill=type)) +
  geom_violin() +
  geom_boxplot(width=0.05, fill="white")

# scatter plot
ggplot(data=dat,mapping=aes(x=x3,y=x4)) +
  geom_point()


ggplot(data=dat,mapping=aes(x=x3,y=x4,col=type)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values=c("steelblue","salmon","orange"),
                      name="Type") +
  xlab("Variable x3") +
  ylab("Variable x4") +
  ggtitle("A covariation plot.") +
  theme_light() +
  theme(text=element_text(size=14))
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'


# beaver
beaver<-rbind(beaver1[beaver1$day==346,],beaver2[beaver2$day==307,])
beaver<-data.frame(
  name=c(rep("beaver1",sum(beaver1$day==346)),
         rep("beaver2",sum(beaver2$day==307))),beaver)

# Line graph
beaver %>%
  filter(name=="beaver1") %>%
  ggplot(mapping=aes(x=time,y=temp)) +
  geom_line()

ggplot(data=beaver,mapping=aes(x=time,y=temp,colour=name)) +
  geom_line(lwd=1.5) # the lwd parameter sets the line width

# heat maps - covariation in 3 variables
dens<-function(x,y){
  return(
    0.35*dnorm(x)*dnorm(y,sd=1.5) + 
      0.65*dnorm(x,mean=2,sd=2)*dnorm(y,mean=3)
    )
  }
x<-seq(-2.5,6.5,by=0.05)
y<-seq(-3,5.5,by=0.05)
densSurf<-expand.grid(x=x,y=y) %>%
  mutate(dens=dens(x,y))

densSurf %>%
  ggplot(mapping=aes(x=x,y=y,fill=dens)) +
  geom_tile(width=0.05,height=0.05)

clrs<-colorRampPalette(c("blue","red","orange","yellow","white"))
densSurf %>%
  ggplot(mapping=aes(x=x,y=y,fill=dens)) +
  geom_tile(width=0.05,height=0.05) +
  scale_fill_gradientn(colours = clrs(200),name="probability density") +
  theme_minimal()

# contour plots
ggplot(data=densSurf,mapping=aes(x=x,y=y,z=dens)) +
  geom_contour()

# combining heat maps and contour plots
densSurf %>%
  ggplot(mapping=aes(x=x,y=y,fill=dens,z=dens)) +
  geom_tile(width=0.05,height=0.05) +
  geom_contour(col="darkgrey",lwd=0.35,alpha=0.75) +
  scale_fill_gradientn(colours = clrs(200),name="probability density") +
  theme_minimal()

library(gridExtra)
g1<-ggplot(data=dat,mapping=aes(x=type,y=x3)) +
  geom_boxplot() +
  geom_jitter(width=0.25,height=0,alpha=0.3) +
  xlab("") +
  ylab("Variable x3") +
  ggtitle("A box & jitter plot.") +
  theme_minimal()

g2<-ggplot(data=dat,mapping=aes(x=x3,y=x4,col=type)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values=c("steelblue","salmon","orange"),
                      name="Type") +
  xlab("Variable x3") +
  ylab("Variable x4") +
  ggtitle("A covariation plot.") +
  theme_light() +
  theme(text=element_text(size=14))

grid.arrange(g1,g2,nrow=1)
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

iris %>%
  ggplot(mapping=aes(x=Petal.Length,y=Petal.Width)) +
  geom_point() +
  facet_wrap(~Species)

# saving graphs
ggsave(g1,filename="myplot.png",width=16,height=9,units="cm",dpi=450)

pdf(width=16,height=9,file="myfile.pdf") # opens the device; pdf will produce vector graphics
# plotting code here
dev.off() # closes the device

png(width=16,height=9,units="in",res=450) # raster graphics
# plotting code heredev.off()






