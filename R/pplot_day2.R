library(ggplot2)
data("economics")
str(economics)
plot(unemploy/pop ~ date, data = economics, type = "l")

library(ggplot2)
ggplot(data = economics, aes(x = date, y = unemploy/pop)) + geom_line()

ggplot(data = economics, aes(x = date, y = unemploy/pop))
ggplot(data = economics, aes(x = date, y = unemploy/pop)) + geom_line() + theme_bw()

economics$month <-months(economics$date)
economics$year <- format(economics$date, format="%Y")
head(economics)

cat("Data type of economics$month:", class(economics$month),"\n")
economics$month <- factor(economics$month, levels = month.name)
head(economics$month)


data2009 <- subset(economics, year == 2009)
data2014 <- subset(economics, year == 2014)
plot(unemploy/pop ~ as.numeric(month), data = data2009,ylim = c(0.025, 0.05), type = "l")
lines(unemploy/pop ~ as.numeric(month), data = data2014,col ="red")
legend("topleft", c("2009","2014"), title="Year",col=c("black","red"), lty = c(1,1))


data2009_2014 <- subset(economics, year %in% c(2014, 2009))
# No need to specify a legend, it is produced automatically
ggplot(data = data2009_2014, aes(x = month, y = unemploy/pop)) +
  geom_line(aes(group = year, color = year))

data2009_2014 <- subset(economics, year %in% c(2014, 2009))

# No need to specify a legend, it is produced automatically
ggplot(data = data2009_2014, aes(x = month, y = unemploy/pop)) +
  geom_line(aes(group = year, color = year)) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = economics, aes(x = month, y = unemploy/pop)) +
  geom_line(aes(group = year, color = year)) +
  theme(axis.text.x = element_text(angle = 45))

data("diamonds")
str(diamonds)
head(diamonds)
hist(diamonds$price)
hist(diamonds$price, breaks = 50)
ggplot(diamonds, aes(x = price)) + geom_histogram()

set.seed(12345) # Make the sample reproducible
dsmall <- diamonds[sample(nrow(diamonds), 200), ]

colorMap <- data.frame(color = rainbow(length(unique(dsmall$color))))
rownames(colorMap) <- unique(dsmall$color)
plot(price ~ carat, data = dsmall, col = colorMap[dsmall$color,"color"])
legend(x ='bottomright', legend = rownames(colorMap), col = colorMap$color, pch = par("pch"), bty ='n', xjust = 1)

ggplot(data = dsmall, aes(x = carat, y = price, color = color)) +
  geom_point()

help.search("geom_", package =  "ggplot2")

p1 <- ggplot(dsmall, aes(x = carat, y = price)) ##making plot as a variable
p1 + geom_point()
p1 + geom_point(aes(color = color)) # color by diamonds color
p1 + geom_point(aes(shape = cut)) # set shape by diamond cut
p1 + geom_point(aes(shape = cut, color = color))
ggplot(data = dsmall, aes(x = carat, y = price)) +
    geom_point(aes(size = 2), color =               "darkgreen")

ggplot(data = dsmall, aes(x = carat, y = price)) +
  geom_point(aes(fill = cut), size = 2, color ="black", shape = 25)

#See all 25 available symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
ggplot(df2, aes(x = x, y = y)) +
  geom_point(aes(shape = z), size = 3,colour ="darkgreen", fill ="orange") + scale_shape_identity()

ggplot(dsmall, aes(x = log(carat), y = log(price))) + geom_point() ###log transformation

set.seed(12345) # Make the sample reproducible
dsmall2 <- diamonds[sample(nrow(diamonds), 100), ]

p2 <- ggplot(dsmall2, aes(x = log(carat), y = log(price)))
p2 + geom_text(aes(label = price))
p2 + geom_label(aes(label = carat))

library(ggrepel)
p2 + geom_point() + geom_text_repel(aes(label=color), size = 3)

# Here we plot data NOT log transformed

ggplot(dsmall2, aes(x = carat, y = price)) + geom_point() +
  geom_text_repel(aes(label=color), size = 3)

#sample indices of points to label
set.seed(123456)
idx <- sample(c(TRUE, FALSE), nrow(dsmall2), replace = TRUE, prob = c(0.5, 0.5))
         ggplot(dsmall2, aes(x = carat, y = price)) + geom_point() +
                geom_text_repel(data = subset(dsmall2, idx), aes(label=color),
                                size = 3, col = "Blue") + geom_point()



#Part4, ggplot2
#Change the path
dat <- read.csv("./data/EconomistData.csv")
ggplot(data = diamonds, aes(x = color, y =price/carat)) + geom_jitter()

j1 <- ggplot(data = diamonds, aes(x = color, y =price/carat)) +  geom_jitter(alpha = I(1/5))
j2 <- ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_jitter(alpha = I(1/50))
j3 <- ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_jitter(alpha = I(1/200))
# We use grid.arrange from gridExtra to display multiple plots
library(gridExtra)
grid.arrange(j1, j2, j3, ncol = 3)

##BOXplot!
ggplot(data = diamonds, aes(x = color, y =price/carat)) +
  geom_boxplot()
ggplot(data = diamonds, aes(x = color, y =price/carat)) +
    geom_boxplot() + geom_jitter(alpha = I(1/50))

h <- ggplot(data = diamonds, aes(x = carat)) + geom_histogram()
d <- ggplot(data = diamonds, aes(x = carat)) + geom_density()
grid.arrange(h, d, ncol = 2) ###grid.arrange!! IMPT!!

p <- ggplot(data = diamonds, aes(x = carat)) + xlim(0, 3)
h1 <- p + geom_histogram(binwidth = 0.5)
h2 <- p + geom_histogram(binwidth = 0.1)
h3 <- p + geom_histogram(binwidth = 0.05)
grid.arrange(h1, h2, h3, ncol = 3)

d1 <- p + geom_density(adjust = 5)
d2 <- p + geom_density(adjust = 1)
d3 <- p + geom_density(adjust = 1/5)
grid.arrange(d1, d2, d3, ncol = 3)

# Here we show grouping by diamonds cut.
h <- p + geom_histogram(aes(fill = cut), position = "dodge", bins = 10)
d <- p + geom_density(aes(color = cut))
grid.arrange(h, d, ncol = 2)

h <- p + geom_histogram(aes(fill = cut), position = "stack")
d <- p + geom_density(aes(fill = cut), position = "stack")
grid.arrange(h, d, ncol = 2)

b1 <- ggplot(diamonds, aes(x = color)) + geom_bar()
b2 <- ggplot(diamonds, aes(x = color)) + geom_bar(aes(weight = carat)) + ylab
grid.arrange(b1, b2, ncol = 2)



pEc <- ggplot(dat, aes(Percent.of.15plus.with.bank.account, SEDA.Current.level))
(pEc <- pEc + geom_point(aes(color = Region)) + scale_color_brewer(palette = "Set1"))
typeof(dat$Region)

dat$Region <- as.character(dat$Region)
dat$Region <- factor(dat$Region,
                     levels = c("Europe", "Asia", "Oceania",
                                "North America",
                                "Latin America & the Caribbean",
                                "Middle East & North Africa",
                                "Sub-Saharan Africa"),
                     labels = c("Europe", "Asia", "Oceania",
                                "North America",
                                "Latin America & \n the Caribbean",
                                "Middle East & \n North Africa",
                                "Sub-Saharan \n Africa"))
pEc <- ggplot(dat, aes(Percent.of.15plus.with.bank.account, SEDA.Current.level))
(pEc <- pEc + geom_point(aes(color = Region)) + scale_color_brewer(palette = "Set1"))
pEc <- ggplot(dat, aes(Percent.of.15plus.with.bank.account, SEDA.Current.level))
pEc + geom_point(aes(color = Region))
#add linear trend
pEc <- pEc + geom_smooth(method = "lm", se = FALSE, col = "black", size = 0.5)
(pEc <- pEc + geom_point(aes(fill = Region), color = "white", shape = 21, size =4))
#change ration of height and width
(pEc <- pEc + coord_fixed(ratio = 0.4))
#change color theme
colors <-  c("#28AADC","#F2583F", "#76C0C1","#24576D",
            "#248E84","#DCC3AA", "#96503F")
(pEc <- pEc + scale_fill_manual(name = "",
                                values = colors))
#change title and axis
(pEc <- pEc +
    scale_x_continuous(name = "% of people aged 15+ with bank account, 2014",
                       limits = c(0, 100),
                       breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(name = "SEDA Score, 100-maximum",
                       limits = c(0, 100),
                       breaks = seq(0, 100, by = 20)) +
    ggtitle("Laughing all the way to the bank \n Well-being amd financial inclusion \n 2014-15"))
##ggthemes
# install.pcakages("ggthemes")
library(ggthemes)
pEc0 <- pEc
pEc <- pEc + theme_economist_white(gray_bg=FALSE)
library(gridExtra)
grid.arrange(pEc0, pEc, ncol = 2)

pEc <- pEc + coord_fixed(0.4) +
  theme(text = element_text(color = "grey37", size = 12),
        legend.position = c(0.45, 1.1), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 10, color = "gray10"),
        plot.title = element_text(size = rel(1.1), color = "black"),
        plot.margin = unit(c(1, 1.5, 1.5, 0.5), "cm")) +
  guides(fill = guide_legend(ncol = 4, byrow = FALSE))
pEc

library(ggrepel)
pointsToLabel <- c("Yemen", "Iraq", "Egypt", "Jordan", "Chad", "Congo",
                   "Angola", "Albania", "Zimbabwe", "Uganda", "Nigeria",
                   "Uruguay", "Kazakhstan", "India", "Turkey", "South Africa",
                   "Kenya", "Russia", "Brazil", "Chile", "Saudi Arabia",
                   "Poland", "China", "Serbia", "United States", "United Kingdom")
(pEcText <-  pEc + geom_text_repel(aes(label = Country), color = "gray20",
                                   data = subset(dat, Country %in% pointsToLabel),
                                   force = 20))
#add note
library(grid)
png(file = "./econScatter.png", width = 800, height = 600)
pEcText
grid.text("Source: Boston Consulting Group",
          x = .02, y = .04, just = "left",
          draw = TRUE, gp=gpar(fontsize=10, col="grey37"))
grid.text("Data available for 123 countries \n Sustainable economic development assesment",
          x = 0.98, y = .06, just = "right",
          draw = TRUE, gp=gpar(fontsize=10, col="grey37"))
dev.off()### add note, didnt work

