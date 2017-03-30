library(ggplot2)
data("economics")
str(economics)
## Classes 'tbl_df', 'tbl' and 'data.frame': 574 obs. of 6 variables:
## $ date : Date, format: "1967-07-01" "1967-08-01" ...
## $ pce : num 507 510 516 513 518 ...
## $ pop : int 198712 198911 199113 199311 199498 199657 199808 199920 200056 200208 ...
## $ psavert : num 12.5 12.5 11.7 12.5 12.5 12.1 11.7 12.2 11.6 12.2 ...
## $ uempmed : num 4.5 4.7 4.6 4.9 4.7 4.8 5.1 4.5 4.1 4.6 ...
## $ unemploy: int 2944 2945 2958 3143 3066 3018 2878 3001 2877 2709
head(economics)
## date pce pop psavert uempmed unemploy
## 1 1967-07-01 507.4 198712 12.5 4.5 2944
## 2 1967-08-01 510.5 198911 12.5 4.7 2945
## 3 1967-09-01 516.3 199113 11.7 4.6 2958
## 4 1967-10-01 512.9 199311 12.5 4.9 3143
plot(unemploy/pop ~ date, data = economics, type = "l")
library(ggplot2)
ggplot(data = economics, aes(x = date, y = unemploy/pop)) + geom_line()

ggplot(data = economics, aes(x = date, y = unemploy/pop))

ggplot(data = economics, aes(x = date, y = unemploy/pop)) + geom_line()

ggplot(data = economics, aes(x = date, y = unemploy/pop)) + geom_line() + theme_bw()

economics$month <-months(economics$date)
economics$year <- format(economics$date, format="%Y")
head(economics)


cat("Data type of economics$month:", class(economics$month), "\n")
## Data type of economics$month: character
# Convert the character vector to a ordered factor vector:
economics$month <- factor(economics$month, levels = month.name)
head(economics$month)
## [1] July August September October November December
## 12 Levels: January February March April May June July August ...


#Using Base graphics
data2009 <- subset(economics, year == 2009)
data2014 <- subset(economics, year == 2014)
plot(unemploy/pop ~ as.numeric(month), data = data2009,
     ylim = c(0.025, 0.05), type = "l")
lines(unemploy/pop ~ as.numeric(month), data = data2014,
      col = "red")
legend("topleft", c("2009", "2014"), title="Year",
       col=c("black", "red"), lty = c(1,1))


# USING GGPLOT2
data2009_2014 <- subset(economics, year %in% c(2014, 2009))
# No need to specify a legend, it is produced automatically
ggplot(data = data2009_2014, aes(x = month, y = unemploy/pop)) +  geom_line(aes(group = year, color = year))


# EASY FIX
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


#breaks can be a vector, a function or a single number
hist(diamonds$price, breaks = 50)


ggplot(diamonds, aes(x = price)) + geom_histogram()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


set.seed(12345) # Make the sample reproducible
dsmall <- diamonds[sample(nrow(diamonds), 200), ]


colorMap <- data.frame(color = rainbow(length(unique(dsmall$color))))
rownames(colorMap) <- unique(dsmall$color)
plot(price ~ carat, data = dsmall, col = colorMap[dsmall$color, "color"])
legend(x = 'bottomright', legend = rownames(colorMap), col = colorMap$color, pch = par("pch"), bty = 'n', xjust = 1)


ggplot(data = dsmall, aes(x = carat, y = price, color = color)) +   geom_point()


p1 <- ggplot(dsmall, aes(x = carat, y = price))
p1 + geom_point()

# color by diamonds color
p1 + geom_point(aes(color = color))


#set shape by diamond cut
p1 + geom_point(aes(shape = cut))


p1 + geom_point(aes(shape = cut, color = color))


ggplot(data = dsmall, aes(x = carat, y = price)) + geom_point(aes(size = 2), color = "darkgreen")


ggplot(data = dsmall, aes(x = carat, y = price)) +  geom_point(aes(fill = cut), size = 2, color = "black", shape = 25)


## See all 25 available symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
ggplot(df2, aes(x = x, y = y)) +
  geom_point(aes(shape = z), size = 3,
             colour = "darkgreen", fill = "orange") +
  scale_shape_identity()


ggplot(dsmall, aes(x = log(carat), y = log(price))) + geom_point()



### Text Labels

set.seed(12345) # Make the sample reproducible
dsmall2 <- diamonds[sample(nrow(diamonds), 100), ]

p2 <- ggplot(dsmall2, aes(x = log(carat), y = log(price)))
p2 + geom_text(aes(label = color))


p2 + geom_label(aes(label = color))

library(ggrepel)
p2 + geom_point() + geom_text_repel(aes(label=color), size = 3)


ggplot(dsmall2, aes(x = carat, y = price)) + geom_point() + geom_text_repel(aes(label=color), size = 3)


# sample indices of points to label
set.seed(123456)
idx <- sample(c(TRUE, FALSE), nrow(dsmall2), replace = TRUE, prob = c(0.5, 0.5))
ggplot(dsmall2, aes(x = carat, y = price)) + geom_point() +
  geom_text_repel(data = subset(dsmall2, idx), aes(label=color),
                  size = 3, col = "Blue") + geom_point()



p1 <- ggplot(dsmall, aes(x = carat, y = price))
p1 + geom_point() + scale_y_sqrt()

ggplot(dsmall, aes(x = log(carat), y = log(price))) + geom_point()


library(gridExtra)
p11 <- p1 + geom_point(aes(shape = cut), size = 3)
p12 <- p1 + geom_point(aes(shape = cut), size = 3) +
  scale_shape_manual(values = c(1:5))
grid.arrange(p11, p12, ncol = 2)


p1 + geom_point(aes(color = cut), size = 3) +
  scale_color_manual(values = c("red", "orange", "yellow", "green", "blue"))

p1 + geom_point(aes(color = price), size = 3) +
  scale_color_gradient(low = "blue", high = "red")

p1 + geom_point(aes(color = cut), size = 3) +
  scale_color_brewer(palette = "Set2")

# scale_color_brewer() does not work with continuous variables
# and will result in an error
p1 + geom_point(aes(shape = price), size = 3) +
  scale_color_brewer(palette = "Spectral")
# Error: A continuous variable can not be mapped to shape


library(RColorBrewer)
p1 + geom_point(aes(color = price), size = 3) +
  scale_color_gradientn(colours = brewer.pal(name = "Spectral", n = 10))


#install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)

p1 + geom_point(aes(color = cut), size = 3) +
  scale_color_manual(values = wes_palette("Darjeeling", n = 5))


p1 + geom_point(aes(color = price), size = 3) +
  scale_color_gradientn(colours = wes_palette("Darjeeling", 100, type = "continuous"))


p1 + geom_point(aes(color = price), size = 3) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10")


p1 + geom_point(aes(color = price), size = 3) +
  scale_color_gradient(low = "blue", high = "red", trans = "log10",
                       breaks = c(1000, 2000, 5000, 10000),
                       labels = c("  1000", "  2000", "  5000", "10000"))


dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
p0 <- ggplot(data = dsmall, aes(x = carat, y = price)) +
  geom_point(size = 1) +
  geom_smooth(aes(colour = cut, fill = cut))
p1 <- p0 + facet_wrap(~ cut)
grid.arrange(p0, p1, ncol = 2)


# Change the path
dat <- read.csv("data/EconomistData.csv")



pEc <- ggplot(dat, aes(Percent.of.15plus.with.bank.account, SEDA.Current.level))
(pEc <- pEc + geom_point(aes(color = Region)) + scale_color_brewer(palette = "Set1"))


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
pEc + geom_point(aes(color = Region))

pEc <- pEc + geom_smooth(method = "lm", se = FALSE, col = "black", size = 0.5)
(pEc <- pEc + geom_point(aes(fill = Region), color = "white", shape = 21, size =4))


(pEc <- pEc + coord_fixed(ratio = 0.4))

colors <-  c("#28AADC","#F2583F", "#76C0C1","#24576D", "#248E84","#DCC3AA", "#96503F")
(pEc <- pEc + scale_fill_manual(name = "",values = colors))



(pEc <- pEc +
    scale_x_continuous(name = "% of people aged 15+ with bank account, 2014",
                       limits = c(0, 100),
                       breaks = seq(0, 100, by = 20)) +
    scale_y_continuous(name = "SEDA Score, 100-maximum",
                       limits = c(0, 100),
                       breaks = seq(0, 100, by = 20)) +
    ggtitle("Laughing all the way to the bank \n Well-being amd financial inclusion \n 2014-15"))


pEc0 <- pEc
pEc <- pEc + theme_economist_white(gray_bg=FALSE)
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




library(ggrepel)
pointsToLabel <- c("Yemen", "Iraq", "Egypt", "Jordan", "Chad", "Congo",
                   "Angola", "Albania", "Zimbabwe", "Uganda", "Nigeria",
                   "Uruguay", "Kazakhstan", "India", "Turkey", "South Africa",
                   "Kenya", "Russia", "Brazil", "Chile", "Saudi Arabia",
                   "Poland", "China", "Serbia", "United States", "United Kingdom")
(pEcText <-  pEc + geom_text_repel(aes(label = Country), color = "gray20",
                                   data = subset(dat, Country %in% pointsToLabel),
                                   force = 20))


library(grid)
png(file = "econScatter.png", width = 800, height = 600)
pEcText
grid.text("Source: Boston Consulting Group",
          x = .02, y = .04, just = "left",
          draw = TRUE, gp=gpar(fontsize=10, col="grey37"))
grid.text("Data available for 123 countries \n Sustainable economic development assesment",
          x = 0.98, y = .06, just = "right",
          draw = TRUE, gp=gpar(fontsize=10, col="grey37"))
dev.off()



for (i in cpd.names) {
  if( length(grep('^z ',i)>0) ){
    print(i)
  }
}





