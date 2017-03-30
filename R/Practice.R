> read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")

set.seed(1234)
x <- rnorm(n = 500, mean = 4, sd = 1)
y <- summary(x)

wt = c (5, 6, 7)
ko = c (8, 9, 11)

t.test (wt, ko)

wt.vs.ko = t.test(wt, ko, var.equal=T)

names(wt.vs.ko)

wt.vs.ko$p.value

history(max.show=Inf)

#list 20 commonly used functions, use cheatsheet
read.table
read.csv
write.table
length

dim

commandArgs()
seq(from, to, by)

x[x>0]


cbind()
rbind()

##
library()
search()

layout(matrix(1:4, 2, 2))
plot(stree(100))
plot(stree(128, "balanced"))
plot(stree(100, "left"))
plot(stree(100, "right"))

install.packages("ape", repos="http://cran.r-project.org") ## problem

read.delim()

x <- 3
x
print (x)
y <- 4
x+y

height <- 1.73
weight <- 58
bmi <- weight/(height)^2
print (bmi)
bmi

5%%6

1>2
2==1
2!=1
2!=1|2<1
2!=1&2<1
y>x

all(c(1==1, 2==3, 3>1))
all(c(1==1, 2==2, 3>1))
any(c(1==1, 2==3, 3>1))

x <- 1
y <- 2.1
x2 <- "1"
x+1
x2+1
z<- "metabolomics"
as.integer(x)
as.character(x)
typeof(x)
typeof(x2)

c(5,6,7,8,9,23,4)
(v <- 5:13)
(v <- seq(5,9, by = 0.5))
(s <- c(1, 3, 7:12))
(s <- c('apple', 'red', 5, 'true'))
t <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
(u <- t[c(1,7,4)])  ##show subset, like QC samples
(x <- t[c(-7,-4)])  ##remove the subset
#or
(v <- t[c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE)])

cpds <- c("xx_a", "xx_b", "xx_c", "xx_d", "yy_a", "yy_b") ##application: recognize iSTD, z_iSTD
grep("^xx",cpds)
cpds[-grep("^xx, cpds")]
length(cpds[-grep("^xx", cpds)])
cpds[grep("^xx",cpds)]
length(cpds[grep("^xx",cpds)])

v3 <- 1:7
v4 <- 2:8
union(v3, v4)
intersect(v3, v4)
setdiff(v3, v4)
setdiff(v4, v3)
serequal(v3, v4)


v1 <- 1:7
v2 <- 2:8
add.result <- v1+v2 ##maths NOTe:vectors must have the same length

M <- matrix(c(5:19), nrow= 3, byrow = TRUE)
M
N <- matrix(c(5:19), nrow= 3, byrow = FALSE)
N
rownames <- c("row1","row2","row3")
colnames <- c("col1","col2","col3","col4","col5")
(p <- matrix(c(5:19), nrow = 3, byrow = TRUE, dimnames = list(rownames, colnames)))
print(p)
print(p[2,5])  ##note! row fist, then column
print(p[2,])
print(p[,3])
print(p[1:2, 3:5]) ##IMPT, to pull subset
(p[1:2, 3:5]) #another way to display

##Matrix can do maths,, but must have the same length
matrix1 <- matrix(c(3,9,-1,-4,2,6), nrow =2)
matrix1
matrix2 <- matrix(c(5,2,0,9,3,4), nrow = 2)
matrix2
matrix1 + matrix2
matrix1 - matrix2
matrix1 * matrix2
matrix1 / matrix2

#skipped matrix algebra

##list
list_data <- list(c("Jan","Feb","Mar"), TRUE,
                  matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))
list_data
myinfo <- list(first = "Tong", Last = "Shen",
               YOB = 2000, male = FALSE) #specify name of attributes
myinfo
names(list_data) <- c("Month","Logical","Matrix","Inner list 1")
myinfo[4]
myinfo$first
list_data[2]
list_data$Month
list_data$"Inner list 1"

#Creat a data frame
emp.data <- data.frame(
  emp_id = c(1:5),
  emp_name = c("Jim","Ivana","Jenny","Gert","Natasha"),
  salary = c(623.3, 515.2, 611.0, 729.0, 843.25),
  start_date = as.Date(c("2012-01-01","2013-09-23","2015-03-27","2014-11-15","2014-05-11")),
  stringsAsFactors = FALSE)
#always wannna stringsAsFactors = FALSE otherwise will convert character into factor
emp.data
head(emp.data,2) #print first 2 rows
summary(emp.data)
result <- data.frame(emp.data$salary,emp.data$emp_name)
emp.data$dept <- c("IT","HR","Finance","RD","EE") #add another column

#add row
emp.newdata <- data.frame(
  emp_id = c(6:8),
  emp_name = c("Tom","Megan","Michael"),
  salary = c(823.3, 815.2, 811.05),
  start_date = as.Date(c("2012-02-01","2013-02-23","2015-02-27")),
  dept = c("IT","HR","EE"),
  stringsAsFactors = FALSE)

emp.finaldata <- rbind(emp.data, emp.newdata)
emp.finaldata

##day 2, if for while
traffic_light <- "green"
if (traffic_light == "green") {
  print("Go.")
}

if (traffic_light == "green") {
  print("Go.")
} else {
  print("Stay.")
}

if (traffic_light == "green") {
  print("Go.")
} else if (traffic_light == "yellow") {
  print("Get ready.")
} else {
  print("Stay.")
}

traffic_light <- "yellow"

## for loop
for (i in 1:5){
  print(i^2)
}
for (k in 1:5){
  i <- k+4
  print(i^2)
}

##while look
i = 1
while(i <= 5) {
  cat("i=", i, "\n")
  i = i + 1
}

for(i in 1:10){
  if(i <= 5){
    print("Skip")
  next
  }
  Cat(i, "is greater than 5.\n")#####problem!!!
}

set.seed(12345)
(x <- matrix(rnorm(30), nrow = 5, ncol = 6))
apply(x, 2 ,sum)##comprehend!!

print(x)
apply(x, 2, function(y) length(y[y < 0]))


lapply(1:3, function(x) x^2)
sapply(1:3, function(x) x^2)

## sort and order
z.vec <- c(5,8,2,4,5)
sort(z.vec)
order(z.vec)
order(z.vec,decreasing = TRUE)
sort(z.vec,decreasing = TRUE)

z.vec <- c(5,3,4,2,5)
z.vec[order(z.vec)] ## comprehend,
### not only indicate the position, but also plot the values in that order

### missing value
z <- c(1:3, NA)
z
ind <- is.na(z)
ind

#check how many NA
all
any
sum

###import export data, import with read, export with write
getwd()
setwd("ddd/ddd")

mydata <- read.table("pat.csv",
                     header = TRUE, sep = ",")
write.table(mydata, "pat.csv", sep = "\t") #tab-delimited
write.table(mydata, "pat.csv", sep = ",") #comma-delimited
write.table(mydata, "pat.csv") #comma-delimited
#practice
data.raw <- read.csv("./Data/mx 107155 _lung cancer tissue_summer course_08-2015_submit.csv", header = F, stringsAsFactors = F)
head(data.raw)
data.raw[20,10]
data.raw[1:5, 8:12]
data.raw[c(11,5,6), 8:12]
data.raw[,1]
data.raw[11,]##which column is the inchikey? v8

cpd.names <- data.raw[,1]
cpd.names[1:20]

for (i in cpd.names){
  if(length(grep('^z ',i)>0) ){
    print(i)
  } ##grep gives a value ,lengh decides if the value is 0 or not, overall, to output all iSTD
}

#get the row number and name of iSTD
for (i in 1:length(cpd.names)) {
  if( length(grep('z ',cpd.names[i])>0)){
    print(c(i,cpd.names[i]))
  }
}

data.raw[1, grep("tumor",data.raw[10,] ) ]# to do what?

data.raw[1,grep("dda)]#not finished, p51. P52-56 is homework


z <- c(1:3,NA)
z
ind <- is.na(z)
ind
x <-z[!is.na(z)]
x
