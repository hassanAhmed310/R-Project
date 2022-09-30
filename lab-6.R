### 1- Loading Data
library(datasets)
data(mtcars)
str(mtcars)
summary(mtcars)


### 2- Extracting Information 
library(dplyr)
#the head of auto
print(head(filter(mtcars, am == '0')))

#the head of manual
print(head(filter(mtcars, am == '1')))

## TOP 10 CARS
#------- Method 1 ------------
# (a)Displacement
print(head(mtcars[order(-mtcars['disp']),], 10))

# (b)hp
print(head(mtcars[order(-mtcars['hp']),], 10))

# (c)drat
print(head(mtcars[order(-mtcars['drat']),], 10))

#------- Method 2 ------------
# (a)Displacement
print(rev(tail(mtcars[order(mtcars['disp']),], 10)))

# (b)hp
print(tail(mtcars[order(mtcars['hp']),], 10))

# (c)drat
print(tail(mtcars[order(mtcars['drat']),], 10))
#---------------------------

## Cars whose mpg above average
print(filter(mtcars, mpg > mean(mtcars$mpg)))

##Data visualization:

#Histograms >> for discrete values spread over a small range
#Box plots >> to provide a visual summary of a categorical variable
#Pie chart >> to show percentage of single numerical variables

#mpg
boxplot(mtcars$mpg, xlab = 'Miles/(US) gallon', main = 'mpg')
#cyl:
cyl <- table(mtcars$cyl)
percent<- round(100*cyl/sum(cyl), 1)
pielabels<- c('4', '6', '8')
pie(cyl,col = rainbow(length(cyl)), labels = percent , main = 'Pie of cyl', cex = 1)
legend("topright", pielabels, fill = rainbow(length(cyl)), cex = 1)
#disp:
boxplot(mtcars$disp, xlab = 'Displacement (cu.in.)', main = 'disp')
#hp:
boxplot(mtcars$hp, xlab = 'Gross horsepower', main = 'hp')
#drat:
boxplot(mtcars$drat, xlab = 'Rear axle ratio', main = 'drat')
#wt:
boxplot(mtcars$wt, xlab = 'Weight (1000 lbs)', main = 'wt')
#qsec:
boxplot(mtcars$qsec, xlab = '1/4 mile time', main = 'qsec')
##vs:
vs <- table(mtcars$vs)
percent<- round(100*vs/sum(vs), 1)
pielabels<- c('V-shaped', 'straight')
pie(vs,col = rainbow(length(vs)), labels = percent , main = 'Pie of vs', cex = 1)
legend("topright", pielabels, fill = rainbow(length(vs)), cex = 1)
#am:
am <- table(mtcars$am)
percent<- round(100*am/sum(am), 1)
pielabels<- c("automatic","manual")
pie(am,col = rainbow(length(am)), labels = (percent ) , main = 'Pie of am', cex = 1)
legend("topright", pielabels, fill = rainbow(length(am)), cex = 1)
#gear:
hist(mtcars$gear, xlab = 'Number of forward gears', main = 'gear', col = 'green')
#carb:
hist(mtcars$carb, xlab = 'Number of carburetors', main = 'carb', col = 'green')


## Boxplots for:
# disp
boxplot(mtcars$disp, xlab = 'Displacement (cu.in.)', main = 'disp')
print(quantile(mtcars$disp, probs = seq(0.25,0.75,by=0.25)))
#hp
boxplot(mtcars$hp, xlab = 'Gross horsepower', main = 'hp')
print(quantile(mtcars$hp, probs = seq(0.25,0.75,by=0.25)))

#qsec
boxplot(mtcars$qsec, xlab = '1/4 mile time', main = 'qsec')
print(quantile(mtcars$qsec, probs = seq(0.25,0.75,by=0.25)))

### 3- Distributions
# A
a_mean = mean(mtcars$wt)
a_sd = sd(mtcars$wt)
a_x = filter(mtcars, wt >= 3.4)$wt
a_ans = sum(dnorm(a_x, mean = a_mean, sd = a_sd)) / sum(dnorm(mtcars$wt, mean = a_mean, sd = a_sd))
print(a_ans)

#B
b_prob = length(filter(mtcars, am == '1')$am) / length(mtcars$am)
b_ans = pbinom(18, 32, b_prob)
print(b_ans)

#C
c_ans = pbinom(4, 12, 0.2)
print(c_ans)

### 4- Permutations and Combinations
library(gtools)

#A
#1
num = seq(0, 2, by = 1)
ans_3a_1 = permutations(n = 3, r = 3, v = num, repeats.allowed = T)
print(ans_3a_1)

#2
for(i in num)
{
  for(j in num)
  {
    for(k  in num)
    {
      cat(i, ' ', j, ' ', k, '\n')
    }
  }
}

#B
#1
total = nrow(combinations(n = 9, r = 3, v = 1:9, repeats.allowed = F))
numOfWays = nrow(combinations(n = 2, r = 1, v = 3:4, repeats.allowed = F))
ans_4b_1 = numOfWays / total
print(ans_4b_1)

#2
total = factorial(9) / (factorial(3) * factorial(9-3))
numOfWays = 2
ans_4b_2 = numOfWays / total
print(ans_4b_2)


















