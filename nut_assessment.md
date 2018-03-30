---
title: "Nutritional Assessment"
author: "Analysis by Nutan Sahoo"
date: "29 September 2017"
output:
  pdf_document: default
  html_document: default
---


##Exploratory Analysis

```r
#read in the data
nut<- read.csv("nutrition.csv")
head(names(nut))
```

```
##  [1] "name"                                                                 
##  [2] "age"                                                                  
##  [3] "height"                                                               
##  [4] "sex"                                                                  
##  [5] "gvt.pvt"                                                              
##  [6] "school..area"                                                         
##  [7] "fathers.occupation"                                                   
##  [8] "fathers.education"                                                    
##  [9] "monthly.income"                                                       
## [10] "Total.Score"                                                          
## [11] "mothers.occupation"                                                   
## [12] "education.mother"                                                     
## [13] "family.member"                                                        
## [14] "address"                                                              
## [15] "weight"                                                               
## [16] "thigh"                                                                
## [17] "waist"                                                                
## [18] "hip"                                                                  
## [19] "chest.breast"                                                         
## [20] "BMI"                                                                  
## [21] "Do.you.take.breakfast.before.coming.to.school.in.the.morning."        
## [22] "if.yes.what.do.you.take."                                             
## [23] "If.not.always.how.often.do.you.take.breakfast.before.coming.to.school"
## [24] "If.not.always.why."                                                   
## [25] "A"                                                                    
## [26] "y"                                                                    
## [27] "C"                                                                    
## [28] "cereals"                                                              
## [29] "Vitamin.A.rich.veg."                                                  
## [30] "White.tubers.and.roots"                                               
## [31] "Dark.green.leafy.veg"                                                 
## [32] "Vitamin.A.rich.fruits"                                                
## [33] "Other.Fruits"                                                         
## [34] "Organ.meat..iron.rich."                                               
## [35] "Flesh.meats"                                                          
## [36] "fish"                                                                 
## [37] "legumes.nuts.and.these.seeds"                                         
## [38] "milk.and.milk.products"                                               
## [39] "eggs"                                                                 
## [40] "oils.and.fats"                                                        
## [41] "sweets16"                                                             
## [42] "coffee.tea.n.milk"                                                    
## [43] "cereals.1"                                                            
## [44] "legumes"                                                              
## [45] "fruits"                                                               
## [46] "vegetables"                                                           
## [47] "roots.and.tubers"                                                     
## [48] "animal.products"                                                      
## [49] "roof"                                                                 
## [50] "wall"                                                                 
## [51] "floor"                                                                
## [52] "how.many.rooms"                                                       
## [53] "what.are.the.two.main.sources.of.energy.for.lighting"                 
## [54] "two.main.sources.f.energy.for.cooking"                                
## [55] "do.you.treat.your.drinking.water"                                     
## [56] "which.method.use"                                                     
## [57] "at.what.times.do.you.wash.your.hands."                                
## [58] "sick"                                                                 
## [59] "disease"
```

```r
dim(nut)
```

```
## [1] 279  59
```
There are many columns. But we only need the first 13 coulumns for now. Let's have a look at the first 13 columns.

```r
str(nut[ ,2:13])
```

```
## 'data.frame':	279 obs. of  12 variables:
##  $ age               : int  11 12 14 12 14 14 13 13 13 13 ...
##  $ height            : num  4.8 4.7 4.11 4.11 4.11 5.1 4.1 4.8 4.9 4.11 ...
##  $ sex               : Factor w/ 2 levels "f","m": 1 1 1 1 1 1 1 1 1 1 ...
##  $ gvt.pvt           : Factor w/ 2 levels "gvt","pvt": 1 1 1 1 1 1 1 1 1 1 ...
##  $ school..area      : Factor w/ 27 levels "continental public school",..: 22 26 26 26 26 26 26 26 26 26 ...
##  $ fathers.occupation: Factor w/ 9 levels "profession","semi-profession",..: 2 6 8 4 6 6 8 6 6 6 ...
##  $ fathers.education : Factor w/ 15 levels "10th","10th ",..: 5 5 13 1 5 15 1 1 5 1 ...
##  $ monthly.income    : Factor w/ 44 levels "10,000","10,500",..: 6 1 18 5 13 15 11 1 11 11 ...
##  $ Total.Score       : logi  NA NA NA NA NA NA ...
##  $ mothers.occupation: Factor w/ 34 levels " H.w","Agan badi ",..: 16 16 16 16 26 16 28 16 16 16 ...
##  $ education.mother  : Factor w/ 14 levels "10th","11th",..: 10 7 1 7 1 3 6 12 1 10 ...
##  $ family.member     : int  5 5 5 4 5 6 5 6 5 5 ...
```
Father's occupation was classified under profession, semi-profession, skilled, semi-skilled, unskilled, shop (who owns a shop) and unemployed using standard classification tables provided by the government of India. But the first level of `fathers.occupation` is shown as music teacher, we classify it under one of the categories. `fathers.education` also shows 15 levels, first two are shown as 10th and 10th. We need to do some data cleaning.

```r
#check the row number which contains "music teacher" as father's occ.
which(nut$fathers.occupation=="music teacher")
nut[89,6]<- "semi-profession" #teaching jobs are classifed under semi profession
```


```r
which(nut$fathers.occupation=="semiskilled")  #4th row
nut[89,6]<- "semi-skilled"
```



```r
which(nut$fathers.occupation=="worker")  #265th row
nut[89,6]<- "semi-skilled"
```



```r
#we need to make similar corrections in father's education 
levels(nut$fathers.education)
#[1] "10th"        "10th "       "11th"        "11th "       "12th"        "12th "       "4th"        
#[8] "5th"         "6th"         "7th"         "8th"         "9th"         "bsc nursing" "pg"         
#[15] "ug"         
nut[nut$fathers.education=="10th ", 7]<- "10th"
```


```r
nut[nut$fathers.education=="11th ", 7]<- "11th"
```


```r
nut[nut$fathers.education=="12th ", 7]<- "12th"
```


```r
nut[nut$fathers.education=="bsc nursing", 7]<- "ug"
```


We have made the required corrections. In the `Total.Score` column we will fill scores on the basis of the education and occupation of the head and monthly income of the family by using the Modified Kuppuswamy socio-economic status scale. 

```r
#assigning values on the basis of occupation
nut$score1[nut$fathers.occupation=="profession"]<- 10
nut$score1[nut$fathers.occupation=="semi-profession"]<- 6
nut$score1[nut$fathers.occupation=="shop"]<- 5
nut$score1[nut$fathers.occupation=="skilled"]<- 4
nut$score1[nut$fathers.occupation=="semi-skilled"]<- 3
nut$score1[nut$fathers.occupation=="unskilled"]<- 2
nut$score1[nut$fathers.occupation=="unemployed"]<- 1

#assigning score on the basis of education
nut$score2[nut$fathers.education=="4th"| nut$fathers.education== "5th"]<- 2
nut$score2[nut$fathers.education=="6th"| nut$fathers.education== "7th"| 
                      nut$fathers.education== "8th"]<- 3
nut$score2[nut$fathers.education=="9th"| nut$fathers.education== "10th"| 
          nut$fathers.education== "11th"|nut$fathers.education== "12th"]<- 4
nut$score2[nut$fathers.education=="ug"| nut$fathers.education== "pg"]<-6
which(is.na(nut$score2)) #check if all the cells have been filled in
```

```
## [1]   3  34  41  43 187 231
```

```r
#assigning values on the basis of monthly family income
head(nut$monthly.income)
```

```
## [1] 13,500 10,000 20,000 13,000 17,000 18,000
## 44 Levels: 10,000 10,500 11,000 12,000 13,000 13,500 13,800 ... 9,800
```

```r
nut$monthly.income<- as.character(nut$monthly.income)
nut$monthly.income<- as.numeric(gsub(",","",nut$monthly.income))

hist(nut$monthly.income) #look at the distribution of salaries
```

![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-55-1.png)

```r
nut$score3[nut$monthly.income > 41430]<- 12
nut$score3[nut$monthly.income > 20715 & nut$monthly.income < 41429]<- 10
nut$score3[nut$monthly.income > 15536 & nut$monthly.income < 20714]<- 6
nut$score3[nut$monthly.income > 10357 & nut$monthly.income < 15535]<- 4
nut$score3[nut$monthly.income > 6214 & nut$monthly.income < 10356]<- 3
nut$score3[nut$monthly.income > 2092 & nut$monthly.income < 6213]<- 2
nut$score3[nut$monthly.income < 2091]<- 1

#calculating final score
class(nut$score1)
```

```
## [1] "numeric"
```

```r
class(nut$score2)
```

```
## [1] "numeric"
```

```r
class(nut$score3)   #make sure that the class is numeric before adding
```

```
## [1] "numeric"
```

```r
nut$Total.Score<- nut$score1+nut$score2+nut$score3
```

It contains commas as thousand seperator, we need to remove it for R to interpret it as numeric.
we can fix this by converting it to character and then using `gsub` and `as.numeric` to convert it to numeric w/o commas. Majority of the people's income lie in the range of 5000 to 20000.
After calculating the final scores we assign them the socio-economic classes according to modified kuppuswamy scale.

```r
nut$class[nut$Total.Score>=26 & nut$Total.Score<=29]<- "Upper"
nut$class[nut$Total.Score>=16 & nut$Total.Score<= 25]<- "Upper middle"
nut$class[nut$Total.Score>=11 & nut$Total.Score<=15]<- "Lower middle"
nut$class[nut$Total.Score>=5 & nut$Total.Score<=10]<- "Upper lower"
nut$class[nut$Total.Score<5]<- "Lower"

table(nut$class)
```

```
## 
## Lower middle        Upper  Upper lower Upper middle 
##          113            5           96           56
```

```r
prop.table(table(nut$class))
```

```
## 
## Lower middle        Upper  Upper lower Upper middle 
##   0.41851852   0.01851852   0.35555556   0.20740741
```

```r
#Displaying this information through a barplot
barplot(table(nut$class), col = rainbow(4), main = "Division of Students on the basis of Socio-Economic Class", ylim = c(0, 130))
```

![plot of chunk unnamed-chunk-56](figure/unnamed-chunk-56-1.png)
Majority of the students belong to the "upper lower" and "lower middle" socio-economic class.

**************
A new column named `height` was added in the csv file. We will read that file into R as a different name and `cbind` it with our current data set named `nut`. `Weight` had many NAs, but all the values have been filled in now. Let's replace this `weight` column which had NAs with the new one.

```r
nut1<- read.csv("nutrition.csv", header = TRUE)
head(nut1,3)
```

```
##           name age height sex gvt.pvt        school..area
## 1      srishti  11   4.80   f     gvt school campus noida
## 2 janki mishra  12   4.70   f     gvt         ssbm school
## 3      rashmi1  14   4.11   f     gvt         ssbm school
##   fathers.occupation fathers.education monthly.income Total.Score
## 1    semi-profession              12th         13,500          NA
## 2            skilled              12th         10,000          NA
## 3          unskilled       bsc nursing         20,000          NA
##   mothers.occupation education.mother family.member address weight thigh
## 1                H.w              8th             5   noida   32.4  13.5
## 2                H.w              5th             5   noida   27.0  13.0
## 3                H.w             10th             5   noida   34.6  14.5
##   waist  hip chest.breast BMI
## 1  27.0 29.0           28  NA
## 2  23.0 27.5           24  NA
## 3  27.5 31.0           28  NA
##   Do.you.take.breakfast.before.coming.to.school.in.the.morning.
## 1                                                         daily
## 2                                                         daily
## 3                                                         daily
##   if.yes.what.do.you.take.
## 1             tea biscuits
## 2                tea bread
## 3           tea roti sabji
##   If.not.always.how.often.do.you.take.breakfast.before.coming.to.school
## 1                                                                 daily
## 2                                                                 daily
## 3                                                                 daily
##   If.not.always.why. A y C cereals Vitamin.A.rich.veg.
## 1          no reason n y 1       y                   y
## 2          no reason n y 2       y                   y
## 3          no reason n y 2       y                   n
##   White.tubers.and.roots Dark.green.leafy.veg Vitamin.A.rich.fruits
## 1                      n                    y                     y
## 2                      n                    y                     y
## 3                      n                    y                     y
##   Other.Fruits Organ.meat..iron.rich. Flesh.meats fish
## 1            y                      n           n    n
## 2            y                      n           n    n
## 3            y                      y           y    y
##   legumes.nuts.and.these.seeds milk.and.milk.products eggs oils.and.fats
## 1                            y                      y    n             y
## 2                            y                      y    n             y
## 3                            y                      y    y             y
##    sweets16 coffee.tea.n.milk cereals.1 legumes fruits vegetables
## 1        no               tea     daily   daily      7          7
## 2 choclates               tea     daily   daily      7          7
## 3    sweets               tea     daily   daily      5          5
##   roots.and.tubers animal.products roof wall floor how.many.rooms
## 1                1           daily  a-5  b-2   c-2              1
## 2                1           daily  a-5  b-2   c-2              1
## 3                0           daily  a-1  b-3   c-2              1
##   what.are.the.two.main.sources.of.energy.for.lighting
## 1                                                   bd
## 2                                                   bd
## 3                                                   bd
##   two.main.sources.f.energy.for.cooking do.you.treat.your.drinking.water
## 1                                     d                                y
## 2                                     d                                y
## 3                                     d                                y
##   which.method.use at.what.times.do.you.wash.your.hands. sick      disease
## 1                b                                     h    y g(chikenpox)
## 2                b                                     h    y       g(UTI)
## 3                b                                     h    n           no
```

```r
#height is in the third column
nut<- cbind(nut, nut1$height)
#weight is in the fifteenth column
nut$weight<-nut1$weight
```
Since, height is in feet and inches we need to convert it to metres to calculate the BMI. The whole part in `nut1$height` variable is in feet and the decimal part is in inches. So we will split the whole and decimal part and then convert them to metres separately.

```r
whole<- floor(nut$`nut1$height`)
dec<- nut$`nut1$height`-whole

#converting them into metres
h1<- whole*0.305
h2<- dec*0.0254
nut$height<- h1+h2
```


```r
#calculating BMI

nut$BMI<- nut$weight/(nut$height)^2
which(is.na(nut$BMI))  #No cell is blank if we see integer(0) 
```

```
## integer(0)
```

```r
#classifications of over and underweight on the basis of BMI
nut$prev[nut$BMI< 18.5]<- "Underweight"
nut$prev[nut$BMI>= 18.5 & nut$BMI<= 24.9]<- "Normalweight"
nut$prev[nut$BMI>= 25 & nut$BMI<= 29.9]<- "Overweight"
nut$prev[nut$BMI>= 30]<- "Obese"
which(is.na(nut$prev)) #these are blank
```

```
## [1]  69  94 101
```

```r
#let's look at the BMI of blank entries
which(is.na(nut$BMI))
```

```
## integer(0)
```

```r
nut$BMI[69]
```

```
## [1] 24.94608
```

```r
nut$BMI[94]
```

```
## [1] 24.93038
```

```r
nut$BMI[101]
```

```
## [1] 24.94418
```

```r
#manually categorise them
nut$BMI[69]<-"Overweight"
nut$BMI[94]<-"Overweight"
nut$BMI[101]<-"Overweight"
which(is.na(nut$BMI))
```

```
## integer(0)
```

```r
#barplot to see the distribution of children in different weight categories
table(nut$prev)
```

```
## 
## Normalweight        Obese   Overweight  Underweight 
##          110           38           31           97
```

```r
barplot(table(nut$prev), col = rainbow(4), main = "distribution of children in different weight categories", ylim = c(0, 120))
```

![plot of chunk unnamed-chunk-59](figure/unnamed-chunk-59-1.png)
`prev` contains the classifications of over and underweight on the basis of BMI. Now, that we have successfully cateforised into diff. weight categories. We will now perform chi-square tests to check if the prevalance of underweight and overweight is dependent on Gender or Socio-economic statuses or the type of school they attend (pvt or govt) or the tyoe of area they live in.


```r
#to test the dependence b/w prevalance of underweight, overweight and gender
tbl1<- table( nut$sex, nut$prev)
tbl1
```

```
##    
##     Normalweight Obese Overweight Underweight
##   f           65    23         16          40
##   m           45    15         15          57
```

```r
#performing chi-square test of independence
chisq.test(tbl1)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl1
## X-squared = 7.8253, df = 3, p-value = 0.04976
```
The Chi-Square test of Independence is used to determine if there is a significant relationship between two nominal (categorical) variables.  The frequency of one nominal variable is compared with different values of the second nominal variable.  The data can be displayed in an R*C contingency table, where R is the row and C is the column as shown in the tbl1 . We take the level of significance as standard 0.05. The p-value of the above is just smaller than 0.05. Hence, we reject the null hypothesis that prevalance of over and underweight is independent of gender. In our survey we see that greater number of females are obese and overweight as seen in contingency table above. 

```r
barplot(tbl1, beside = TRUE , legend.text = c("Female", "Male"))
```

![plot of chunk unnamed-chunk-61](figure/unnamed-chunk-61-1.png)
We can see the distribution of students on the basis of Gender in the different weight categories.


```r
#to test the dependence b/w prevalance of underweight, overweight and socioeconomic class
tbl2<- table( nut$prev, nut$class)
tbl2
```

```
##               
##                Lower middle Upper Upper lower Upper middle
##   Normalweight           55     3          37           12
##   Obese                  14     0           9           15
##   Overweight             12     1          12            3
##   Underweight            31     1          36           26
```

```r
#performing chi-square test of independence
chisq.test(tbl2)
```

```
## Warning in chisq.test(tbl2): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl2
## X-squared = 22.924, df = 9, p-value = 0.006368
```

```r
barplot(tbl2, beside = TRUE , legend.text = c("Normalweight", "Obese", "Overweight", "Underweight"))
```

![plot of chunk unnamed-chunk-62](figure/unnamed-chunk-62-1.png)
The p-value is less than 0.05. We reject the null hypothesis that prevalance of under and overweight is independent of Socio-economic class. Hence, there is a very significant relationship between the socioeconomic class of students and their weight category in the areas we surveyed. A large no. of students belonging to Lower middle and Upper lower are in the category of underweight. A clearer picture can be seen in the barplot.


```r
#to test the dependence b/w prevalance of underweight, overweight and the type of school they attend
tbl3<- table(nut$gvt.pvt,  nut$prev)
tbl3
```

```
##      
##       Normalweight Obese Overweight Underweight
##   gvt           96    22         20          77
##   pvt           14    16         11          20
```

```r
#performing chi-square test of independence
chisq.test(tbl3)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl3
## X-squared = 17.795, df = 3, p-value = 0.0004848
```
The p-value is 0.0004848 which is much smaller than 0.05. We reject the null hypothesis that there is no relationship b/w the two categorical variables. By looking at the p-value we can say that there is a highly significant relationship b/w the kind of school (gvt/pvt) students attend and the weight category they belong to.


```r
barplot(tbl3, beside = TRUE , legend.text = c("Government School","Private School"), ylim = c(0, 120))
```

![plot of chunk unnamed-chunk-64](figure/unnamed-chunk-64-1.png)
There are more number no. of students of govt school in all the categories. One of the reasons can be that there were very few students in the upper and upper middle class. 












