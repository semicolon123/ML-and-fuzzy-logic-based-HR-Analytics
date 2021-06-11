library(ggplot2)
library(frbs)
library(dplyr)
DataSet=read.csv("C:/Users/SWATI/Desktop/projects+libraries/MAJOR PROJECT/Hr.csv")
DataSet
head(DataSet)
str(DataSet)

round(prop.table(table(DataSet$placed))*100)


####INFERENTIAL ANALYSIS#####



stud_placed<-filter(DataSet,placed=="Yes")
stud_placed

stud_placed_going=filter(stud_placed,stud_placed$will_join_or_not==1)

tr <- function(a){
            ggplot(data =stud_placed_going , aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +    geom_density()
}
tr(stud_placed_going$tech_project)

ggplot(stud_placed_going, aes(x = factor('btech_per'), fill = factor(btech_per))) +
       geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
labs(title="BTech percentage") 

ggplot(stud_placed_going, aes(x = factor('twelfth_per'), fill = factor(twelfth_per))) +
       geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
labs(title="twelfth_per")  

ggplot(stud_placed_going, aes(x = factor('tenth_perc'), fill = factor(tenth_perc))) +
       geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
labs(title="tenth_perc")

tr(stud_placed_going$research_pprs)

tr(stud_placed_going$tech_internships)

ggplot(stud_placed_going, aes(x = factor('package'), fill = factor(package))) +
       geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y")+theme_bw()+
labs(title="package")

tr(stud_placed_going$dead_backlog)

tr(stud_placed_going$active_backlog)



####fuzzy implementation

library(sets)
sets_options("universe", seq(1, 100, 0.5))
variables <- set(
  techproj = fuzzy_partition(varnames = c(dry = 3, good = 4, wet = 5),sd = 5.0),
  btechper = fuzzy_partition(varnames = c(dry = 83, good = 78, wet = 73), sd = 3.0),
  twelfth = fuzzy_partition(varnames = c(dry = 97, good = 87,wet = 93), sd = 7.5),
  tenth= fuzzy_partition(varnames = c(dry = 93, good = 98, wet = 88), 
                             sd = 3.0),
  #1 means 0 research ppr
  researchppr = fuzzy_partition(varnames = c(dry = 1, good = 2, wet = 3), 
                             sd = 3.0),
  techintern = fuzzy_partition(varnames = c(dry = 2, good = 1, wet = 3), sd = 3.0),

  #1 means same state, 2 means different
  loc_diff=fuzzy_partition(varnames=c(first=1, second=2, other=0),sd=7.5),
  
  package1=fuzzy_partition(varnames=c(first=3, second=4, third=5),sd=7.5),
  
  joining = fuzzy_partition(varnames = c(bad = 40, ok = 65, perfect = 80),
                            FUN = fuzzy_cone, radius = 10)
)

# Fuzzy rules
rules <- set(
  #1
  fuzzy_rule(techproj %is% dry && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% first && package1 %is% first, joining %is% ok),
  #2
  fuzzy_rule(techproj %is% good && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% first && package1 %is% first, joining %is% ok),
  #3
  fuzzy_rule(techproj %is% wet && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% first && package1 %is% first, joining %is% ok),
  #4
  fuzzy_rule(techproj %is% dry && btechper %is% good && twelfth %is% good && tenth %is% good && researchppr %is% good && techintern %is% dry && loc_diff %is% second && package1 %is% second, joining %is% ok),
  #5
  fuzzy_rule(techproj %is% good && btechper %is% good && twelfth %is% good && tenth %is% good && researchppr %is% good && techintern %is% dry && loc_diff %is% second && package1 %is% second, joining %is% ok),
  #6
  fuzzy_rule(techproj %is% wet && btechper %is% good && twelfth %is% good && tenth %is% good && researchppr %is% good && techintern %is% dry && loc_diff %is% second && package1 %is% second, joining %is% ok),
  #7
  fuzzy_rule(techproj %is% dry && btechper %is% wet && twelfth %is% wet && tenth %is% wet && researchppr %is% wet && techintern %is% wet && loc_diff %is% first && package1 %is% third, joining %is% perfect),
  #8
  fuzzy_rule(techproj %is% dry && btechper %is% wet && twelfth %is% wet && tenth %is% wet && researchppr %is% wet && techintern %is% wet && loc_diff %is% second && package1 %is% third, joining %is% ok),
  #9
  fuzzy_rule(techproj %is% good && btechper %is% wet && twelfth %is% wet && tenth %is% wet && researchppr %is% wet && techintern %is% wet && loc_diff %is% first && package1 %is% third, joining %is% perfect),
  #10
  fuzzy_rule(techproj %is% good && btechper %is% wet && twelfth %is% wet && tenth %is% wet && researchppr %is% wet && techintern %is% wet && loc_diff %is% second && package1 %is% third, joining %is% ok),
  #11
  fuzzy_rule(techproj %is% wet && btechper %is% wet && twelfth %is% wet && tenth %is% wet && researchppr %is% wet && techintern %is% wet && loc_diff %is% first && package1 %is% third, joining %is% perfect),
  #12
  fuzzy_rule(techproj %is% wet && btechper %is% wet && twelfth %is% wet && tenth %is% wet && researchppr %is% wet && techintern %is% wet && loc_diff %is% second && package1 %is% third, joining %is% ok),
  #13
  fuzzy_rule(techproj %is% dry && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% second && package1 %is% first, joining %is% bad),
  #14
  fuzzy_rule(techproj %is% dry && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% second && package1 %is% third, joining %is% ok),
  #15
  fuzzy_rule(techproj %is% dry && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% first && package1 %is% second, joining %is% ok),
  #16
  fuzzy_rule(techproj %is% dry && btechper %is% dry && twelfth %is% dry && tenth %is% dry && researchppr %is% dry && techintern %is% good && loc_diff %is% first && package1 %is% third, joining %is% perfect),
  #17
  fuzzy_rule(techproj %is% good && btechper %is% good && twelfth %is% good && tenth %is% good && researchppr %is% good && techintern %is% dry && loc_diff %is% first && package1 %is% first, joining %is% bad),
  #18
  fuzzy_rule(techproj %is% good && btechper %is% good && twelfth %is% good && tenth %is% good && researchppr %is% good && techintern %is% dry && loc_diff %is% first && package1 %is% third, joining %is% perfect)  
)
model <- fuzzy_system(variables, rules)

print(model)
plot(model)

#example.1 <- fuzzy_inference(model, list(techproj = 5, btechper = 73,twelfth = 93, tenth=88,researchppr=3, techintern=3, loc_diff=1,package1=5))
example.2 <- fuzzy_inference(model, list(techproj = 5, btechper = 73,twelfth = 93, tenth=88,researchppr=3, techintern=3, loc_diff=2,package1=5))
example.3 <- fuzzy_inference(model, list(techproj = 4, btechper = 78,twelfth = 87, tenth=98,researchppr=2, techintern=2, loc_diff=1,package1=3))
#example.4 <- fuzzy_inference(model, list(techproj = 5, btechper = 82,twelfth = 90, tenth=96,researchppr=1, techintern=2, loc_diff=2,package1=5))

#gset_defuzzify(example.1, "largestofmax")
gset_defuzzify(example.2, "largestofmax")
gset_defuzzify(example.3, "largestofmax")
#gset_defuzzify(example.4, "largestofmax")

plot(example.1)
plot(example.2)
plot(example.3)
plot(example.4)


