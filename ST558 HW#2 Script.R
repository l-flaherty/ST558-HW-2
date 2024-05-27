##########Written By Liam Flaherty For ST558 HW2###########
####still need to fix knitr in pt 2###


#####1. Basic Vector Practice#####
pre_bp=c(130, 128, 116, 124, 133,              #data provided#
         134, 118, 126, 114, 127,
         141, 138, 128, 140, 137,
         131, 120, 128, 139, 135)
post_bp=c(114, 98, 113, 99, 107,               #data provided#
          116, 113, 111, 119, 117,
          101, 119, 130, 122, 106,
          106, 124, 102, 117, 113)

names(pre_bp)=paste0("Subject_",               #add names attribute#
                     1:length(pre_bp))   
names(post_bp)=paste0("Subject_",              #add names attribute#
                      1:length(post_bp)) 
 
diff_bp=post_bp-pre_bp                         #new vector for the difference#
mean(diff_bp)                                  #find the mean of the difference#

which(diff_bp<0)                               #subjects who experienced decrease# 
mean(diff_bp[which(diff_bp<0)])                #mean difference of those who decreased#





#####2. Basic Data Frame Practice#####
treatment=data.frame(                          #Put everything into a dataframe#
  patient=names(pre_bp),
  pre_bp, 
  post_bp,
  diff_bp)

treatment[which(treatment$diff_bp<0),]         #Filter the df to those who decreased#

treatment$low_bp=ifelse(treatment$post_bp<120, 
                        TRUE, FALSE)
#knitr::kable(treatment)#





#####3. List Practice#####
pre_bp=c(138, 135, 147, 117, 152,                       #overwriting previous vector#
         134, 114, 121, 131, 130)
post_bp=c(105, 136, 123, 130, 134,                      #overwriting previous vector#
          143, 135, 139, 120, 124)

placebo=data.frame(                                     #mimick treatment df#
  patient=paste0("Placebo_Subject_", 1:length(pre_bp)),
  pre_bp,
  post_bp,
  diff_bp=post_bp-pre_bp,                              
  low_bp=ifelse(post_bp<120, TRUE, FALSE))             

mylist=list(treatment=treatment, placebo=placebo)       #Place the df's together#

mylist[1]                                               #returns a list#
mylist[[1]]                                             #returns a dataframe#
mylist["treatment"]                                     #other way to access df in list#
mylist[-2]                                              #more ways to access df in list#

mylist[["placebo"]]["pre_bp"]                           #specific part of df from list#





#####4. Control Flow Practice#####
mylist[["treatment"]]$status=character(nrow(treatment))  #initialize#
mylist[["placebo"]]$status=character(nrow(placebo))      #initialize#

for(i in 1:nrow(mylist[["treatment"]])) {                #just to practice 'for'# 
  a=mylist[["treatment"]][["post_bp"]]                   #to make less cluttered#
  if(a[i]>130) {                                       
    mylist[["treatment"]][["status"]][i]="high"                                  
  } else if (a[i]>120) {                               
    mylist[["treatment"]][["status"]][i]="borderline"
  } else {
    mylist[["treatment"]][["status"]][i]="optimal"
  }
}

for(i in 1:nrow(mylist[["placebo"]])) {                   #just to practice 'for'#
  a=mylist[["placebo"]][["post_bp"]]                      #to make less cluttered#
  if(a[i]>130) {                                       
    mylist[["placebo"]][["status"]][i]="high"                                  
  } else if (a[i]>120) {                               
    mylist[["placebo"]][["status"]][i]="borderline"
  } else {
    mylist[["placebo"]][["status"]][i]="optimal"
  }
}





#####5. Function Writing#####
myfunction=function(l, func=mean) {       #list w/ 2 dfs, set default function to mean#
  df1=l[[1]]; df2=l[[2]]
  a=c("pre_bp", "post_bp", "diff_bp")     #assume same variable names in dfs#
  b=vector()                              #initialize#
  
  for (i in 1:length(a)) {                #apply function (no l/sappy yet)#
    b[(2*i)-1]=func(df1[,a[i]])           #no fear of hard-coding 2...#
    b[(2*i)]=func(df2[,a[i]])             #...since input list is 2dfs##
  }
  
  stat=deparse(substitute(func))          #get name of user inputted stat#
  
  names(b)=paste0(
    rep(c(names(l)), length(a)),          #names of df's in list#
    "_", rep(a,2), "_",                   #names of columns in df#
     stat)                                #name of desired stat#
                
return(b)                                 #directions say named vector instead of list okay#
}

myfunction(mylist)                        #testing to see defaults work#
myfunction(mylist, var)                   #testing various functions#
myfunction(mylist, sd)
myfunction(mylist, min)
myfunction(mylist, max)
