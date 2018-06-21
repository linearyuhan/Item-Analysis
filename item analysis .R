rm(list = ls())
kidaptive = read.csv('final_data.csv', header = TRUE)
dim(kidaptive)
install.packages(c('dplyr', 'ggplot2', 'lme4', 'multcomp'))


#select MATH only
math = dplyr::filter(kidaptive, subject_code %in% 'MATH')
math = math[,-1]
dim(math)
#no missing values
sum(is.na(math))
summary(math)
str(math)
#change the str of the variables
#math$learner_id = as.factor(math$learner_id)

#change variables to factors
cols <- c("learner_id", "edition_seq", "toc_seq", "line_up", "attempt_count", "learner_birthday")
math[cols] = lapply(math[cols], factor)

#explore each variable 211 unique learners 
unique(math$learner_id)

#explore genders  NEED TO ADD COUNT OF GENDERS ON THE PLOT and CHANGE COLOR OF HISTGRAM
#wrong because students gender overlap
female = math %>% filter(learner_gender == 'female') %>% distinct(learner_id) 
male = math %>% filter(learner_gender == 'male') %>% distinct(learner_id) 

#age range from 9-11,(however in korea, when you are born, you are counted as 1 instead of 0)
old = math %>% filter(learner_birthday == '2006') %>% distinct(learner_id) #only 1
mid = math %>% filter(learner_birthday == '2008') %>% distinct(learner_id)  #205
young = math %>% filter(learner_birthday == '2009') %>% distinct(learner_id) #5

#attemp
ggplot(math) +
  geom_bar(aes(x = math$attempt_count)) + 
  facet_wrap(~edition_seq) +
  xlab("Edition") +  
  ylab("Count") + 
  ggtitle("Attemp in each edition")

#15086= Division (1) 595859 = Division (2) 595860 = Division (3) 15100 = Formation test (2)
ggplot(data = math) +
  geom_bar(aes(x = edition_seq, fill = outcome))+ 
  xlab("Edition") +  ylab("Count") + ggtitle("Edition Sequence")

# first attempt accuracy for each edition
first_attemp = math[which(math$attempt_count == 1),]
ggplot(data = first_attemp) +
  geom_bar(aes(x = edition_seq, fill = first_attemp$outcome))+ 
  facet_wrap(~first_attemp$learner_gender)+ 
  xlab("Edition") +  ylab("Count") + ggtitle("Edition Sequence")

#############DATE 
first_attemp$event_time = as.Date(first_attemp$event_time)
weekdays <- weekdays(first_attemp$event_time)
pie(table(weekdays), main="activity")
pie(table(first_attemp$learner_birthday), main = 'Age')

#######EIDTION LEVEL
dv1_test_ed = math[which(math$edition_seq == "15086" & math$attempt_count ==1),]
right_answer_1 = dv1_test_ed %>% filter(outcome == 'true') #6011

dv2_test_ed = math[which(math$edition_seq == "595859" & math$attempt_count ==1),]
right_answer_2 = dv2_test_ed %>% filter(outcome == 'true') #9586

dv3_test_ed = math[which(math$edition_seq == "595860" & math$attempt_count ==1),]
right_answer_3 = dv3_test_ed %>% filter(outcome == 'true') #9900

for_test_ed = math[which(math$edition_seq == "15100" & math$attempt_count ==1),]
right_answer_4 = for_test_ed %>% filter(outcome == 'true') #6273
accuracy_dv1test_ed = 6011/8972
accuracy_dv2test_ed = 9586/14094
accuracy_dv3test_ed = 9900/13323
accuracy_formtest_ed = 6273/9483

accuracy.comp = data.frame(c(accuracy_dv1test_ed, accuracy_dv2test_ed, accuracy_dv3test_ed, accuracy_formtest_ed))
colnames(accuracy.comp) = "accuracy.rate"
Editions = c('DV1', 'DV2', "DV3", 'Formation')
accuracy.comp = cbind(accuracy.comp, Editions)

ggplot(accuracy.comp, aes(x = accuracy.comp$Editions, y = accuracy.comp$accuracy.rate))+
  geom_point() + ggtitle('Average Accuracy by edition') + xlab('edition') + ylab('accuracy')

#########MIX EFFECT MODEL1
model = glmer(outcome ~ edition_seq + (1|learner_id),data = first_attemp, family = binomial)
summary(model)
contrasts(first_attemp$outcome)

#plot
tmp <- as.data.frame(confint(glht(model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

#########MODEL2
model2 = glmer(outcome ~ learner_gender + (1|learner_id),data = first_attemp, family = binomial)
summary(model2)

tmp2 <- as.data.frame(confint(glht(model2))$confint)
tmp2$Comparison <- rownames(tmp2)
ggplot(tmp2, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()

#######################HOWS THAT ONE STUDENT DOING?
#15086= Division (1) 595859 = Division (2) 595860 = Division (3) 15100 = Formation test (2)

old_dv1 = math[which(math$edition_seq == "15086" & math$attempt_count ==1 & math$learner_birthday == '2006'),]
old_right_dv1 = old_dv1 %>% filter(outcome == 'true') #34
dim(old_dv1)
old_dv1_acc = 34/45

old_dv2 = math[which(math$edition_seq == "595859" & math$attempt_count ==1 & math$learner_birthday == '2006'),]
old_right_dv2 = old_dv2 %>% filter(outcome == 'true') #28
dim(old_dv2)
old_dv2_acc = 28/72

old_dv3 = math[which(math$edition_seq == "595860" & math$attempt_count ==1 & math$learner_birthday == '2006'),]
old_right_dv3 = old_dv3 %>% filter(outcome == 'true') #35
dim(old_dv3)
old_dv3_acc = 35/68

old_form = math[which(math$edition_seq == "15100" & math$attempt_count ==1 & math$learner_birthday == '2006'),]
old_right_form = old_form %>% filter(outcome == 'true') #22
dim(old_form)
old_form_acc = 22/70

##########WHAT ABOUT THE YOUNG KIDS?

young_dv1 = math[which(math$edition_seq == "15086" & math$attempt_count ==1 & math$learner_birthday == '2009'),]
young_right_dv1 = young_dv1 %>% filter(outcome == 'true') #189
dim(young_dv1)
young_dv1_acc = 189/248

young_dv2 = math[which(math$edition_seq == "595859" & math$attempt_count ==1 & math$learner_birthday == '2009'),]
young_right_dv2 = young_dv2 %>% filter(outcome == 'true') #286
dim(young_dv2)
young_dv2_acc = 286/360

young_dv3 = math[which(math$edition_seq == "595860" & math$attempt_count ==1 & math$learner_birthday == '2009'),]
young_right_dv23= young_dv3 %>% filter(outcome == 'true') #275
dim(young_dv3)
young_dv3_acc = 275/332

young_form = math[which(math$edition_seq == "15100" & math$attempt_count ==1 & math$learner_birthday == '2009'),]
young_right_form = young_form %>% filter(outcome == 'true') #153
dim(young_form)
young_form_acc = 153/227


accuracy_young = data.frame(c(young_dv1_acc, young_dv2_acc, young_dv3_acc, young_form_acc))
colnames(accuracy_young) = "accuracy.rate"
rownames(accuracy_young) = c('dv1', 'dv2', 'dv3', 'form')

plot(accuracy_old$accuracy.rate, type = 'l', col = 'red', xlab = "edition", ylab = 'accuracy', ylim = c(0.3, 0.9))
accuracy_old = data.frame(c(old_dv1_acc, old_dv2_acc, old_dv3_acc,old_form_acc))
colnames(accuracy_old) = "accuracy.rate"
rownames(accuracy_old) = c('dv1', 'dv2', 'dv3', 'form')
lines(accuracy_young$accuracy.rate, col = 'green')
lines(accuracy.comp$accuracy.rate, col = 'black')
legend(1, 0.45, c('2009', 'Average', '2006'), col = c('green', 'black', 'red'), 
       lty= c(1,1), lwd = c(0.5,0.5), cex = 0.5)


###############ITEM ANALYSIS 
dv1_test = math[which(math$toc_seq == "200305" & math$attempt_count ==1),]
dv1_test_sub = dv1_test[, c('learner_id', "line_up", "outcome")]

#learner 132 only has 9 items. maybe he didnt finish all the questions 
groupby_learner = dv1_test_sub %>% group_by (learner_id) %>% summarise(n = n())


id_list =unique(dv1_test_sub$learner_id)
item_list = rev(unique(dv1_test_sub$line_up))
n = length(id_list)
m = length(item_list)

division_test = data.frame(matrix(NA, nrow=n, ncol=m, byrow = TRUE),row.names = id_list)
colnames(division_test) = paste('Item', item_list, sep = '.')


for (i in 1:n){
  dat_vec = rev(as.vector(dv1_test_sub$outcome[dv1_test_sub$learner_id == id_list[i]]))
  if(length(dat_vec) == 10){ 
    division_test[i,] = dat_vec
  } 
  else { 
    value = setdiff(c(1:10), dat_vec)
    dat_vec_new = append(dat_vec, 'false', after = value-1)
    division_test[i,] = dat_vec_new
  }
}

division_test[division_test == 'false'] = 0
division_test[division_test == 'true'] = 1
str(division_test)

#Devision_test_df= sapply(division_test, as.numeric)

itemnames = c(names(division_test))

#change to numeric
division_test[itemnames] = lapply(division_test[itemnames], as.numeric)
Devision_test_df = division_test


#commpute total score
Devision_test_df$Score <- apply(Devision_test_df, 1, sum)
#inter-item correlation
nItems <- ncol(Devision_test_df[,-1])
IIC <- cor(Devision_test_df)
AIIC <- sapply(1:nItems, FUN = function(x) mean(IIC[-c(x, nrow(IIC)), x]))
#write.csv(AIIC, "AIIC.csv")


#item difficulty
nItems = ncol(Devision_test_df)
P <- apply(Devision_test_df[, 1:10], 2, mean)
#write.csv(P, 'p.csv')

#item reliability index 
iri <- apply(Devision_test_df[, 1:nItems], 2, sd) * IIC[1:nItems, nItems]

#give each student a total score

tblScores <- table(Devision_test_df$Score)
ggplot(Devision_test_df, aes(Score)) + geom_bar() + labs(x = "Total Score", title = "Distribution of Total Scores") + stat_function(fun = function(x) {
  length(Devision_test_df$Score) * dnorm(x = x, mean = mean(Devision_test_df), sd = sd(Devision_test_df$Score))}, color = "red")

#descirptibe stats of all the items and total score. the last row is total score. 
tvDescriptives <- function(x) {
  return(c(Mean = mean(x, na.rm = TRUE), p.adj = mean(x, na.rm = TRUE) + 
             (mean(x, na.rm = TRUE)/4), Variance = var(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Minimum = min(x, na.rm = TRUE), Maximum = max(x, na.rm = TRUE), N = sum(!is.na(x)), 
           Missing = sum(is.na(x))))
}
(dfDescriptives<- data.frame(do.call(rbind, lapply(1:ncol(Devision_test_df), FUN = function(iCol) tvDescriptives(Devision_test_df[,iCol])))))
write.csv(dfDescriptives, 'dfDescriptives.csv') 

###################yayyyyyyy


###useful package 
install.packages('psychometric')
library(psychometric)
nrow(division_test)
colMeans(Devision_test_df,na.rm=TRUE)
#gives pretty much everything
item.exam(Devision_test_df, y = NULL, discrim = FALSE)


# item_names = c(names(division_test))
# division_test[item_names] = lapply(division_test[item_names], factor)


# for (k in 1:nrow(division_test)){
#   for(l in 1:ncol(division_test)){
#     if (division_test[k,l] == 'true'){
#       division_test[k,l] = 1
#     }
#     else{
#       division_test[k,l] = 0
#     }
#   }
# }

#cannot compare them across the different edition because they test drastically differerent concept 
summary(math$toc_seq)
#in each edition, how many of the items are tested
dv1 = math[which(math$edition_seq == "15086"),]
dv1_no_attemp = dv1 %>% filter(dv1$attempt_count == 0)
dv1_one_attemp = dv1 %>% filter(dv1$attempt_count == 1)
#is there a case that second attempt generates false
#do data analysis 

length(Devision_test_df[which(Devision_test_df$Score == 10),])
outlier = math[which(math$learner_id == 132 & math$edition_seq == 15086),]
