
#STAT 4559 - Final

df <- read.csv('trump_speeches_v6.csv')
#http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

df$state.result
df$state.result <-as.factor(df$state.result)
df$city.county.type <-as.factor(df$city.county.type)


############average length############ 
colnames(df)


#average number of sentence (median)#

## by state

kruskal.test(X..of.sentences~state.result,df)
?kruskal.test

##by county
wilcox.test(df[df$city.county.type=='red','X..of.sentences'],
            df[df$city.county.type=='blue','X..of.sentences'],correct = T,exact=F)

##2 level
anova(lm(X..of.sentences ~ city.county.type+state.result,df),lm(X..of.sentences ~ city.county.type+state.result+city.county.type*state.result,df)) 
summary(lm(X..of.sentences ~ city.county.type+state.result,df))
summary(lm(X..of.sentences ~ city.county.type+state.result+city.county.type*state.result,df))



#average number of words (median)

##by state
kruskal.test(X..of.words~state.result,df)


##by county
wilcox.test(df[df$city.county.type=='red','X..of.words'],
            df[df$city.county.type=='blue','X..of.words'],correct = T,exact=F)

##2 level

anova(lm(X..of.words ~ city.county.type+state.result,df),lm(X..of.words ~ city.county.type+state.result+city.county.type*state.result,df)) 

summary(lm(X..of.words ~ city.county.type+state.result+city.county.type*state.result,df))



#average sentence length (median)#

##by state
kruskal.test(Average.Sentence.Length~state.result,df)

##by county
wilcox.test(df[df$city.county.type=='red','Average.Sentence.Length'],
            df[df$city.county.type=='blue','Average.Sentence.Length'],correct = T,exact=F)

## 2 level


anova(lm(Average.Sentence.Length ~ city.county.type+state.result,df),
      lm(Average.Sentence.Length ~ city.county.type+state.result+city.county.type*state.result,df)) 


############Topic Modeling############ 

#Entropy

##state level
kruskal.test(entropy~state.result,df) #significant!!!

library(FSA)
dunnTest(entropy~state.result,df, method="bh")
#REF:https://rcompanion.org/rcompanion/d_06.html


##county level
wilcox.test(df[df$city.county.type=='red','entropy'],
            df[df$city.county.type=='blue','entropy'],correct = T,exact=F)

wilcox.test(df[df$state.result=='swing-blue','entropy'],
            df[df$city.county.type=='blue','entropy'],correct = T,exact=F)
##2 level

med2way(entropy ~ city.county.type*state.result,
        data = df)
mcp2a(entropy ~ city.county.type*state.result,
      data = df)


anova(lm(entropy ~ city.county.type+state.result,df),
      lm(entropy ~ city.county.type+state.result+city.county.type*state.result,df)) 

summary(lm(entropy ~ city.county.type+state.result+city.county.type*state.result,df))


############Keywords############ 


#Immigration

##by state
kruskal.test(mention.immigration~state.result,df)

##by county - significant

wilcox.test(df[df$city.county.type=='red','mention.immigration'],
            df[df$city.county.type=='blue','mention.immigration'],correct = T,exact=F,alternative ='less')


##2 level
med2way(mention.immigration~ city.county.type*state.result,
        data = df)
anova(lm(mention.immigration ~ city.county.type+state.result,df),
      lm(mention.immigration ~ city.county.type+state.result+city.county.type*state.result,df)) 

summary(lm(mention.immigration ~ city.county.type+state.result+city.county.type*state.result,df))
summary(lm(mention.immigration ~ city.county.type+state.result,df))


#Econ
##by state
kruskal.test(mention.econ~state.result,df)
dunnTest(mention.econ~state.result,df,method="bh")


##by county
wilcox.test(df[df$city.county.type=='red','mention.econ'],
            df[df$city.county.type=='blue','mention.econ'],correct = T,exact=F)


##2 level
med2way(mention.econ~ city.county.type*state.result,
        data = df)


anova(lm(mention.econ ~ city.county.type+state.result,df),
      lm(mention.econ ~ city.county.type+state.result+city.county.type*state.result,df)) 
summary(lm(mention.econ ~ city.county.type+state.result+city.county.type*state.result,df))


#Minority

##by state
kruskal.test(mention.minority~state.result,df)

##by county
wilcox.test(mention.minority~city.county.type,df,correct = T,exact=F)

##2 level
med2way(mention.minority~ city.county.type*state.result,
        data = df)
anova(lm(mention.minority ~ city.county.type+state.result,df),
      lm(mention.minority ~ city.county.type+state.result+city.county.type*state.result,df)) 


#6. black
##by state
kruskal.test(mention.black~state.result,df)

##by county
wilcox.test(mention.black~city.county.type,df,correct = F,exact=F,alternative ='less')


wilcox.test(df[df$city.county.type=='red','mention.black'],
            df[df$city.county.type=='blue','mention.black'],correct = T,exact=F,alternative ='greater')

##2 level
med2way(mention.black~ city.county.type*state.result,
        data = df)

anova(lm(mention.black ~ city.county.type+state.result,df),
      lm(mention.black ~ city.county.type+state.result+city.county.type*state.result,df)) 

summary(lm(mention.black ~ city.county.type+state.result+city.county.type*state.result,df))
summary(lm(mention.black ~ city.county.type+state.result,df))


#Election

##by state - sig
kruskal.test(mention.election~state.result,df)
dunnTest(mention.election~state.result,df,method="bh")

##by county - sig
wilcox.test(mention.election~city.county.type,df,correct = F,exact=F,alternative ='less')
wilcox.test(df[df$city.county.type=='red','mention.election'],
            df[df$city.county.type=='blue','mention.election'],correct = T,exact=F,alternative ='greater')



##2 level
med2way(mention.election~ city.county.type*state.result,
        data = df)

anova(lm(mention.election ~ city.county.type+state.result,df),
      lm(mention.election ~ city.county.type+state.result+city.county.type*state.result,df)) 

summary(lm(mention.election ~ city.county.type+state.result+city.county.type*state.result,df))
summary(lm(mention.election ~ city.county.type+state.result,df))


#Healthcare


##by state

kruskal.test(mention.healthcare~state.result,df)
dunnTest(mention.healthcare~state.result,df,method="bh")

##by county
wilcox.test(mention.healthcare~city.county.type,df,correct = F,exact=F,alternative ='greater')


wilcox.test(df[df$city.county.type=='blue','mention.healthcare'],
            df[df$city.county.type=='red','mention.healthcare'],correct = T,exact=F,alternative ='less')


##2 level
med2way(mention.black~ city.county.type*state.result,
        data = df)

anova(lm(mention.healthcare ~ city.county.type+state.result,df),
      lm(mention.healthcare ~ city.county.type+state.result+city.county.type*state.result,df)) 

summary(lm(mention.election ~ city.county.type+state.result+city.county.type*state.result,df))
summary(lm(mention.election ~ city.county.type+state.result,df))

######################## 





