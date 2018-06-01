#Note: Different models are compiled by different team member, thus each of us
#performs data preprocessing individually, 
#but follows the same rule as shown in the first part of the code.

#Loading Data Package
install.packages('AmesHousing')
library(AmesHousing)
library(reshape2)
library(ggplot2)

#First make a copy of the ames_raw dataset
amesdata = ames_raw[,]

#Create an empty DataFrame and input missing percentage
Missing <- data.frame(matrix(ncol = 1, nrow = dim(amesdata)[2]))
rownames(Missing) = colnames(amesdata)
colnames(Missing) = 'Missing Percentage'
for (i in rownames(Missing)){
  Missing[i,] = round(sum(is.na(amesdata[i]))/dim(amesdata)[1],3)
}

#Filter the missing value
subset(Missing, Missing[,1] > 0.5)

#View the missing values in every variable
Na_table = apply(amesdata,2,function(x) {sum(is.na(x))})

#Go through the variables one by one and make changes
#We just do it with the variables that are expected to be influencial

table(unique(amesdata$`MS SubClass`))
amesdata$`MS SubClass` <- as.factor(amesdata$`MS SubClass`)

table(unique(amesdata$`MS Zoning`))
amesdata$`MS Zoning` <- as.factor(amesdata$`MS Zoning`)

amesdata$Alley <- as.factor(amesdata$Alley)
amesdata$Alley <- ifelse(is.na(amesdata$Alley), "None", amesdata$Alley)
amesdata$Alley <- as.factor(amesdata$Alley)

table(amesdata$`Lot Shape`)
amesdata$`Lot Shape` <- as.factor(amesdata$`Lot Shape`)

amesdata$`Land Contour` <- as.factor(amesdata$`Land Contour`)

table(amesdata$Neighborhood)
amesdata$Neighborhood <- as.factor(amesdata$Neighborhood)
amesdata$`Condition 2` <- as.factor(amesdata$`Condition 2`)
amesdata$`Bsmt Cond` <- as.factor(amesdata$`Bsmt Cond`)
amesdata$`Bsmt Exposure` <- as.factor(amesdata$`Bsmt Exposure`)

amesdata$`Garage Cars` = as.factor(amesdata$`Garage Cars`)

#check the Na values in Total Bsmt SF variable
sum(is.na(amesdata$`Total Bsmt SF`))
#create dummy variable for missing value
amesdata$TotalBsmtSF.NA <- ifelse(is.na(amesdata$`Total Bsmt SF`), 1, 0)
#Replace Na value with 0
amesdata$`Total Bsmt SF`[is.na(amesdata$`Total Bsmt SF`)]<- 0
#Make sure this variable is treated as numeric
amesdata$`Total Bsmt SF` = as.numeric(amesdata$`Total Bsmt SF`)

amesdata$`MS Zoning`[is.na(amesdata$`MS Zoning`)]<- 0
amesdata$`Garage Cars`[is.na(amesdata$`Garage Cars`)]<- 0


#We made another copy of amesdata
#Replace Na with 0 value for numeric variables
#This is a temporary process just for plotting the heatmap
ames_copy = amesdata[,]
nums <- sapply(ames_copy, is.numeric)
ames_copy[,nums][is.na(ames_copy[,nums])] <- 0



#Insert Position Variable 'Position' and 'Distance'
# ames_raw$Position <- ames_raw$Neighborhood
# ames_raw$Distance <- ames_raw$Neighborhood
# 
# for (i in Positions$Name){
#   ames_raw$Position[ames_raw$Neighborhood== i] = as.numeric(Positions$Position[Positions$Name == i])
# }
# for (i in Positions$Name){
#   ames_raw$Distance[ames_raw$Neighborhood== i] = as.numeric(Positions$Distance[Positions$Name == i])
# }



#select all the numeric variables, delete the Order Variable
nums <- sapply(ames_raw, is.numeric)
df_num_variables = ames_raw[ , nums]
drops <- c("Order")
df_num_variables = df_num_variables[ , !(names(df_num_variables) %in% drops)]


#Create Heatmaps for numeric variables
cormat = round(cor(df_num_variables, method = "pearson", use = "complete.obs"),3)
melted_cormat <- melt(cormat)
HeatPlot = ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_gradient2(low = "#4A96AD", high = "#C63D0F", mid = "white", 
                       midpoint = 0, limit = c(-0.5,1), space = "Lab", 
                       name="Pearson\nCorrelation")

select_list = c('Lot Area',
                'Overall Qual',
                'Year Built', 
                'Year Remod/Add',
                '1st Flr SF',
                '2nd Flr SF',
                'Gr Liv Area',
                'Full Bath',
                'Half Bath',
                'TotRms AbvGrd',
                'Fireplaces',
               'Wood Deck SF',
                'Open Porch SF',
               'Garage Cars',
               "Garage Area",
               'Total Bsmt SF',
               'SalePrice')
df_num_selected = ames_raw[,select_list]
df_num_selected$`Garage Cars` = as.numeric(df_num_selected$`Garage Cars`)
df_num_selected$`Garage Area` = as.numeric(df_num_selected$`Garage Area`)
df_num_selected$`Total Bsmt SF` = as.numeric(df_num_selected$`Total Bsmt SF`)
cormat2 = round(cor(df_num_selected, method = "pearson", use = "complete.obs"),2)
melted_cormat2 <- melt(cormat2)
HeatPlot2 = ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile() + 
        theme(axis.text.x = element_text(angle = 75, hjust = 1)) + 
        scale_fill_gradient2(low = "#4A96AD", high = "#C63D0F", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")+
        geom_text(aes(Var2, Var1, label = value), color = "#3c204c", size = 2)

#plot the distribution of neighborhood
ggplot(ames_raw, aes(Neighborhood)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_bw()
  

#Plot the histogram of SalePrice
Plot_1 = qplot(ames_raw$SalePrice,
      geom="histogram",
      binwidth = 3000,
      main = "SalePrice Distribution", 
      xlab = "Sale Price",
      ylab = "Frequency")

#View the scatter plot between SalePirce and ground living area
Sale_Area = ggplot(ames_raw, aes(x = `Gr Liv Area`, y= SalePrice )) + 
            geom_point()

#View the scatter plot between SalePirce and overall Quality
Sale_Qual = ggplot(ames_raw, aes(x = `Overall Qual`, y= SalePrice )) +
            geom_point()

#View the scatter plot between SalePirce and Distance
Sale_Dist = ggplot(ames_raw, aes(x = Distance , y= SalePrice )) +
  geom_point()

#View the scatter plot between SalePirce and Position
Sale_Posit = ggplot(ames_raw, aes(x = Position , y= SalePrice )) +
  geom_point() 

ames_raw$`Overall Cond` = as.factor(ames_raw$`Overall Cond`) 

#View the BoxPlot between SalePrice and Type of Dwelling
Sale_Neighborhood = ggplot(ames_raw, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot(fill = '#A8CD1B', colour = '#585858') + theme_bw() +  
  scale_x_discrete(name = "Neighborhood") +
  scale_y_continuous(name = "SalePrice", breaks = seq(0, 900000, 50000)) + 
  ggtitle("Boxplot of SalePrice by Neighborhood") 
Sale_Neighborhood

#View the BoxPlot between SalePrice and others
Factor_list = c('Sale Type',
                'Sale Condition',
                'Overall Cond',
                'Neighborhood',
                'MS SubClass',
                'MS Zoning',
                'Bldg Type',
                'House Style')

Sale_MSSubClass = ggplot(ames_raw, aes(x = `MS Zoning`, y = SalePrice)) +
  geom_boxplot(fill = '#4A96AD', colour = '#404040') + theme_bw() +  
  scale_x_discrete(name = "MS Zoning") +
  scale_y_continuous(name = "SalePrice", breaks = seq(0, 900000, 50000)) + 
  ggtitle("Boxplot of SalePrice by Zoning Classification") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  

#Try combining
ames_raw_subset = subset(ames_raw, Neighborhood %in% c('NoRidge', 'Somerst') ,
                         select = c('Yr Sold', 'SalePrice', 'Neighborhood'))
ggplot(ames_raw_subset, aes(x=`Yr Sold`, y=SalePrice, colour =Neighborhood )) + geom_point() + 
  stat_summary(fun.y = mean, geom = "line", size=1, aes(colour=Neighborhood))+
  theme_bw() +
  scale_y_continuous(name = "Sale Price", breaks = seq(0, 900000, 50000)) +
  xlab("Year") + 
  scale_color_manual(values=c("#67BCDB", "#A2AB58"))

#Three dimension plot
ggplot(aes(x = YearBuilt, y=SalePrice, color = OverallQual), data=variable_df) + geom_point(size = 0.3) + 
  facet_wrap(~Neighborhood)+theme_bw()+
  ggtitle('Categorized by Neighborhood') +
  scale_color_gradient2(midpoint=5, low="Yellow", mid="Orange",high="Red", space ="Lab" )
  
summary(lm(amesdata$SalePrice~amesdata$`Overall Cond`))


#Model Part

library(caret)
library(randomForest)
library(AmesHousing)
library(rpart)

variable_list = c('MS Zoning',
                'Neighborhood',
                'Overall Cond',
                'Total Bsmt SF',
                'Year Built',
                'Gr Liv Area',
                'Garage Cars',
                'Overall Qual',
                'SalePrice')
variable_df = amesdata[,variable_list]
name_list = c('MSZoning',
  'Neighborhood',
  'OverallCond',
  'TotalBsmtSF',
  'YearBuilt',
  'GrLivArea',
  'GarageCars',
  'OverallQual',
  'SalePrice')
colnames(variable_df) = name_list


#Add average price
variable_df['Average_price'] = variable_df$SalePrice/variable_df$GrLivArea


#two way anova model
var<-c('MS Zoning','Neighborhood','Overall Cond','Total Bsmt SF','Year Built',
       'Gr Liv Area', 'Garage Cars','Overall Qual','SalePrice')
newdata<-ames_raw[var]
newdata$`Neighborhood`<-as.factor(newdata$`Neighborhood`)
newdata$`MS Zoning`<-as.factor(newdata$`MS Zoning`)
newdata$`Overall Qual`<-as.factor(newdata$`Overall Qual`)
newdata$`Overall Cond`<-as.factor(newdata$`Overall Cond`)
newdata$`Year Built`[which(newdata$`Year Built`>1871 & 1903>newdata$`Year Built`)]<-1
newdata$`Year Built`[which(newdata$`Year Built`>1902 & 1934>newdata$`Year Built`)]<-2
newdata$`Year Built`[which(newdata$`Year Built`>1933 & 1965>newdata$`Year Built`)]<-3
newdata$`Year Built`[which(newdata$`Year Built`>1964 & 1996>newdata$`Year Built`)]<-4
newdata$`Year Built`[which(newdata$`Year Built`>1995 & 2011>newdata$`Year Built`)]<-5
newdata$`Year Built`<-as.factor(newdata$`Year Built`)
ordered(newdata$`Overall Qual`, levels = c('1', '2', '3','4','5','6','7','8','9','10'))
ordered(newdata$`Overall Cond`, levels = c('1', '2', '3','4','5','6','7','8','9','10'))
ordered(newdata$`Year Built`, levels = c('1', '2', '3','4','5'))


# To find whether MS Zoning and YearBuilt has siganificant effects on the SalePrice, two way anova model has been 
# used.
data2<-newdata
#Basic plot for three variables
stripchart(SalePrice ~ `Year Built`, vertical=TRUE,
           method="jitter",data=data2,xlab='YearBuilt')

boxplot(SalePrice ~ `Year Built`, outline=FALSE,data=data2)

stripchart(SalePrice ~ `MS Zoning`, vertical=TRUE,
           method="jitter",data=data2)
boxplot(SalePrice ~ `MS Zoning`, outline=FALSE,data=data2,xlab='MS Zoning',ylab='SalePrice')
# two way anova model
model3<-lm(SalePrice~`MS Zoning`*`Year Built`,data=data2)
anova(model3)

#Diagnostics
par(mfrow=c(1,1))
qqnorm(model3$residuals)
plot(model3$fitted,model1$residuals,xlab='fitted',ylab='residuals')

# Notice the trumpet pattern for the residuals, so try Box-cox transformation.
boxcox(model3)

#Box plot result shows that we should use log to get normality.
data2$SalePrice<-log(data2$SalePrice)


#Rerun the two way Anova Model
model4<-lm(SalePrice~`MS Zoning`*`Year Built`,data=data2)
anova(model4)
summary(model4)


#Diagnostics
par(mfrow=c(1,1))
qqnorm(model4$residuals)
plot(model4$fitted,model4$residuals,xlab='fitted',ylab='residuals')



#Ancova Model
var<-c('MS Zoning','Neighborhood','Overall Cond','Total Bsmt SF','Year Built',
       'Gr Liv Area', 'Garage Cars','Overall Qual','SalePrice')
newdata<-amesdata[var]

newdata$`Neighborhood`<-as.factor(newdata$`Neighborhood`)
newdata$`MS Zoning`<-as.factor(newdata$`MS Zoning`)
newdata$`Overall Qual`<-as.factor(newdata$`Overall Qual`)
newdata$`Overall Cond`<-as.factor(newdata$`Overall Cond`)

newdata$`Year Built`[which(newdata$`Year Built`>1871 & 1903>newdata$`Year Built`)]<-1
newdata$`Year Built`[which(newdata$`Year Built`>1902 & 1934>newdata$`Year Built`)]<-2
newdata$`Year Built`[which(newdata$`Year Built`>1933 & 1965>newdata$`Year Built`)]<-3
newdata$`Year Built`[which(newdata$`Year Built`>1964 & 1996>newdata$`Year Built`)]<-4
newdata$`Year Built`[which(newdata$`Year Built`>1995 & 2011>newdata$`Year Built`)]<-5
newdata$`Year Built`<-as.factor(newdata$`Year Built`)
ordered(newdata$`Overall Qual`, levels = c('1', '2', '3','4','5','6','7','8','9','10'))
ordered(newdata$`Overall Cond`, levels = c('1', '2', '3','4','5','6','7','8','9','10'))
ordered(newdata$`Year Built`, levels = c('1', '2', '3','4','5'))
#ancova
#Fit the Full Model
model<-lm(SalePrice~.,data=newdata)
anova(model)
summary(model)



variable_df = na.omit(variable_df)
variable_df$MSZoning = as.factor(variable_df$MSZoning)
variable_df$Neighborhood = as.factor(variable_df$Neighborhood)
variable_df$GarageCars = as.factor(variable_df$GarageCars)
variable_df$TotalBsmtSF = as.numeric(variable_df$TotalBsmtSF)
set.seed(1)
ames_idx = createDataPartition(variable_df$SalePrice, p = 0.9, list = FALSE)
data_trn = variable_df[ames_idx,]
data_tst = variable_df[-ames_idx,]
get_tst_rmse = function(model, data){
  pred = predict(model, newdata = data)
  act = data$SalePrice
  sqrt(mean((pred - act) ^ 2))
}


##additive lm
lm_mod = glm(SalePrice ~., data = data_trn)

##knn
knn = train(
  SalePrice ~ .,
  data = data_trn,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5)
)

##
library(corrplot)
# M=cor(variable_df[,3:9])
# corrplot(M, method="ellipse")
tree_mod = rpart(SalePrice ~ .,data = data_trn,cp = 0.001)
tree_pred = predict(tree_mod, newdata = data_tst)
plotcp(tree_mod)
#tree size.min
id.min = which.min(tree_mod$cptable[,'xerror'])
my1se.err = tree_mod$cptable[id.min,'xerror'] + tree_mod$cptable[id.min, 'xstd']
plotcp(tree_mod)
abline(h=my1se.err, col="red")
CP.min = (tree_mod$cptable[id.min, 'CP'] + tree_mod$cptable[(id.min-1), 'CP'])/2
tree.min = prune.rpart(tree_mod, CP.min)
# plot(tree.min)
# text(tree.min)


##tree size.1se
id.1se = min(which(tree_mod$cptable[,'xerror'] < my1se.err)) 
CP.1se = (tree_mod$cptable[id.1se, 'CP'] + tree_mod$cptable[(id.1se-1), 'CP'])/2
tree.1se = prune.rpart(tree_mod , CP.1se)
# plot(tree.1se)
# text(tree.1se)

#rf mod

rf_mod = randomForest(SalePrice ~ ., data = data_trn, mtry = 3, importance = TRUE, ntrees = 500)

#calculate rmse
lm_rmse = get_tst_rmse(lm_mod,data = data_tst)
knn_rmse = get_tst_rmse(knn,data = data_tst)
tree_min_rmse = get_tst_rmse(tree.min,data = data_tst)
tree_1se_rmse = get_tst_rmse(tree.1se,data = data_tst)
rf_rmse = get_tst_rmse(rf_mod,data = data_tst)
tab = rbind(lm_rmse,knn_rmse,tree_min_rmse,tree_1se_rmse,rf_rmse)
rownames(tab) = c("Additive linear regression","K nearest neighbors","Tree with size min","Tree with size 1se","Random forest")
colnames(tab) = c("Test RMSE")
tab


