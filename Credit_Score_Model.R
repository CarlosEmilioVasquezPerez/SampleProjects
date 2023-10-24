############Library#################
library(scorecard)
library(DBI)
library(splitstackshape)
library(ROracle)
library(pROC)
library(dplyr)
library(randomForest)

###############ORCL#################
drv <- dbDriver("orcl")
host <- "host"
port <- "port"
sid <- "orcl"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
con <- dbConnect(drv, username = "user", password = "password", dbname = connect.string, prefetch = FALSE,
                 bulk_read = 1000L, stmt_cache = 0L, external_credentials = FALSE,
                 sysdba = FALSE)

####################Base################
MAIN <- dbSendQuery(con,"
select * from table
where filter = filter
                            ")
MAIN <- fetch(MAIN)

Segment<-'Name'

#-----------------------------------------------------------------------------
#-------------------------------Base------------------------------------------
#-----------------------------------------------------------------------------

Tabla<- MAIN
#Tabla<- filter(MAIN,SCORE_TU>1)

df<- data.frame(
  Tabla$variable1
  ,Tabla$variable2
  ,Tabla$variable3
  ,Tabla$default)

names(df) <- gsub("Tabla.", "", names(df), fixed = TRUE)

#-----------------------------------------------------------------------------
#-----------------------------Partition---------------------------------------
#-----------------------------------------------------------------------------

dt_list = split_df(df, y="default", ratio = 0.8, seed = 1234)
train = dt_list$train; test = dt_list$test;

#-----------------------------------------------------------------------------
#------------------------- Treatment of WOE ----------------------------------
#-----------------------------------------------------------------------------

breaks_adj = lapply(df, function(x) as.character(unique(x)))
breaks_adj = breaks_adj[-length((breaks_adj))]

bins = woebin(df, y="default",stop_limit = 0, bin_num_limit = 15, breaks_list = breaks_adj)

train_woe = woebin_ply(train, bins)
test_woe = woebin_ply(test, bins)

#-----------------------------------------------------------------------------
#-------------------------------Model-----------------------------------------
#-----------------------------------------------------------------------------

m1 = glm(default ~ ., family = binomial(), data = train_woe)
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)

#--------------------------Pred Train y Test----------------------------------

train_pred = predict(m2, train_woe, type='response')
test_pred = predict(m2, test_woe, type='response')

train_perf = perf_eva(train_pred,train$default, title = "train", binomial_metric = c("ks","gini","auc"),showplot = c("auc","gini"))
test_perf = perf_eva(test_pred,test$default, title = "test",binomial_metric = c("ks","gini","auc"),showplot = c("auc","gini"))


#------------------------- ScoreCard Values ----------------------------------

card = scorecard(bins, m2, points0 = 500, odds0 = 1/10, pdo = 75)

train_score = scorecard_ply(train, card, print_step=0)
test_score = scorecard_ply(test, card, print_step=0)

perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$default, test = test$default)
)

#-----------------------------------------------------------------------------
#------------------------ Distribution Table ---------------------------------
#-----------------------------------------------------------------------------

#--- Final Table -----
dfs = rbind(#train_score, 
  test_score)
dft = rbind(#train, 
  test)

distribucion<-gains_table(dfs,dft$default,bin_num = 20,bin_type = 'freq')
Cards<- data.frame()
for (i in (2:length(card))) {
  data_temp<-data.frame(card[[i]]$variable,card[[i]]$bin,card[[i]]$points)
  Cards<-rbind(Cards,card[[i]]) 
}

sdf<- data.frame('score'=dfs$score,'default'=dft$default)

#Partition of data in 0s and 1s to make a 50/50 balance
set.seed(45321)
train_unos <- subset(train_base,FLAG_91_12>0)
train_ceros <- subset(train_base,FLAG_91_12<1)
index <- sample(1:nrow(train_ceros),nrow(train_ceros),replace = F)

#Create 5 models with different 0s but same 1s to make sure of the balance of the data
train_ceros_1 <- train_ceros[index[1:nrow(train_unos)],]
train_ceros_2 <- train_ceros[index[(1 + nrow(train_unos)):(2*nrow(train_unos))],]
train_ceros_3 <- train_ceros[index[(1 + 2*nrow(train_unos)):(3*nrow(train_unos))],]
train_ceros_4 <- train_ceros[index[(1 + 3*nrow(train_unos)):(4*nrow(train_unos))],]
train_ceros_5 <- train_ceros[index[(1 + 4*nrow(train_unos)):(5*nrow(train_unos))],]

train_base1 <-rbind(train_unos,train_ceros_1)
train_base2 <-rbind(train_unos,train_ceros_2)
train_base3 <-rbind(train_unos,train_ceros_3)
train_base4 <-rbind(train_unos,train_ceros_4)
train_base5 <-rbind(train_unos,train_ceros_5)

rm(train_unos);rm(train_ceros);
rm(train_ceros_1);rm(train_ceros_2);rm(train_ceros_3);rm(train_ceros_4);rm(train_ceros_5)