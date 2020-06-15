setwd("F:\\Material Building for softanbees\\Demo Assignment\\Tidymodels Tutorial")

library(tidyverse)
library(tidymodels)
library(skimr)

B_Can = read.csv("Breast_cancer_data.csv")

B_Can$diagnosis = as.factor(B_Can$diagnosis)

attach(B_Can)

skim(B_Can)

#Splitting This dataset

set.seed(3456)

wn_split = B_Can%>%
                initial_split(strata = diagnosis)


train = training(wn_split)

test  = testing(wn_split)

#Data pre-processing and feature emgineering with recipe

wn_rec = recipe(diagnosis~.,data = train)%>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  step_zv(all_numeric())%>%
  step_normalize(all_numeric())%>%
  prep()


DPP_train = juice(wn_rec)

DPP_test = bake(wn_rec,test)


#KNN modeling

knn_spec = nearest_neighbor()%>%
              set_engine("kknn")%>%
              set_mode("classification") 

knn_fit = knn_spec%>% 
              fit(diagnosis ~.,data = train)
knn_fit

#Random Forest

rd_spec = rand_forest()%>%
            set_engine("ranger")%>%
            set_mode("classification") 

rad_fit = rd_spec%>%
                 fit(diagnosis ~.,data = train) 



#SVM 

svm_spec = svm_rbf()%>%
            set_engine("kernlab")%>%
            set_mode("classification")

svm_fit = svm_spec%>%
              fit(diagnosis ~.,data = train)
svm_fit

# Evaluate Models

set.seed(4876)

validation_splits = mc_cv(DPP_train,prop = 0.9,strata = diagnosis)

knn_res = fit_resamples(
   
  diagnosis ~.,
  knn_spec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
  
)

knn_res %>%
   collect_metrics()



rd_res = fit_resamples(
  
  diagnosis ~.,
  rd_spec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
  
)

rd_res %>%
  collect_metrics()


svm_res = fit_resamples(
  
  diagnosis~.,
  svm_spec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
  
)

svm_res %>%
  collect_metrics()

#Plot for a better model

 knn_res %>%
  unnest(.predictions)%>%
  mutate(model = "kknn")%>%
  bind_rows(rd_res %>%
              unnest(.predictions)%>%
              mutate(model = "ranger"))%>%
  bind_rows(svm_res %>%
              unnest(.predictions)%>%
              mutate(model = "kernlab"))%>%
  group_by(model)%>%
  roc_curve(diagnosis,.pred_1)%>%
  autoplot()

#Confusion Matrix
 
 rd_res%>%
   unnest(.predictions)%>%
   conf_mat(diagnosis,.pred_class)%>%
  autoplot(type = "heatmap")

 #Prediction using downsampled test data
 
 rad_fit%>%
   predict(DPP_test,type = "prob")%>%
   mutate(truth = test$diagnosis)%>%
   roc_auc(truth,.pred_1)


    
 
  
  
  


