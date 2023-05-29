
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

data = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")

data$discrim_harassed

harrased = ggplot(data, aes(discrim_harassed)) +
  geom_bar(fill = "#00AFBB") +
  labs(title = 'discrimination: harrased')+
  
  theme_pubclean()

harrased_plot = harrased + scale_x_discrete(name = "response",
                    limits = c("1", "2", "3", "4", "5", "6"),
                    labels=c("1" = "almost every day",
                             "2" = "at least once a week",
                             "3" = "a few times a month",
                             "4" = "a few times a year",
                             "5" = "less than once a year",
                             "6" = "never"))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=30))

print(harrased_plot)

############
############

data$discrim_lessrespect

lessrespect = ggplot(data, aes(discrim_lessrespect)) +
  geom_bar(fill = "#00AFBB") +
  labs(title = 'discrimination: lessrespect')+
  
  theme_pubclean()

lessrespect_plot = lessrespect +  scale_x_discrete(name = "response",
                                limits = c("1", "2", "3", "4", "5", "6"),
                                labels=c("1" = "almost every day",
                                         "2" = "at least once a week",
                                         "3" = "a few times a month",
                                         "4" = "a few times a year",
                                         "5" = "less than once a year",
                                         "6" = "never"))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=30))

print(lessrespect_plot)

############
############


data$discrim_medical


medical = ggplot(data, aes(discrim_medical)) +
  geom_bar(fill = "#00AFBB") +
  labs(title = 'discrimination: medical')+
  
  theme_pubclean()

medical_plot = medical +  scale_x_discrete(name = "response",
                     limits = c("1", "2", "3", "4", "5", "6"),
                     labels=c("1" = "almost every day",
                              "2" = "at least once a week",
                              "3" = "a few times a month",
                              "4" = "a few times a year",
                              "5" = "less than once a year",
                              "6" = "never"))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=30))

print(medical_plot)
############
############

data$discrim_notclever

notclever = ggplot(data, aes(discrim_notclever)) +
  geom_bar(fill = "#00AFBB") +
  labs(title = 'discrimination: not clever')+
  theme_pubclean()

notclever_plot = notclever + scale_x_discrete(name = "response",
                             limits = c("1", "2", "3", "4", "5", "6"),
                             labels=c("1" = "almost every day",
                                      "2" = "at least once a week",
                                      "3" = "a few times a month",
                                      "4" = "a few times a year",
                                      "5" = "less than once a year",
                                      "6" = "never"))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=30))



print(notclever_plot)

############
############

data$discrim_poorerservice


poorerservice = ggplot(data, aes(discrim_poorerservice)) +
  geom_bar(fill = "#00AFBB") +
  labs(title = 'discrimination: poorer service')+
  
  theme_pubclean()

poorerservice_plot = poorerservice +  scale_x_discrete(name = "response",
                                  limits = c("1", "2", "3", "4", "5", "6"),
                                  labels=c("1" = "almost every day",
                                           "2" = "at least once a week",
                                           "3" = "a few times a month",
                                           "4" = "a few times a year",
                                           "5" = "less than once a year",
                                           "6" = "never"))+
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=30))

print(poorerservice_plot)


############
############