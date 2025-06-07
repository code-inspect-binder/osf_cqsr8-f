#----------------------------------------------------------------
# create a df that illustrats performance measures per method ####

# combine data
methods_plot <- left_join(bagging_plot,rf_plot,by=c("indices")) %>%
  left_join(., boosting_plot,by=c("indices"))  %>% 
  left_join(., boosting_xgbDART_plot,by=c("indices")) %>%
  left_join(., knn_plot,by=c("indices")) %>%
  left_join(., neuronal_plot,by=c("indices"))

# manipulate data for ggplot
methods_plot <- reshape2::melt(methods_plot,id.vars = "indices") 
methods_plot$value <- as.numeric(methods_plot$value) 
methods_plot$value <- methods_plot$value*100 
methods_plot$indices <- ifelse(methods_plot$indices=="Neg Pred Value","NPV",methods_plot$indices)
#----------------------------------------------------------------

#-------------------------------------------------------------------------------
# create a plot that illustrats: sensitivity, specificity, precision and NPV ####

plot_1 <- methods_plot %>% 
  filter(indices %in% c("Sensitivity","Specificity", "Precision", "NPV"))

ggplot(plot_1,aes(x=variable,y=value,fill=reorder(indices,value))) +
  geom_bar(stat = "identity", position = "dodge",color="black") + 
  xlab("") +
  ylab("in %") +
  ggtitle("") +
  scale_fill_brewer(palette = "Accent",name="") +
  theme_bw() + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(0,12.5,25,37.5,50,62.5,75,87.5,100),limits = c(0,95)) + 
  theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1, size = 12)) + 
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) 
#-------------------------------------------------------------------------------

#-----------------------------------------------------------
# create a plot that illustrats: Informedness,Markedness ####

plot_2 <- methods_plot %>% 
  filter(indices %in% c("Informedness","Markedness"))

ggplot(plot_2,aes(x=variable,y=value,fill=reorder(indices,value))) +
  geom_bar(stat = "identity",position = "dodge",color="black") + 
  xlab("") + 
  ylab("in %") + 
  ggtitle("") + 
  scale_fill_brewer(palette = "Pastel2",name="") + 
  theme_bw() +
  theme(legend.position="bottom") + 
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = c(0,12.5,25,37.5,50,62.5,75,87.5,100),limits = c(0,95)) +
  theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1, size = 12)) + 
  theme(axis.text.y = element_text( size = 12)) +
  theme(axis.title.y = element_text( size = 12)) +
  theme(legend.title = element_text( size = 12 )) 
#-----------------------------------------------------------

#------------------------------------------------------------------
# create a plot that illustrats: Accuracy, Kappa, AUROC and MCC ####

plot_3 <- methods_plot %>% 
  filter(indices %in% c("Accuracy", "Kappa", "AUROC" , "MCC"))

ggplot(plot_3,aes(x=variable,y=value,fill=reorder(indices,value))) +
  geom_bar(stat = "identity",position = "dodge",color="black") + xlab("") +
  ylab("in %") + 
  ggtitle("") +
  scale_fill_brewer(palette = "Pastel1",name="") + 
  theme_bw()  +   theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(0,12.5,25,37.5,50,62.5,75,87.5,100),limits = c(0,95)) +
  theme(axis.text.x = element_text(angle=30, hjust = 1, vjust = 1, size = 12)) +
  theme(axis.text.y = element_text( size = 12)) +
  theme(axis.title.y = element_text( size = 12)) +
  theme(legend.title = element_text( size = 12 )) 
#------------------------------------------------------------------

#---------------------------------------
# create a table with model outcomes ####
tab_methods <- reshape2::dcast(methods_plot, indices ~ variable)
#---------------------------------------