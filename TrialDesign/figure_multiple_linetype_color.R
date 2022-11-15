#===================================
data_1 <- data.table(info=seq(0,10,by=0.01)) %>% 
  .[,Benefit:=10*(1-exp(-0.6*info))] %>%  
  .[,Cost:=info^2/6] 

data_1_plot <- data_1 %>%
  melt(id.vars='info') %>% 
  data.table() %>% 
  .[,type:=paste('Information Type 1 (',variable,')',sep='')] 

data_2 <- data.table(info=seq(0,10,by=0.01)) %>% 
  .[,Benefit:=2*(1-exp(-0.4*info))] %>%  
  .[,Cost:=info^2/14] 

data_2_plot <- data_2 %>% 
  melt(id.vars='info') %>% 
  data.table() %>% 
  .[,type:=paste('Information Type 2 (',variable,')',sep='')] 

data <- rbind(mutate(data_1,type:=1),mutate(data_2,type:=2)) %>% 
  data.table()
data_plot <- rbind(data_1_plot,data_2_plot)

opt_info <- data[,.SD[which.max(Benefit-Cost),],by=type]


g_info <- ggplot(data=data_plot) +
  geom_line(aes(y=value,x=info,color=type,linetype=type),size=1) +
  scale_linetype_manual(name='',values = c('solid','dashed','solid','dashed'),guide=guide_legend(nrow=2,byrow=TRUE)) +
  scale_color_manual(name='',values = c('red','red','blue','blue'),guide=guide_legend(nrow=2,byrow=TRUE)) +
  ylim(0,10) +
  #--- optimal ---#
  geom_segment(x=opt_info[type==1,info],y=opt_info[type==1,Cost],xend=opt_info[type==1,info],yend=opt_info[type==1,Benefit]) +
  annotate('text',x=4,y=5.5,label='Maximum Net \n Benefit Achieved',family='Times',color='red') +
  annotate('text',x=3,y=9.6,label='Optimal Amountf of \n Information of type 1',family='Times') +
  #--- too little information ---#
  geom_segment(x=0.5,y=0.5^2/6,xend=0.5,yend=10*(1-exp(-0.6*0.5))) +
  annotate('text',x=1.3,y=1.7,label='Too Little \n Information \n of type 1',family='Times') +
  #--- too much information ---#
  geom_segment(x=7,y=7^2/6,xend=7,yend=10*(1-exp(-0.6*7))) +
  annotate('text',x=6.2,y=8.7,label='Too Much \n Information \n of type 1',family='Times') +
  #--- excessive information ---#
  geom_segment(x=7,y=7^2/14,xend=7,yend=2*(1-exp(-0.4*7))) +
  annotate('text',x=7.8,y=2.85,label='Excessive \n Information \n of type 2',family='Times') +
  #--- format ---#
  xlab('Information Acquisition Effort') +
  ylab('Value/Cost of Information') +
  theme(
    legend.position='bottom',
    legend.key.width=unit(2,'cm'),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
    )
