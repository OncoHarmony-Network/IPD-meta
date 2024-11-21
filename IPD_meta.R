rm(list=ls())
library(survminer)

#####survival analysis#####
####OS####
load('all_os.RData')
#p value, HR and 95% CI
#ED group ICI vs CT
cox <- coxph(Surv(futime,fustat) ~ subgroup, data=subset(all_os, EF_group == 'ED'))
sum <- summary(cox)
hr_ed <- exp(coef(cox))
lci_ed <- sum$conf.int[3]
uci_ed <- sum$conf.int[4]
p_ed <- sum$coefficients[, "Pr(>|z|)"]
#NED group ICI vs CT
cox <- coxph(Surv(futime,fustat) ~ subgroup, data=subset(all_os, EF_group == 'NED'))
sum <- summary(cox)
hr_ned <- exp(coef(cox))
lci_ned <- sum$conf.int[3]
uci_ned <- sum$conf.int[4]
p_ned <- sum$coefficients[, "Pr(>|z|)"]
#ICI group ED vs NED
cox <- coxph(Surv(futime,fustat) ~ EF_group, data=subset(all_os, subgroup == 'ICI'))
sum <- summary(cox)
hr_ici <- exp(coef(cox))
lci_ici <- sum$conf.int[3]
uci_ici <- sum$conf.int[4]
p_ici <- sum$coefficients[, "Pr(>|z|)"]
#CT group ED vs NED
cox <- coxph(Surv(futime,fustat) ~ EF_group, data=subset(all_os, subgroup == 'CT'))
sum <- summary(cox)
hr_ct <- exp(coef(cox))
lci_ct <- sum$conf.int[3]
uci_ct <- sum$conf.int[4]
p_ct <- sum$coefficients[, "Pr(>|z|)"]
#all data ED vs NED
cox <- coxph(Surv(futime, fustat) ~ EF_group, data = all_os)
sum <- summary(cox)
hr <- exp(coef(cox))
p <- sum$coefficients[, "Pr(>|z|)"]

#plot
fit <- survfit(Surv(futime, fustat) ~ EF_group + subgroup, data = all_os)
p_os <- ggsurvplot(fit,
                 surv.median.line = 'hv',
                 pval = T,
                 PVAL.METHOD = T,
                 conf.int = F,
                 risk.table = T,
                 test.for.trend = F,
                 risk.table.title = 'No. at risk',
                 xlab = 'Follow-up time, mo',
                 ylab = 'Progression-free survival, %',
                 legend.title = 'Group',
                 legend.lab = c('EF/ICI','EF/CT','NED/ICI','NED/CT'),
                 break.x.by = 4,
                 palette = c('#F79F36','#00ABEA','#305864','#897F72'),
                 fontsize = 4,
                 pval.size = 3,
                 risk.tables.y.text = T,
                 tables.y.text.col = F,
                 tables.height = 0.3)
median_time <- summary(fit)$table[,'median']
p_os$plot <- p_os$plot + annotate('text',x=Inf,y=1,label=paste('ED/ICI vs ED/CT: mOS=',sprintf('%.2f',median_time[1]),'vs',sprintf('%.2f',median_time[2]),'; p=',sprintf('%.2f',p_ed),';HR=',sprintf('%.2f',hr_ed),';95%CI=',sprintf('%.2f',lci_ed),'-',sprintf('%.2f',uci_ed)),
                              parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.95,label=paste('NED/ICI vs NED/CT: mOS=',sprintf('%.2f',median_time[3]),'vs',sprintf('%.2f',median_time[4]),'; p=',sprintf('%.2f',p_ned),';HR=',sprintf('%.2f',hr_ned),';95%CI=',sprintf('%.2f',lci_ned),'-',sprintf('%.2f',uci_ned)),
           parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.9,label=paste('ED/ICI vs NED/ICI: mOS=',sprintf('%.2f',median_time[1]),'vs',sprintf('%.2f',median_time[3]),'; p=',sprintf('%.2f',p_ici),';HR=',sprintf('%.2f',hr_ici),';95%CI=',sprintf('%.2f',lci_ici),'-',sprintf('%.2f',uci_ici)),
           parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.85,label=paste('ED/CT vs NED/CT: mOS=',sprintf('%.2f',median_time[2]),'vs',sprintf('%.2f',median_time[4]),'; p=',sprintf('%.2f',p_ct),';HR=',sprintf('%.2f',hr_ct),';95%CI=',sprintf('%.2f',lci_ct),'-',sprintf('%.2f',uci_ct)),
           parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.8,label=paste('ED vs NED: HR=',sprintf('%.2f',hr),';p=',sprintf('%.2f',p)),
           parse = F, hjust = 1.3, vjust = 0.5)
pdf('KM_OS.pdf',width=10,height=8)
print(p_os,newpage=F)
dev.off()

####PFS####
load('all_pfs.RData')
#p value, HR and 95% CI
#ED group ICI vs CT
cox <- coxph(Surv(futime,fustat) ~ subgroup, data=subset(all_pfs, EF_group == 'ED'))
sum <- summary(cox)
hr_ed <- exp(coef(cox))
lci_ed <- sum$conf.int[3]
uci_ed <- sum$conf.int[4]
p_ed <- sum$coefficients[, "Pr(>|z|)"]
#NED group ICI vs CT
cox <- coxph(Surv(futime,fustat) ~ subgroup, data=subset(all_pfs, EF_group == 'NED'))
sum <- summary(cox)
hr_ned <- exp(coef(cox))
lci_ned <- sum$conf.int[3]
uci_ned <- sum$conf.int[4]
p_ned <- sum$coefficients[, "Pr(>|z|)"]
#ICI group ED vs NED
cox <- coxph(Surv(futime,fustat) ~ EF_group, data=subset(all_pfs, subgroup == 'ICI'))
sum <- summary(cox)
hr_ici <- exp(coef(cox))
lci_ici <- sum$conf.int[3]
uci_ici <- sum$conf.int[4]
p_ici <- sum$coefficients[, "Pr(>|z|)"]
#CT group ED vs NED
cox <- coxph(Surv(futime,fustat) ~ EF_group, data=subset(all_pfs, subgroup == 'CT'))
sum <- summary(cox)
hr_ct <- exp(coef(cox))
lci_ct <- sum$conf.int[3]
uci_ct <- sum$conf.int[4]
p_ct <- sum$coefficients[, "Pr(>|z|)"]
#all data ED vs NED
cox <- coxph(Surv(futime, fustat) ~ EF_group, data = all_pfs)
sum <- summary(cox)
hr <- exp(coef(cox))
p <- sum$coefficients[, "Pr(>|z|)"]

#plot
fit <- survfit(Surv(futime, fustat) ~ EF_group + subgroup, data = all_pfs)
p_pfs <- ggsurvplot(fit,
                   surv.median.line = 'hv',
                   pval = T,
                   PVAL.METHOD = T,
                   conf.int = F,
                   risk.table = T,
                   test.for.trend = F,
                   risk.table.title = 'No. at risk',
                   xlab = 'Follow-up time, mo',
                   ylab = 'Progression-free survival, %',
                   legend.title = 'Group',
                   legend.lab = c('EF/ICI','EF/CT','NED/ICI','NED/CT'),
                   break.x.by = 4,
                   palette = c('#F79F36','#00ABEA','#305864','#897F72'),
                   fontsize = 4,
                   pval.size = 3,
                   risk.tables.y.text = T,
                   tables.y.text.col = F,
                   tables.height = 0.3)
median_time <- summary(fit)$table[,'median']
p_pfs$plot <- p_os$plot + annotate('text',x=Inf,y=1,label=paste('ED/ICI vs ED/CT: mOS=',sprintf('%.2f',median_time[1]),'vs',sprintf('%.2f',median_time[2]),'; p=',sprintf('%.2f',p_ed),';HR=',sprintf('%.2f',hr_ed),';95%CI=',sprintf('%.2f',lci_ed),'-',sprintf('%.2f',uci_ed)),
                                  parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.95,label=paste('NED/ICI vs NED/CT: mOS=',sprintf('%.2f',median_time[3]),'vs',sprintf('%.2f',median_time[4]),'; p=',sprintf('%.2f',p_ned),';HR=',sprintf('%.2f',hr_ned),';95%CI=',sprintf('%.2f',lci_ned),'-',sprintf('%.2f',uci_ned)),
           parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.9,label=paste('ED/ICI vs NED/ICI: mOS=',sprintf('%.2f',median_time[1]),'vs',sprintf('%.2f',median_time[3]),'; p=',sprintf('%.2f',p_ici),';HR=',sprintf('%.2f',hr_ici),';95%CI=',sprintf('%.2f',lci_ici),'-',sprintf('%.2f',uci_ici)),
           parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.85,label=paste('ED/CT vs NED/CT: mOS=',sprintf('%.2f',median_time[2]),'vs',sprintf('%.2f',median_time[4]),'; p=',sprintf('%.2f',p_ct),';HR=',sprintf('%.2f',hr_ct),';95%CI=',sprintf('%.2f',lci_ct),'-',sprintf('%.2f',uci_ct)),
           parse = F, hjust = 1.1, vjust = 0.5)+
  annotate('text',x=Inf,y=0.8,label=paste('ED vs NED: HR=',sprintf('%.2f',hr),';p=',sprintf('%.2f',p)),
           parse = F, hjust = 1.3, vjust = 0.5)
pdf('KM_PFS.pdf',width=10,height=8)
print(p_pfs,newpage=F)
dev.off()


#####meta analysis#####
library(metafor)
####OS####
load('meta_OS.RData')
ln <- log(all$hr)
se <- (all$uci-all$lci)/(2*1.96)
escil <- cbind(all,ln,se)
num_ed <- paste(all$no_ed,'(',sprintf('%.2f',all$no_ed/all$ED*100),')')
num_ned <- paste(all$no_ned,'(',sprintf('%.2f',all$no_ned/all$nED*100),')')

res <- rma(yi=ln,sei=se,data=escil)
res_ici <- rma(yi=ln,sei=se,data=escil,subset = (treatment=='ICI'))
res_ct <- rma(yi=ln,sei=se,data=escil,subset = (treatment=='CT'))
res
#plot
pdf('forest_OS.pdf',width=8,height = 6)
forest(res,slab=paste(all$cohort,'(n =',all$num,')'),order=order(all$treatment),xlim=c(-12,4),ylim=c(-3,22),at = c(log(0.5), 0, log(1.5)),atransf=exp,
       ilab=cbind(all$ED,all$nED,num_ed,num_ned),ilab.xpos=c(-8,-6.3,-4.3,-1.8),cex=.8,rows = c(3:9,14:18),mlab='FE Model for All Studies')
op <- par(cex=.75,font=4)
text(-12,c(10,19),c('ICI','CT'),pos=4)
par(font=2)
text(c(-8,-6.3),21,c('No. of ED','No. of NED'))
text(-12,21,'Cohort (No.)',pos=4)
text(4,21,'HR[95%CI]',pos=2)
segments(x0=-5.3, y0=21.6, x1=-3.3, y1=21.6, col="black", lwd=1)
segments(x0=-2.8, y0=21.6, x1=-0.8, y1=21.6, col="black", lwd=1)
text(c(-4.3,-1.8),22.1,c('ED','NED'))
text(c(-4.3,-1.8),21,c('No. of cases(%)','No. of cases(%)'))
par(op)
addpoly(res1,row=1.5,cex=.75,atransf=exp,mlab='FE Model for Subgroup')
addpoly(res2,row=12.5,cex=.75,atransf=exp,mlab='FE Model for Subgroup')
text(-12,0.5,pos=4,cex=.6,font=4,'p=0.01; Heterogeneity: I^2=0.01%, p=0.58')
text(-12,11.5,pos=4,cex=.6,font=4,'p=0.05; Heterogeneity: I^2=0.01%, p=0.76')
text(-12,-2,pos=4,cex=.6,font=4,'p=0.01; Heterogeneity: I^2=0.01%, p=0.83')
text(c(-1,1.2),-3,c('Favors ED','Favors NED'))
dev.off()

####PFS####
load('meta_PFS.RData')

ln <- log(all$hr)
se <- (all$uci-all$lci)/(2*1.96)
escil <- cbind(all,ln,se)
num_ed <- paste(all$no_ed,'(',sprintf('%.2f',all$no_ed/all$ED*100),')')
num_ned <- paste(all$no_ned,'(',sprintf('%.2f',all$no_ned/all$nED*100),')')

res <- rma(yi=ln,sei=se,data=escil,method='FE')
res_ici <- rma(yi=ln,sei=se,data=escil,method='FE',subset = (treatment=='ICI'))
res_ct <- rma(yi=ln,sei=se,data=escil,method='FE',subset = (treatment=='CT'))
res


pdf('forest_PFS.pdf',width=8,height = 6)
forest(res,slab=paste(all$cohort,'(n =',all$num,')'),order=order(all$treatment),xlim=c(-12,4),ylim=c(-3,18),at = c(log(0.5), 0, log(1.5)),atransf=exp,
       ilab=cbind(all$ED,all$nED,num_ed,num_ned),ilab.xpos=c(-8,-6.3,-4.3,-1.8),cex=.8,rows = c(3:7,12:14),mlab='FE Model for All Studies')
op <- par(cex=.75,font=4)
text(-12,c(8,15),c('ICI','CT'),pos=4)
par(font=2)
text(c(-8,-6.3),17,c('No. of ED','No. of NED'))
text(-12,17,'Cohort (No.)',pos=4)
text(4,17,'HR[95%CI]',pos=2)
segments(x0=-5.3, y0=17.6, x1=-3.3, y1=17.6, col="black", lwd=1)
segments(x0=-2.8, y0=17.6, x1=-0.8, y1=17.6, col="black", lwd=1)
text(c(-4.3,-1.8),18.1,c('ED','NED'))
text(c(-4.3,-1.8),17,c('No. of cases(%)','No. of cases(%)'))
par(op)
addpoly(res1,row=1.5,cex=.75,atransf=exp,mlab='FE Model for Subgroup')
addpoly(res2,row=11,cex=.75,atransf=exp,mlab='FE Model for Subgroup')
text(-12,0.5,pos=4,cex=.6,font=4,'p=0.02; Heterogeneity: I^2=0.01%, p=0.81')
text(-12,10,pos=4,cex=.6,font=4,'p=0.39; Heterogeneity: I^2=12.52%, p=0.32')
text(-12,-2,pos=4,cex=.6,font=4,'p=0.02; Heterogeneity: I^2=0.01%, p=0.73')
text(c(-1,1.2),-3,c('Favors ED','Favors NED'))
dev.off()
