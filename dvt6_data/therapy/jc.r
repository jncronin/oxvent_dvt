library(ggplot2)

# load + clean data
d = readxl::read_excel('oxvent_air_rates.xlsx', skip=1)

d2 = d[which(!is.na(d$`06 Achieved`)),]
d3 = reshape2::melt(d2, measure.vars=c('06 Achieved', '03 Achieved', '05 Achieved'))
d3$vent.id = as.numeric(substr(d3$variable, 1, 2))

d4 = data.frame(vent.id=d3$vent.id,
                vt=d3$`Tidal Volume`,
                tinsp=d3$`Insp. Time (s)`,
                texp=d3$`Exp. Time (s)`,
                act.vt=d3$value)
d4$vt.error = d4$act.vt - d4$vt

# pred vs act flow?
d4$pred.flow = d4$vt/d4$tinsp
d4$act.flow = d4$act.vt/d4$tinsp

ggplot(d4, aes(x=pred.flow, y=act.flow, col=as.factor(vent.id))) +
  geom_point()
ggplot(d4, aes(x=pred.flow, y=act.flow, col=as.factor(vt))) +
  geom_point()
ggplot(d4, aes(x=pred.flow, y=act.flow-pred.flow, col=as.factor(vt))) +
  geom_point()


# median per pred_flow value
ggplot(d4, aes(x=pred.flow, y=act.flow)) +
  stat_summary(fun=median, geom='point')

# divide pred_flow into 25 mL/min bins
lq = function(x) quantile(x, 0.25)
uq = function(x) quantile(x, 0.75)
d4$pred.flow.bin = as.numeric(as.character(cut(d4$pred.flow, breaks=seq(from=700,to=900,by=25),
                       labels=seq(from=712.5,to=887.5, by=25))))
d4$pred.flow.bin.name = sprintf("%i - %i", as.integer(d4$pred.flow.bin-12.5), as.integer(d4$pred.flow.bin+12.5-1))
pleft <- ggplot(d4, aes(x=pred.flow.bin, y=act.flow-pred.flow)) +
  stat_summary(fun.min=min, fun.max=max, geom='errorbar') +
  stat_summary(fun.min=lq, fun.max=uq, fun=median, geom='crossbar', fill='white') +
  #stat_summary(fun=median, geom='point') +
  #stat_summary(fun=median, geom='line') +
  scale_x_continuous(breaks=seq(from=700, to=900, by=50)) +
  geom_point(aes(x=pred.flow, y=act.flow-pred.flow, col=as.factor(vt)),
             inherit.aes = FALSE,
             position=position_dodge2(width=5)) +
  theme_classic() +
  xlab('Set Flow (mL/s)') +
  ylab('Actual - Set Flow (mL/s)') +
  theme(axis.text=element_text(color='black')) +
  scale_color_discrete(name=expression(V[T]*" (mL)"))



# Save for VT (already binned)

pright <- ggplot(d4, aes(x=vt, y=act.vt - vt)) +
  stat_summary(fun.min=min, fun.max=max, geom='errorbar') +
  stat_summary(fun.min=lq, fun.max=uq, fun=median, geom='crossbar', fill='white') +
  #stat_summary(fun=median, geom='point') +
  #stat_summary(fun=median, geom='line') +
  scale_x_continuous(breaks=seq(from=400, to=600, by=50)) +
  geom_point(aes(x=vt, y=act.vt - vt, col=as.factor(pred.flow.bin.name)),
             inherit.aes = FALSE,
             position=position_dodge2(width=5)) +
  theme_classic() +
  xlab(expression('Set V'[T]*' (mL)')) +
  ylab(expression('Actual - Set V'[T] * ' (mL)')) +
  theme(axis.text=element_text(color='black'), axis.ticks=element_line(color='black')) +
  scale_color_discrete(name='Set Flow (mL/s)')

# combine
p <- ggpubr::ggarrange(pleft, pright, nrow=1, ncol=2,
                       align='hv',
                       labels = 'auto')

# export
Cairo::CairoPDF('fig4.pdf', width=8.5, height=4)
print(p)
dev.off()



# try again with just the greatest error per vent setting
d4$flow.error = d4$act.flow - d4$pred.flow

dvt <- reshape2::dcast(d4, vt+tinsp+pred.flow~'vt.error', value.var='vt.error', fun.aggregate=min)
dflow <- reshape2::dcast(d4, vt+tinsp+pred.flow~'flow.error', value.var='flow.error', fun.aggregate=min)
dvt.flow <- merge(dvt, dflow)

#dvt.flow$pred.flow.bin = as.numeric(as.character(cut(dvt.flow$pred.flow, breaks=seq(from=700,to=900,by=25),
#                                               labels=seq(from=712.5,to=887.5, by=25))))
#dvt.flow$pred.flow.bin.name = sprintf("%i - %i", as.integer(dvt.flow$pred.flow.bin-12.5), as.integer(dvt.flow$pred.flow.bin+12.5-1))

dvt.flow$pred.flow.bin = cut(dvt.flow$pred.flow, breaks=c(700,800,900),
                             labels=c('700-799', '800-899'))
dvt.flow$pred.flow.bin.name = dvt.flow$pred.flow.bin

pleft2 <- ggplot(dvt.flow, aes(x=pred.flow, y=flow.error, col=as.factor(vt))) +
  geom_point() +
  geom_line() +
  theme_classic() +
  xlab('Set Flow (mL/s)') +
  ylab('Actual - Set Flow (mL/s)') +
  theme(axis.text=element_text(color='black'), axis.ticks=element_line(color='black')) +
  scale_color_discrete(name=expression(V[T]*" (mL)"))

pright2 <- ggplot(dvt.flow, aes(x=vt, y=vt.error, col=as.factor(pred.flow.bin.name))) +
  geom_point() +
  #geom_line() +
  geom_smooth(aes(group=as.factor(pred.flow.bin.name)),
              method='lm', formula=y~x, se=FALSE) +
  theme_classic() +
  xlab(expression('Set V'[T]*' (mL)')) +
  ylab(expression('Actual - Set V'[T] * ' (mL)')) +
  theme(axis.text=element_text(color='black'), axis.ticks=element_line(color='black')) +
  scale_color_discrete(name='Set Flow (mL/s)')

# combine
p2 <- ggpubr::ggarrange(pleft2, pright2, nrow=1, ncol=2,
                       align='hv',
                       labels = 'auto')

# export
Cairo::CairoPDF('fig4-2.pdf', width=8.5, height=3)
print(p2)
dev.off()
