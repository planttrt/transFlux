
g <- ggplot(bothField, aes(Site, TR)) 
g + geom_violin(scale='area')

g <- ggplot(bothField , aes(WS)) 
g + geom_density(binwidth=2, aes(color=Site))

g <- ggplot(pkField, aes(TR, ET))
g + geom_point() + geom_smooth() +geom_abline(color='red')




g <- ggplot(bothFieldMODIS, aes(LST-TA, TR, size=Rg, color=Rg))
g + geom_point() + geom_smooth() +facet_grid(~Site)


plot(bwFieldMODIS$STd-bwFieldMODIS$Ta, bwFieldMODIS$TR)