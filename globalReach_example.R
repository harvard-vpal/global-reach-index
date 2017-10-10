##Author: Ilia Rushkin
source('globalReachIndex.R')

#This script loads a fake data set 'example_data.csv' for two fake courses and calculates the global reach indices. 
#It produces a csv table with the result and an html with a graph.

library(plotly)
library(plyr)


df=read.csv(file.path('example_data.csv'),header=TRUE,stringsAsFactors = FALSE)


df1=aggregate(df$country, by=list(course_id=df$course_id), FUN=function(x){return(eddington(x)$R)})
df1=plyr::rename(df1,c('x'='R-index'))

df2=aggregate(df$country, by=list(course_id=df$course_id), FUN=function(x){return(100*eddington(x)$Rn)})
df2=plyr::rename(df2,c('x'='R-index-normalized-pct'))

df1=merge(df1,df2,by='course_id')
df1=df1[order(-df1$`R-index`),]

p=plot_ly(df1,y=~`R-index`,x=~course_id,type='bar',orientation='v',name='R-index',
          hoverinfo='text',
          text=~paste0(course_id,'<br>R-index: ',`R-index`)
          
)%>%
  layout(title='Global Reach',
         yaxis=list(title='R-index', showticklabels=TRUE),
         xaxis=list(title='Courses',showticklabels=TRUE),
         barmode='relative',
         margin=list(t=70,b=70)
  )

p1= plot_ly(df1,y=~`R-index-normalized-pct`,x=~course_id, type='bar',
            hoverinfo='text',
            text=~paste0(course_id,'<br>Rn-index: ',signif(`R-index-normalized-pct`,3), '%'),
            name='Rn-index (normalized by course size)'
)%>%
  layout(title='Global Reach',
         yaxis=list(title='Rn-index (normalized by course size)', showticklabels=TRUE, autorange='reversed',ticksuffix='%'),
         xaxis=list(title='Courses',showticklabels=TRUE),
         barmode='relative',
         margin=list(t=70,b=70)
  )

p=subplot(p,p1,shareX = TRUE,nrows = 2, margin=0)

print(p)


vars_record=c('course_id','R-index','R-index-normalized-pct')

htmlwidgets::saveWidget(p, "globalReach_example.html")
write.csv(df1[,vars_record],file='globalReachTable_example.csv', row.names = FALSE)


