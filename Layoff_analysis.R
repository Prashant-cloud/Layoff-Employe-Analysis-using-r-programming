install.packages("dplyr")
install.packages("plotly")
library(dplyr)
library(plotly)
layoff <- read.csv("~/Dashboard/layoff.csv")





df= as.data.frame(layoff)
df




####Top 10 company layoff
 
top_n_company <- df%>%
                  group_by(company)%>%
                  summarise(total_laid = sum(total_laid_off))%>%
                  arrange(desc(total_laid))%>%
                  head(n = 10)

 View(top_n_company)
 

 fig <- plot_ly(top_n_company, x = ~ company, y = ~total_laid, type = 'bar', color = ~company)
 fig <- fig %>% layout(title = "Top 10 company",
                       xaxis = list(title = "Company"),
                       yaxis = list(title = "No. of employee"))
 fig
 
####Countrys
 
top_n_country <- df%>%
         group_by(country)%>%
         summarise(total_laid = sum(total_laid_off))%>%
        mutate(percent_laid = total_laid/sum(total_laid)*100)%>%
         arrange(desc(total_laid))%>%
  head(n = 10)

View(top_n_country)

fig<- plot_ly(top_n_country, labels = ~country, values = ~total_laid, type = 'pie')
fig <- fig %>% layout(title = "Top 10 Country")

fig

#Industry wise layoff

industry_layoff <- df%>%
  group_by(industry)%>%
  summarise(total_layoff = sum(total_laid_off))%>%
  arrange(desc(total_layoff))

View(industry_layoff)

fig <- plot_ly(industry_layoff, x = ~ industry, y = ~total_layoff, type = 'bar', color = ~industry)
fig <- fig %>% layout(title = "Industry wise Layoff",
                      xaxis = list(title = "Industry"),
                      yaxis = list(title = "No. of employee"))
fig

#Stage wise layoff

stage_layoff <- df%>%
  group_by(stage)%>%
  summarise(count = n()))%>%
  mutate(modelay = mode(total_layoff))%>%
  arrange(desc(total_layoff))

View(industry_layoff)

fig <- plot_ly(stage_layoff, x = ~stage, y = ~total_layoff, text = ~stage, type = 'bar',color = stage)
fig <- fig %>% layout(title = "Stage wise Layoff",
                      xaxis = list(title = "Stage"),
                      yaxis = list(title = "No. of employee"))           
fig


## relationship btw fund recived and layoff

fig <- plot_ly(df, x = ~funds_raised, y = ~total_laid_off, type = 'scatter' ,
               color = ~total_laid_off, size = ~total_laid_off,
               text = ~company)
fig <- fig %>% layout(title = 'Layoff vs Percentage Of layoff',
                      yaxis = list(title = "Fund Raised", zeroline = FALSE),
                      xaxis = list(title = "No. of Layoff", zeroline = FALSE))
   fig
   
   

   
   
## relationship btw no of emplyoee layoff and percentage
   
   fig <- plot_ly(df, x = ~total_laid_off, y = ~percentage_laid_off, type = 'scatter',text = ~company,
                  marker = list(size = 10,
                                color = 'rgba(255, 182, 193, .9)',
                                line = list(color = 'rgba(152, 0, 0, .8)',
                                            width = 2)))
   fig <- fig %>% layout(title = 'Layoff vs Percentage Of layoff',
                         yaxis = list(title = "No. of layoff", zeroline = FALSE),
                         xaxis = list(title = "Percentage of employee", zeroline = FALSE))
   fig

   
### year wise layoff
   

  

     
##usa layoff analysis

usa_layoff <- df%>%
  filter(country == "United States")

View(usa_layoff)


#location wise layoff in usa

location_usa <- usa_layoff%>%
  group_by(location)%>%
  summarise(total_layoff = sum(total_laid_off))%>%
  arrange(desc(total_layoff))%>%
  head(n = 10)

View(location_usa)

fig<- plot_ly(location_usa, labels = ~location, values = ~total_layoff, type = 'pie')
fig <- fig %>% layout(title = 'USA location wise layoff')

fig

#company in USA to layoff

company_usa <- usa_layoff%>%
  group_by(company)%>%
  summarise(total_layoff = sum(total_laid_off))%>%
  arrange(desc(total_layoff))%>%
  head(n = 5)

fig <- plot_ly(company_usa, x = ~ company, y = ~total_layoff, type = 'bar')
fig <- fig %>% layout(title = "Company wise layoff in USA",
                      xaxis = list(title = "Company"),
                      yaxis = list(title = "No. of Employee"))   
fig


### relationship btw fund recived and layoff in usa

fig <- plot_ly(usa_layoff, x = ~funds_raised, y = ~total_laid_off, type = 'scatter',
               color = ~total_laid_off, size = ~total_laid_off,
               text = ~company)
fig <- fig %>% layout(title = 'Layoff vs Funds Raised',
                      yaxis = list(title = "Fund Raised", zeroline = FALSE),
                      xaxis = list(title = "No. of Layoff", zeroline = FALSE))
fig


##India layoff analysis

india_layoff <- df%>%
  filter(country == "India")

View(india_layoff)


#location wise layoff in India

location_india <- india_layoff%>%
  group_by(location)%>%
  summarise(total_layoff = sum(total_laid_off))%>%
  arrange(desc(total_layoff))%>%
  head(n = 10)

View(location_india)

fig<- plot_ly(location_india, labels = ~location, values = ~total_layoff, type = 'pie')
fig <- fig %>% layout(title = "Location wise layoff in India")
fig


#company in india to layoff

company_india <- india_layoff%>%
  group_by(company)%>%
  summarise(total_layoff = sum(total_laid_off))%>%
  arrange(desc(total_layoff))%>%
  head(n = 5)

fig <- plot_ly(company_india, x = ~ company, y = ~total_layoff, type = 'bar')
fig <- fig %>% layout(title = "Company wise layoff in India",
                      xaxis = list(title = "Company"),
                      yaxis = list(title = "No. of employee"))   
fig


### relationship btw fund recived and layoff in india

fig <- plot_ly(india_layoff, x = ~funds_raised, y = ~total_laid_off, type = 'scatter',
               color = ~total_laid_off, size = ~total_laid_off,
               text = ~company)
fig <- fig %>% layout(title = 'Layoff vs Funds Raised',
                      yaxis = list(title = "Fund Raised", zeroline = FALSE),
                      xaxis = list(title = "No. of Layoff", zeroline = FALSE))
fig
  
  





