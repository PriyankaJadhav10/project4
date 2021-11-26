library(shiny)
library(ggplot2)
library(shinydashboard)


shinyServer(function(input,output)
  {
    output$geom<-renderPlot({
      
      ggplot(runs_o,aes(x=season,y=avg,colour=over,group=over))+
        geom_line(show.legend = TRUE, size =1.25,linetype=1)+
        theme(legend.position="bottom")+
        theme(legend.direction = "horizontal") +
        facet_wrap(~inning)+
        scale_y_continuous(name="average runs per ball")+
        scale_x_discrete(name="season",limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Average runs per ball by over each season and innings")
    })
    output$win<-renderPlot({
      matches$toss_match<-ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost")
      ggplot(matches[which(!is.na(matches$toss_match)),],aes(toss_match, fill = toss_match))+ 
        geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")
      
    })
    output$run<-renderPlot({
      ggplot(dismissal,aes(x=dismissal_kind,y=wickets,colour=as.factor(season),fill=as.factor(season)))+
        geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
        theme(legend.position="bottom")+
        coord_flip()+
        theme(legend.direction = "horizontal") +
        scale_y_continuous(name="wickets")+
        scale_x_discrete(name="dismissal kind")+
        ggtitle("Breakdown of dismissal type")
    })
       
    output$tr<-renderPlot({
      ggplot(batting_TW,aes(x=season,y=runs,colour=batting_team,fill=batting_team))+
        geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
        geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
        theme(legend.position="none")+
        coord_flip()+
        scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Total Runs of Tournament winners season wise")
    })  
      
    output$twks<-renderPlot({
      ggplot(bowling_TW,aes(x=season,y=wicket,colour=bowling_team,fill=bowling_team))+
        geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
        geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
        theme(legend.position="none")+
        coord_flip()+
        scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Total wickets of Tournament winners by season wise")
    })
    
    output$hrs<-renderPlot({
      ggplot(bastmen,aes(x=season,y=runs,colour=batsman,fill=batsman))+
        geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
        geom_text(aes(label=batsman,hjust=-.25, colour="green"))+
        theme(legend.position="none")+
        coord_flip()+
        scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Highest run scorers season wise")
    })
    
    output$hwt<-renderPlot({
      ggplot(bowler,aes(x=season,y=wicket,colour=bowler,fill=bowler))+
        geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
        geom_text(aes(label=bowler,hjust=-.25, colour="green"))+
        theme(legend.position="none")+
        coord_flip()+
        scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Highest wickter takers  season wise")
    })
    
    output$trs<-renderPlot({
      ggplot(team_runs,aes(x=season,y=runs,colour=s_team,fill=s_team))+
        geom_line(show.legend = TRUE, size =1.25,linetype=1)+
        theme(legend.position="bottom")+
        theme(legend.direction = "horizontal") +
        scale_y_continuous(limits = c(1500,3000))+
        scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Total runs by teams season wise")
    })
    
    output$twk<-renderPlot({
      ggplot(team_wickets,aes(x=season,y=wicket,colour=s_team,fill=s_team))+
        geom_line(show.legend = TRUE, size =1.25,linetype=1)+
        theme(legend.position="bottom")+
        theme(legend.direction = "horizontal") +
        scale_y_continuous(limits = c(60,125))+
        scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
        ggtitle("Total wickets by teams season wise")
    })
    
    output$rs<-renderPlot({
      ggplot(runs_cat,aes(x=s_team,y=runs,colour=batsman_runs,fill=batsman_runs))+
        geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
        theme(legend.position="bottom")+
        theme(legend.direction = "horizontal") +
        scale_y_continuous(name="Runs")+
        scale_x_discrete(name="Teams")+
        ggtitle("Total runs scored in 1s to 6s")
    })
    
    output$tam<-renderPlot({
      ggplot(toss_w,aes(x=s_team,y=wins,colour=type,fill=type))+
        geom_bar(position = "dodge",stat = "identity")+
        theme(legend.position="right")+
        scale_y_continuous(name="Toss and Match Wins")+
        scale_x_discrete(name="Toss and Match winner")+
        ggtitle("Toss and Match wins by each Team")
      
    })
    
    output$td<-renderPlot({
      ggplot(wins_1,aes(x=s_team,y=wins,colour=toss_decision,fill=toss_decision))+
        geom_bar(position = "dodge",stat = "identity")+
        theme(legend.position="right")+
        scale_y_continuous(name="Toss decision")+
        scale_x_discrete(name="Toss winners and toss decisions")+
        ggtitle("Toss decisions by each Team")
    })
    
    output$content <- reactive({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
    })
    
    output$dt<-renderPlot({
      rpart.plot(fit, extra = 106)
  
    })
    
    output$wordcloud=renderPlot({
      player_of_matchCorpus <- Corpus(VectorSource(matches$player_of_match))
      wordcloud(player_of_matchCorpus, min.freq = 100, max.words = 200, scale=c(3,1), colors=brewer.pal(8,"Dark2"), random.order = FALSE)
      
      
    })
    output$dtt<-renderPlot({
      rpart.plot(fitt, extra = 106)

      
    })
    output$acc<-renderText({
      print(paste('Accuracy for test', accuracy_t))  
      
    })
    
    
})
