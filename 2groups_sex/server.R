server = function(input, output)({
  group_1 = c("thc")
  group_2 = c("veh")
  
  #----------------------------------------
  # Reactive data sets ---- 
  reactive.data1 <- reactive({
    data_summary(FR_data, varname=input$y, groupnames=c("session", "group", "fr", "sex"))
    #data_summary(FR_data, varname=input$y, groupnames=c("session", "group", "fr"))
  })
  
  reactive.data2<- reactive({
    subset(reactive.data1(),
           session >= input$session[1] & session <=input$session[2])
  })
  
  reactive.data3<- reactive({
    subset(FR_data, group==group_1)
  })
  reactive.data4<- reactive({
    subset(FR_data, group==group_2)
  })
  
  # reactive.data5<- reactive({
  #   data_summary(reactive.data3, varname=input$y, groupnames="subject")
  # })
  # 
  # reactive.data15<- reactive({
  #   data_summary(reactive.data4, varname=input$y, groupnames="subject")
  # })
  
  reactive.data6<- reactive({
    select(FR_data, subject, session, group, boxnum, fr, active, trial.active, timeout.active, inactive, reward, sex) %>% subset(group==group_1 & sex=="f")
  })
  
  reactive.data7<- reactive({
    select(FR_data, subject, session, group, boxnum, fr, active, trial.active, timeout.active, inactive, reward, sex) %>% subset(group==group_2 & sex=="f")
  })
  
  reactive.data8<- reactive({
    select(FR_data, subject, session, group, boxnum, fr, active, trial.active, timeout.active, inactive, reward, sex) %>% subset(group==group_1 & sex=="m")
  })
  
  reactive.data11<- reactive({
    select(FR_data, subject, session, group, boxnum, fr, active, trial.active, timeout.active, inactive, reward, sex) %>% subset(group==group_2 & sex=="m") 
  })
  
  reactive.data9<- reactive({
    keep4    <- subset(FR_data, group==group_1 & sex=="f")[ vals4$keeprows4, , drop = FALSE]
    keep5    <- subset(FR_data, group==group_2 & sex=="f")[ vals5$keeprows5, , drop = FALSE]
    keep6    <- subset(FR_data, group==group_1 & sex=="m")[ vals6$keeprows6, , drop = FALSE]
    keep7    <- subset(FR_data, group==group_2 & sex=="m")[ vals7$keeprows7, , drop = FALSE]
    exclude_final<-rbind(keep4,keep5, keep6, keep7)
    level1_9<-tidyr::gather(exclude_final, lever, presses, c(active, inactive)) 
    level1_9<-with(level1_9, level1_9[order(session, subject),])
  })
  
  reactive.data10<- reactive({
    lmer(presses ~ session*lever*group*sex + (1|subject), data=reactive.data9())
  })
  
  reactive.data13<- reactive({
    keep4    <- subset(FR_data, group==group_1 & sex=="f")[ vals4$keeprows4, , drop = FALSE]
    keep5    <- subset(FR_data, group==group_2 & sex=="f")[ vals5$keeprows5, , drop = FALSE]
    keep6    <- subset(FR_data, group==group_1 & sex=="m")[ vals6$keeprows6, , drop = FALSE]
    keep7    <- subset(FR_data, group==group_2 & sex=="m")[ vals7$keeprows7, , drop = FALSE]
    exclude_final<-rbind(keep4,keep5, keep6, keep7)
    level1_9<-tidyr::gather(exclude_final, lever, presses, c(active, inactive)) 
    level1_9<-with(level1_9, level1_9[order(session, subject),])
    level1_9$session<- factor(level1_9$session)
    return(level1_9)
  })
  
  reactive.data14<- reactive({
    lmer(presses ~ session*lever*group*sex + (1|subject), data=reactive.data13())
  })
  
  reactive.data16<- reactive({
    keep4    <- subset(FR_data, group==group_1 & sex=="f")[ vals4$keeprows4, , drop = FALSE]
    keep5    <- subset(FR_data, group==group_2 & sex=="f")[ vals5$keeprows5, , drop = FALSE]
    keep6    <- subset(FR_data, group==group_1 & sex=="m")[ vals6$keeprows6, , drop = FALSE]
    keep7    <- subset(FR_data, group==group_2 & sex=="m")[ vals7$keeprows7, , drop = FALSE]
    exclude_final<-rbind(keep4,keep5, keep6, keep7)
    level1_9<-tidyr::gather(exclude_final, lever, presses, c(active, inactive)) 
    level1_9<-with(level1_9, level1_9[order(session, subject),])
    data15_1<-unite(level1_9, session, lever, col="session_lever", sep="_")
    data15_1<-dplyr::select(data15_1, subject, group, sex, session_lever, presses)
    data15_1$session_lever<-data15_1$session_lever %>% factor() %>% fct_inorder()
    data16_1<-spread(data15_1, session_lever, presses)
    group<- data16_1$group
    subject<- data16_1$subject
    sex<- data16_1$sex
    data16_1$group<-NULL
    data16_1$subject<-NULL
    data16_1$sex<-NULL
    data16_1<-t(apply(data16_1, 1, function(x) c(x[is.na(x)], x[!is.na(x)])))
    data16_1<-data16_1[,apply(data16_1, 2, function(x) !any(is.na(x)))]
    data16_1<-data16_1[,(ncol(data16_1)-5):ncol(data16_1)]
    colnames(data16_1)<-c("x_active", "x_inactive", "y_active", "y_inactive", "z_active", "z_inactive")
    data16_1<-as.data.frame(data16_1)
    data16_1<- cbind(subject, group, sex, data16_1)
    data16_1<- gather(data16_1, "session_lever", "presses", x_active:z_inactive)
    data16_1<- separate(data16_1, session_lever, c("session", "lever"), sep="_")
    data16_1$presses<- as.numeric(data16_1$presses)
    return(data16_1)
  })
  
  reactive.data12<- reactive({
    lmer(presses ~ session*lever*group*sex + (session|subject), data=reactive.data16())
  })
  
  vals4 <- reactiveValues(
    keeprows4 = rep(TRUE, nrow(subset(FR_data, group==group_1 & sex=="f")))
  )
  
  vals5 <- reactiveValues(
    keeprows5 = rep(TRUE, nrow(subset(FR_data, group==group_2 & sex=="f")))
  )
  
  vals6 <- reactiveValues(
    keeprows6 = rep(TRUE, nrow(subset(FR_data, group==group_1 & sex=="m")))
  )
  
  vals7 <- reactiveValues(
    keeprows7 = rep(TRUE, nrow(subset(FR_data, group==group_2 & sex=="m")))
  )        
  
  download.plot<- reactive({
    ggplot(reactive.data2(), aes(x=session, y= mean, group = interaction(group,sex), color=interaction(group,sex))) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(0.05)) +
      geom_point(aes(shape=fr), size = 5) +
      geom_line(aes(linetype=interaction(group,sex)), size=1) +
      ggtitle(input$title) +
      ylab(input$y) +
      theme_classic()
  })
  
  #----------------------------------------     
  # Plots 
  
  # Main plots ----
  output$plot<- renderPlot({
    p <- ggplot(reactive.data2(), aes(x=session, y= mean, color=sex)) + #group = interaction(group,sex),color=interaction(group,sex)
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05, size=1) +
      geom_point(aes(shape=fr), size = 5) +
      geom_line(aes(linetype=group), size=1) +
      #ggtitle(input$title) +
      ylab(input$y) +
      scale_shape_discrete(name="FR Ratio Schedule") +
      theme_classic()
    p
  })
  
  output$plot2<- renderPlot({
    q<- ggplot(reactive.data3(), aes_string(y=input$y, x="session", group = "sex", color="sex")) +
      geom_line() +
      geom_point(aes(shape=fr), size=3) +
      facet_wrap( ~ subject) +
      scale_shape_discrete(name="FR Ratio Schedule") +
      theme_bw()
    q
  }, height=1000)
  
  output$plot3<- renderPlot({
    q<- ggplot(reactive.data4(), aes_string(y=input$y, x="session", group = "sex", color="sex")) +
      geom_line() +
      geom_point(aes(shape=fr), size=3) +
      facet_wrap(~ subject) +
      scale_shape_discrete(name="FR Ratio Schedule") +
      theme_bw()
    q
  }, height=1000)
  
  output$plot4<- renderPlot({
    keep4    <- subset(FR_data, group==group_1 & sex=="f")[ vals4$keeprows4, , drop = FALSE]
    ggplot(keep4, aes_string(y=input$y, x="session")) +
      geom_boxplot(aes(fill=factor(session))) +
      geom_point() +
      labs(y=input$y, x="session") +
      theme_bw()
  })
  output$plot5<- renderPlot({
    keep5    <- subset(FR_data, group==group_2 & sex=="f")[ vals5$keeprows5, , drop = FALSE]
    ggplot(keep5, aes_string(y=input$y, x="session")) +  
      geom_boxplot(aes(fill=factor(session))) +
      geom_point() +
      labs(y=input$y, x="session") +
      theme_bw()
  })
  
  output$plot6<- renderPlot({
    keep6    <- subset(FR_data, group==group_1 & sex=="m")[ vals6$keeprows6, , drop = FALSE]
    ggplot(keep6, aes_string(y=input$y, x="session")) +
      geom_boxplot(aes(fill=factor(session))) +
      geom_point() +
      labs(y=input$y, x="session") +
      theme_bw()
  })
  output$plot7<- renderPlot({
    keep7    <- subset(FR_data, group==group_2 & sex=="m")[ vals7$keeprows7, , drop = FALSE]
    ggplot(keep7, aes_string(y=input$y, x="session")) +  
      geom_boxplot(aes(fill=factor(session))) +
      geom_point() +
      labs(y=input$y, x="session") +
      theme_bw()
  })
  
  output$int_plot<- renderPlot({
    emmip(reactive.data14(), group + sex ~ session | lever)
  })
  
  output$int_plot2<- renderPlot({
    emmip(reactive.data12(), group + sex ~ session | lever)
  })
  
  output$residualplot3<- renderPlot({
    qqnorm(residuals(reactive.data12()))
    qqline(residuals(reactive.data12()))
  })
  
  output$residualplot4<- renderPlot({
    plot(reactive.data12(), resid(., scaled=TRUE) ~ presses | sex*group)
  })
  
  output$residualplot1<- renderPlot({
    qqnorm(residuals(reactive.data10()))
    qqline(residuals(reactive.data10()))
  })
  
  output$residualplot2<- renderPlot({
    plot(reactive.data10(), resid(., scaled=TRUE) ~ presses | sex*group)
  })
  
  #---------------------------------------- 
  # Render Prints ----
  
  output$hover_info1<- renderPrint({
    nearPoints(reactive.data6(), input$plot_hover1, xvar = "session", yvar = input$y)
  })
  
  output$hover_info2<- renderPrint({
    nearPoints(reactive.data7(), input$plot_hover2, xvar = "session", yvar = input$y)
  })
  
  output$hover_info3<- renderPrint({
    nearPoints(reactive.data8(), input$plot_hover3, xvar = "session", yvar = input$y)
  })
  
  output$hover_info4<- renderPrint({
    nearPoints(reactive.data11(), input$plot_hover4, xvar = "session", yvar = input$y)
  })     
  
  output$stats_wald2<-renderPrint({
    Anova(lmer(presses ~ session*lever*group*sex + (1|subject), data=reactive.data9()), type=2)
  })
  
  output$stats_wald3<-renderPrint({
    Anova(lmer(presses ~ session*lever*group*sex + (1|subject), data=reactive.data9()), type=3)
  })
  
  # output$stats_s2<-renderPrint({
  #   anova(lmer(presses ~ session*lever*group*sex + (group|subject), data=reactive.data9()), type=2)
  # })
  # 
  # output$stats_s3<-renderPrint({
  #   anova(lmer(presses ~ session*lever*group*sex + (group|subject), data=reactive.data9()), type=3)
  # })
  
  output$stats_tukey<-renderPrint({
    TukeyHSD(x=aov(presses ~ group + lever + sex, data=reactive.data9()), conf.level=0.95)
  })
  
  
  output$last3sessions_anova3<-renderPrint({
    Anova(lmer(presses ~ session*lever*group*sex + (session|subject), data=reactive.data16()), type=3)
    #aov(presses~group*lever*session + Error(subject/session), data=reactive.data16())
  })
  
  output$last3sessions_anova2<-renderPrint({
    Anova(lmer(presses ~ session*lever*group*sex + (session|subject), data=reactive.data16()), type=2)
    #aov(presses~group*lever*session + Error(subject/session), data=reactive.data16())
  })
  
  output$stats_tukey2<-renderPrint({
    TukeyHSD(x=aov(presses ~ group + lever + sex, data=reactive.data16()), conf.level=0.95)
  })
  
  output$contrasts1<- renderPrint({
    emmeans(reactive.data10(), pairwise ~ lever + group + sex)
  })
  
  output$contrasts2<- renderPrint({
    emmeans(reactive.data12(), pairwise ~ lever + group + sex)
  })
  
  
  #----------------------------------------           
  # Render Tables ----
  
  output$master.data<- DT::renderDataTable({
    DT::datatable(FR_data, filter='top', options=list(pagelength=25), class = 'cell-border stripe')
    })
          
  output$table<- DT::renderDataTable({
    DT::datatable(reactive.data2(), filter='top', options=list(pagelength=25), class = 'cell-border stripe')
  })
  
  #---------------------------------------- 
  # Download Handlers ----
  # output$downloadplot<- downloadHandler(
  #   filename = function() { paste(input$y, '.png', sep = '')},
  #   content = function(file) {
  #     ggsave(file, plot=download.plot(), device="png")
  #   })
  
  output$downloaddata<- downloadHandler(
    filename = function(){ paste0("data.", input$filetype, sep = '')},
    content = function(file){
      if(input$filetype == "csv"){ 
        write_csv(FR_data, file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(FR_data, file) 
      }
      if(input$filetype =="xlsx"){
        #write_excel_csv(FR_data, file)
        write.xlsx(FR_data, file)
      }
    })
  
  
  
  
  #---------------------------------------- 
  # Toggle points that are clicked ----
  
  observeEvent(input$plot_click1, {
    res4 <- nearPoints(subset(FR_data, group==group_1 & sex=="f"), input$plot_click1, allRows = TRUE)
    vals4$keeprows4 <- xor(vals4$keeprows4, res4$selected_)
    
    res5 <- nearPoints(subset(FR_data, group==group_2 & sex=="f"), input$plot_click2, allRows = TRUE)
    vals5$keeprows5 <- xor(vals5$keeprows5, res5$selected_)
    
    res6 <- nearPoints(subset(FR_data, group==group_1 & sex=="m"), input$plot_click3, allRows = TRUE)
    vals6$keeprows6 <- xor(vals6$keeprows6, res6$selected_)
    
    res7 <- nearPoints(subset(FR_data, group==group_2 & sex=="m"), input$plot_click4, allRows = TRUE)
    vals7$keeprows7 <- xor(vals7$keeprows7, res7$selected_)
    
  })
  
  
  #---------------------------------------- 
  # Toggle points that are brushed, when button is clicked ----
  observeEvent(input$exclude_toggle, {
    res4 <- brushedPoints(subset(FR_data, group==group_1 & sex=="f"), input$plot_brush1, allRows = TRUE)
    vals4$keeprows4 <- xor(vals4$keeprows4, res4$selected_)
    
    res5 <- brushedPoints(subset(FR_data, group==group_2 & sex=="f"), input$plot_brush2, allRows = TRUE)
    vals5$keeprows5 <- xor(vals5$keeprows5, res5$selected_)
    
    res6 <- brushedPoints(subset(FR_data, group==group_1 & sex=="m"), input$plot_brush3, allRows = TRUE)
    vals6$keeprows6 <- xor(vals6$keeprows6, res6$selected_)
    
    res7 <- brushedPoints(subset(FR_data, group==group_2 & sex=="m"), input$plot_brush4, allRows = TRUE)
    vals7$keeprows7 <- xor(vals7$keeprows7, res7$selected_) 
  })
  
  #----------------------------------------      
  # Reset all toggle points ----
  observeEvent(input$exclude_reset, {
    vals4$keeprows4 <- rep(TRUE, nrow(subset(FR_data, group==group_1 & sex=="f")))
    vals5$keeprows5 <- rep(TRUE, nrow(subset(FR_data, group==group_2 & sex=="f")))
    vals6$keeprows6 <- rep(TRUE, nrow(subset(FR_data, group==group_1 & sex=="m")))
    vals7$keeprows7 <- rep(TRUE, nrow(subset(FR_data, group==group_2 & sex=="m")))
    
    
  })
  
  
})

shinyApp(ui = ui,server = server)
