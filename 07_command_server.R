output$select_pitch_name3 <- renderUI({
  selectInput(inputId = "pitch_name3", 
              label = "球種選択: ", 
              choices = dplyr::bind_rows(data.frame(pitch_name="全球種"), 
                                         database() %>% 
                                           dplyr::select(pitch_name) %>% 
                                           dplyr::distinct(pitch_name)))
})

database07 <- reactive({
  if (input$pitch_name3=="全球種") {
    db <- database()
  } else {
    db <- database() %>% 
      dplyr::filter(pitch_name==input$pitch_name3)
  }
  db2 <- db %>% 
    dplyr::filter(description%in%c("ball","called_strike"), plate_z>0) %>% 
    dplyr::mutate(cs = ifelse(description=="ball", 0, 1), 
                  plate_x = plate_x/unit_convert(input$unit), 
                  plate_z = plate_z/unit_convert(input$unit), 
                  sz_top = sz_top/unit_convert(input$unit), 
                  sz_bot = sz_bot/unit_convert(input$unit), 
                  scaled_plate_z = case_when(plate_z>sz_top ~ plate_z-sz_top+1,
                                             plate_z<sz_bot ~ plate_z/sz_bot-2,
                                             TRUE ~ (plate_z-sz_bot)/(sz_top-sz_bot)*(1+1)-1))
  return(db2)
})

cs_prob_db_Both <- reactive({
  data.frame(expand.grid(plate_x = round(seq(-2, 2, length=401), 2), 
                         scaled_plate_z = round(seq(-2, 2, length=401), 2))) %>% 
    dplyr::mutate(lp = predict(gam(data=database07(), 
                                   formula=cs ~ s(plate_x, scaled_plate_z), 
                                   family=binomial(link="probit")), 
                               expand.grid(plate_x = round(seq(-2, 2, length=401), 2), 
                                           scaled_plate_z = round(seq(-2, 2, length=401), 2))), 
                  prob = exp(lp) / (1 + exp(lp))) %>% 
    dplyr::select(-lp)
})

cs_prob_db_Left <- reactive({
  data.frame(expand.grid(plate_x = round(seq(-2, 2, length=401), 2), 
                         scaled_plate_z = round(seq(-2, 2, length=401), 2))) %>% 
    dplyr::mutate(lp = predict(gam(data=database07() %>% 
                                     filter(stand=="L"), 
                                   formula=cs ~ s(plate_x, scaled_plate_z), 
                                   family=binomial(link="probit")), 
                               expand.grid(plate_x = round(seq(-2, 2, length=401), 2), 
                                           scaled_plate_z = round(seq(-2, 2, length=401), 2))), 
                  prob = exp(lp) / (1 + exp(lp))) %>% 
    dplyr::select(-lp)
})

cs_prob_db_Right <- reactive({
  data.frame(expand.grid(plate_x = round(seq(-2, 2, length=401), 2), 
                         scaled_plate_z = round(seq(-2, 2, length=401), 2))) %>% 
    dplyr::mutate(lp = predict(gam(data=database07() %>% 
                                     filter(stand=="R"), 
                                   formula=cs ~ s(plate_x, scaled_plate_z), 
                                   family=binomial(link="probit")), 
                               expand.grid(plate_x = round(seq(-2, 2, length=401), 2), 
                                           scaled_plate_z = round(seq(-2, 2, length=401), 2))), 
                  prob = exp(lp) / (1 + exp(lp))) %>% 
    dplyr::select(-lp)
})

output$cs_prob_Both <- renderPlot({
  ggplot() + 
    geom_segment(data = data.table(x=c(-0.83,-0.83,-0.83,0.83),xend=c(0.83,0.83,-0.83,0.83),y=c(-1,1,-1,-1),yend=c(-1,1,1,1)), 
                 mapping = aes(x=x, y=y, xend=xend, yend=yend), size=1, alpha=.8) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_hline(yintercept=-2) + 
    geom_contour(data = cs_prob_db_Both(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.8), size=1, linetype="solid") + 
    geom_contour(data = cs_prob_db_Both(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.5), size=1, linetype="dashed") + 
    geom_contour(data = cs_prob_db_Both(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.3), size=1, linetype="dotted") + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name3, "\n 通算ストライク判定確率密度"), 
         subtitle="実線：80%, 破線：50%, 点線：30%", x="横", y="高さ") + 
    xlim(-2, 2) + ylim(-2, 2) + 
    theme_cowplot(16)
})

output$cs_prob_Left <- renderPlot({
  ggplot() + 
    geom_segment(data = data.table(x=c(-0.83,-0.83,-0.83,0.83),xend=c(0.83,0.83,-0.83,0.83),y=c(-1,1,-1,-1),yend=c(-1,1,1,1)), 
                 mapping = aes(x=x, y=y, xend=xend, yend=yend), size=1, alpha=.8) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_hline(yintercept=-2) + 
    geom_contour(data = cs_prob_db_Left(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.8), size=1, linetype="solid") + 
    geom_contour(data = cs_prob_db_Left(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.5), size=1, linetype="dashed") + 
    geom_contour(data = cs_prob_db_Left(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.3), size=1, linetype="dotted") + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name3, "\n 対左ストライク判定確率密度"), 
         subtitle="実線：80%, 破線：50%, 点線：30%", x="横", y="高さ") + 
    xlim(-2, 2) + ylim(-2, 2) + 
    theme_cowplot(16)
})

output$cs_prob_Right <- renderPlot({
  ggplot() + 
    geom_segment(data = data.table(x=c(-0.83,-0.83,-0.83,0.83),xend=c(0.83,0.83,-0.83,0.83),y=c(-1,1,-1,-1),yend=c(-1,1,1,1)), 
                 mapping = aes(x=x, y=y, xend=xend, yend=yend), size=1, alpha=.8) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_hline(yintercept=-2) + 
    geom_contour(data = cs_prob_db_Right(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.8), size=1, linetype="solid") + 
    geom_contour(data = cs_prob_db_Right(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.5), size=1, linetype="dashed") + 
    geom_contour(data = cs_prob_db_Right(), 
                 mapping = aes(x=plate_x, y=scaled_plate_z, z=prob), 
                 breaks=c(.3), size=1, linetype="dotted") + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name3, "\n 対右ストライク判定確率密度"), 
         subtitle="実線：80%, 破線：50%, 点線：30%", x="横", y="高さ") + 
    xlim(-2, 2) + ylim(-2, 2) + 
    theme_cowplot(16)
})

output$cs_plot_Both <- renderPlot({
  ggplot() + 
    geom_segment(data = data.table(x=c(-0.83,-0.83,-0.83,0.83),xend=c(0.83,0.83,-0.83,0.83),y=c(-1,1,-1,-1),yend=c(-1,1,1,1)), 
                 mapping = aes(x=x, y=y, xend=xend, yend=yend), size=1, alpha=.8) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_hline(yintercept=-2) + 
    geom_point(data = database07(), 
               mapping = aes(x=plate_x, y=scaled_plate_z, color=description), 
               size=2, alpha=.6) + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name3, "\n 通算投球判定"), x="横", y="高さ") + 
    xlim(-2, 2) + ylim(-2, 2) + 
    theme_cowplot(16)
})

output$cs_plot_Left <- renderPlot({
  ggplot() + 
    geom_segment(data = data.table(x=c(-0.83,-0.83,-0.83,0.83),xend=c(0.83,0.83,-0.83,0.83),y=c(-1,1,-1,-1),yend=c(-1,1,1,1)), 
                 mapping = aes(x=x, y=y, xend=xend, yend=yend), size=1, alpha=.8) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_hline(yintercept=-2) + 
    geom_point(data = database07() %>% 
                 dplyr::filter(stand=="L"), 
               mapping = aes(x=plate_x, y=scaled_plate_z, color=description), 
               size=2, alpha=.6) + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name3, "\n 対左投球判定"), x="横", y="高さ") + 
    xlim(-2, 2) + ylim(-2, 2) + 
    theme_cowplot(16)
})

output$cs_plot_Right <- renderPlot({
  ggplot() + 
    geom_segment(data = data.table(x=c(-0.83,-0.83,-0.83,0.83),xend=c(0.83,0.83,-0.83,0.83),y=c(-1,1,-1,-1),yend=c(-1,1,1,1)), 
                 mapping = aes(x=x, y=y, xend=xend, yend=yend), size=1, alpha=.8) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + geom_hline(yintercept=-2) + 
    geom_point(data = database07() %>% 
                 dplyr::filter(stand=="R"), 
               mapping = aes(x=plate_x, y=scaled_plate_z, color=description), 
               size=2, alpha=.6) + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name3, "\n 対右投球判定"), x="横", y="高さ") + 
    xlim(-2, 2) + ylim(-2, 2) + 
    theme_cowplot(16)
})