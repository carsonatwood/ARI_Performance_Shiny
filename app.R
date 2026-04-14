library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(plotly)

bw   <- read_excel("Individual_Creative_Component_Data.xlsx", sheet = "BW")
gain <- read_excel("Individual_Creative_Component_Data.xlsx", sheet = "Gain")
fcr  <- read_excel("Individual_Creative_Component_Data.xlsx", sheet = "FCR")
org  <- read_excel("Individual_Creative_Component_Data.xlsx", sheet = "Organ_Weights")

ari_levels <- c("Low", "Medium", "High")
bw$ARI   <- factor(bw$ARI,   levels = ari_levels)
gain$ARI <- factor(gain$ARI, levels = ari_levels)
fcr$ARI  <- factor(fcr$ARI,  levels = ari_levels)
org$ARI  <- factor(org$ARI,  levels = ari_levels)

bw_long <- bw %>%
    pivot_longer(starts_with("d"), names_to = "Day", values_to = "BW") %>%
    mutate(Day = as.integer(sub("d", "", Day)))

int_levels <- c("d0_d7","d7_d14","d14_d21","d21_d28","d28_d35","d35_d42")
int_labels <- c("0-7","7-14","14-21","21-28","28-35","35-42")

gain_long <- gain %>%
    pivot_longer(-c(ID, ARI), names_to = "Interval", values_to = "Gain") %>%
    mutate(Interval = factor(Interval, levels = int_levels, labels = int_labels))

fcr_long <- fcr %>%
    pivot_longer(-c(ID, ARI), names_to = "Interval", values_to = "FCR") %>%
    mutate(Interval = factor(Interval, levels = int_levels, labels = int_labels))

org_long <- org %>%
    pivot_longer(-c(ID, ARI), names_to = "Organ", values_to = "Pct") %>%
    mutate(Organ = sub(",.*", "", Organ))

ari_cols <- c("Low" = "#2A6EBB", "Medium" = "#E07B39", "High" = "#3DAA6D")

theme_clean <- function() {
    theme_minimal(base_size = 13, base_family = "sans") +
        theme(
            plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
            panel.background = element_rect(fill = "#FFFFFF", colour = NA),
            panel.grid.major = element_line(colour = "#EBEBEB", linewidth = 0.4),
            panel.grid.minor = element_blank(),
            axis.line        = element_line(colour = "#CCCCCC", linewidth = 0.4),
            axis.ticks       = element_line(colour = "#CCCCCC", linewidth = 0.4),
            axis.title       = element_text(colour = "#444444", size = 12),
            axis.text        = element_text(colour = "#666666", size = 11),
            legend.position  = "bottom",
            legend.title     = element_text(colour = "#444444", size = 11, face = "bold"),
            legend.text      = element_text(colour = "#555555", size = 11),
            legend.key.size  = unit(0.9, "lines"),
            plot.margin      = margin(14, 18, 10, 14)
        )
}

app_css <- "
  * { box-sizing: border-box; margin: 0; padding: 0; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    background: #F0F2F5;
    color: #222;
  }

  .app-header {
    background: #1A2E44;
    color: #fff;
    padding: 16px 28px;
    display: flex;
    align-items: baseline;
    gap: 14px;
    border-bottom: 3px solid #2A6EBB;
  }
  .app-header h1 { font-size: 18px; font-weight: 600; letter-spacing: .2px; }
  .app-header span { font-size: 12px; color: #7A9FBF; font-weight: 400; }

  .app-body { display: flex; min-height: calc(100vh - 55px); }

  .app-sidebar {
    width: 210px; min-width: 210px;
    background: #1A2E44;
    padding: 20px 0;
    display: flex; flex-direction: column; gap: 1px;
  }
  .sidebar-section {
    padding: 14px 18px 6px;
    font-size: 10px; font-weight: 600;
    letter-spacing: 1.2px; color: #4A6E8E;
    text-transform: uppercase;
  }
  .nav-btn {
    display: block; width: 100%;
    background: none; border: none;
    padding: 9px 18px;
    text-align: left; font-family: inherit;
    font-size: 13px; font-weight: 400;
    color: #96BAD4; cursor: pointer;
    border-left: 3px solid transparent;
    transition: all .12s;
  }
  .nav-btn:hover { background: #1F3650; color: #fff; }
  .nav-btn.active { background: #1E3A58; color: #fff; border-left-color: #2A6EBB; font-weight: 500; }

  .sidebar-divider { height: 1px; background: #243C57; margin: 12px 18px; }
  .sidebar-label {
    padding: 0 18px 5px;
    font-size: 10px; font-weight: 600;
    letter-spacing: 1px; color: #4A6E8E;
    text-transform: uppercase;
  }
  .ari-check { padding: 4px 18px; }
  .ari-check label { display: flex; align-items: center; gap: 8px; font-size: 13px; color: #96BAD4; cursor: pointer; }
  .ari-check input[type=checkbox] { accent-color: #2A6EBB; width: 13px; height: 13px; }

  .app-main { flex: 1; padding: 26px 28px; overflow-y: auto; }

  .page-title { font-size: 17px; font-weight: 600; color: #1A2E44; margin-bottom: 3px; }
  .page-sub   { font-size: 12px; color: #999; margin-bottom: 20px; }

  .stat-row { display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; margin-bottom: 18px; }
  .stat-box {
    background: #fff; border-radius: 5px;
    padding: 16px 18px;
    box-shadow: 0 1px 3px rgba(0,0,0,.06);
  }
  .stat-box .lbl { font-size: 10px; font-weight: 600; letter-spacing: .6px; color: #999; text-transform: uppercase; }
  .stat-box .val { font-size: 24px; font-weight: 600; color: #1A2E44; line-height: 1.25; margin-top: 3px; }
  .stat-box .sub { font-size: 11px; color: #bbb; margin-top: 1px; }

  .card {
    background: #fff; border-radius: 5px;
    padding: 18px 20px 14px;
    box-shadow: 0 1px 3px rgba(0,0,0,.06);
    margin-bottom: 18px;
  }
  .card-title { font-size: 12px; font-weight: 600; color: #333; letter-spacing: .2px; text-transform: uppercase; margin-bottom: 2px; }
  .card-sub   { font-size: 11px; color: #AAA; margin-bottom: 12px; }

  .two-col { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; margin-bottom: 18px; }

  .ctrl-row { display: flex; align-items: flex-end; gap: 24px; margin-bottom: 14px; flex-wrap: wrap; }
  .ctrl-group { display: flex; flex-direction: column; gap: 5px; }
  .ctrl-group > .lbl2 { font-size: 11px; font-weight: 600; color: #555; letter-spacing: .3px; text-transform: uppercase; }
  .radio-group { display: flex; gap: 14px; }
  .radio-group label { display: flex; align-items: center; gap: 5px; font-size: 12px; color: #444; cursor: pointer; }
  .radio-group input[type=radio] { accent-color: #2A6EBB; }

  select.styled {
    border: 1px solid #DDD; border-radius: 4px;
    padding: 6px 10px; font-size: 12px;
    font-family: inherit; color: #333; background: #fff; outline: none;
  }
  select.styled:focus { border-color: #2A6EBB; }
"

ui <- fluidPage(
    tags$head(
        tags$style(HTML(app_css)),
        tags$script(HTML("
      function setTab(tab) {
        Shiny.setInputValue('active_tab', tab, {priority: 'event'});
        document.querySelectorAll('.nav-btn').forEach(b => b.classList.remove('active'));
        var btn = document.getElementById('btn_' + tab);
        if (btn) btn.classList.add('active');
      }
    "))
    ),
    
    div(class="app-header",
        tags$h1("Broiler Performance Dashboard"),
        tags$span("Social Rank Study Performance Data")
    ),
    
    div(class="app-body",
        
        div(class="app-sidebar",
            div(class="sidebar-section", "Sections"),
            tags$button(id="btn_overview", class="nav-btn active",
                        onclick="setTab('overview')", "Overview"),
            tags$button(id="btn_bw", class="nav-btn",
                        onclick="setTab('bw')", "Body Weight"),
            tags$button(id="btn_gain", class="nav-btn",
                        onclick="setTab('gain')", "Weight Gain"),
            tags$button(id="btn_fcr", class="nav-btn",
                        onclick="setTab('fcr')", "Feed Conv. Ratio"),
            tags$button(id="btn_organ", class="nav-btn",
                        onclick="setTab('organ')", "Organ Weights"),
            div(class="sidebar-divider"),
            div(class="sidebar-label", "ARI Filter"),
            div(class="ari-check",
                tags$label(tags$input(type="checkbox", id="ck_Low", checked=NA,
                                      onchange="Shiny.setInputValue('ck_Low',this.checked)"), "Low")),
            div(class="ari-check",
                tags$label(tags$input(type="checkbox", id="ck_Medium", checked=NA,
                                      onchange="Shiny.setInputValue('ck_Medium',this.checked)"), "Medium")),
            div(class="ari-check",
                tags$label(tags$input(type="checkbox", id="ck_High", checked=NA,
                                      onchange="Shiny.setInputValue('ck_High',this.checked)"), "High"))
        ),
        
        div(class="app-main", uiOutput("page_content"))
    )
)

server <- function(input, output, session) {
    
    sel_ari <- reactive({
        ck <- c(
            if (isTRUE(input$ck_Low))    "Low",
            if (isTRUE(input$ck_Medium)) "Medium",
            if (isTRUE(input$ck_High))   "High"
        )
        if (length(ck) == 0) ari_levels else ck
    })
    
    r_bw_long   <- reactive({ bw_long   %>% filter(ARI %in% sel_ari()) })
    r_gain_long <- reactive({ gain_long %>% filter(ARI %in% sel_ari()) })
    r_fcr_long  <- reactive({ fcr_long  %>% filter(ARI %in% sel_ari()) })
    r_org_long  <- reactive({ org_long  %>% filter(ARI %in% sel_ari()) })
    
    output$page_content <- renderUI({
        tab <- if (is.null(input$active_tab)) "overview" else input$active_tab
        switch(tab,
               "overview" = overview_ui(),
               "bw"       = bw_ui(),
               "gain"     = gain_ui(),
               "fcr"      = fcr_ui(),
               "organ"    = organ_ui()
        )
    })
    
    overview_ui <- function() tagList(
        p(class="page-title", "Overview"),
        p(class="page-sub",   "Summary data across all ARI groups"),
        div(class="stat-row",
            div(class="stat-box",
                div(class="lbl", "Birds Selected"),
                div(class="val", textOutput("stat_n", inline=TRUE)),
                div(class="sub", "overall")),
            div(class="stat-box",
                div(class="lbl", "Average Final Body Weight"),
                div(class="val", textOutput("stat_bw", inline=TRUE)),
                div(class="sub", "kg")),
            div(class="stat-box",
                div(class="lbl", "Overall Average FCR"),
                div(class="val", textOutput("stat_fcr", inline=TRUE)),
                div(class="sub", "across all intervals"))
        ),
        div(class="card",
            div(class="card-title", "Average Body Weight Over Time"),
            div(class="card-sub",   ""),
            plotlyOutput("ov_bw", height=340)
        ),
        div(class="two-col",
            div(class="card",
                div(class="card-title", "Average Weight Gain by Interval"),
                div(class="card-sub", ""),
                plotlyOutput("ov_gain", height=270)),
            div(class="card",
                div(class="card-title", "Average FCR by Interval"),
                div(class="card-sub", ""),
                plotlyOutput("ov_fcr", height=270))
        )
    )
    
    bw_ui <- function() tagList(
        p(class="page-title", "Body Weight"),
        p(class="page-sub",   "Mean body weight trajectories by ARI group"),
        div(class="card",
            div(class="ctrl-row",
                div(class="ctrl-group",
                    div(class="lbl2", "Chart Type"),
                    div(class="radio-group",
                        tags$label(tags$input(type="radio", name="bw_type", value="line", checked=NA,
                                              onchange="Shiny.setInputValue('bw_type','line')"), "Mean Line"),
                        tags$label(tags$input(type="radio", name="bw_type", value="bar",
                                              onchange="Shiny.setInputValue('bw_type','bar')"), "Bar Chart")
                    )
                )
            ),
            div(class="card-title", "Body Weight (kg) by Day"),
            div(class="card-sub",   "Mean \u00b1 SD"),
            plotlyOutput("bw_main", height=400)
        )
    )
    
    gain_ui <- function() tagList(
        p(class="page-title", "Weight Gain"),
        p(class="page-sub",   "Weekly weight gain between measurement intervals"),
        div(class="card",
            div(class="ctrl-row",
                div(class="ctrl-group",
                    div(class="lbl2", "Chart Type"),
                    div(class="radio-group",
                        tags$label(tags$input(type="radio", name="gain_type", value="line", checked=NA,
                                              onchange="Shiny.setInputValue('gain_type','line')"), "Mean Line"),
                        tags$label(tags$input(type="radio", name="gain_type", value="bar",
                                              onchange="Shiny.setInputValue('gain_type','bar')"), "Bar Chart")
                    )
                )
            ),
            div(class="card-title", "Weight Gain (kg) by Interval"),
            div(class="card-sub",   "Mean \u00b1 SD  \u00b7  Interval shown in days"),
            plotlyOutput("gain_main", height=400)
        )
    )
    
    fcr_ui <- function() tagList(
        p(class="page-title", "Feed Conversion Ratio"),
        p(class="page-sub",   "Lower FCR = more efficent"),
        div(class="card",
            div(class="ctrl-row",
                div(class="ctrl-group",
                    div(class="lbl2", "Chart Type"),
                    div(class="radio-group",
                        tags$label(tags$input(type="radio", name="fcr_type", value="line", checked=NA,
                                              onchange="Shiny.setInputValue('fcr_type','line')"), "Mean Line"),
                        tags$label(tags$input(type="radio", name="fcr_type", value="bar",
                                              onchange="Shiny.setInputValue('fcr_type','bar')"), "Bar Chart")
                    )
                )
            ),
            div(class="card-title", "FCR by Interval"),
            div(class="card-sub",   "Mean \u00b1 SD  \u00b7  Interval shown in days"),
            plotlyOutput("fcr_main", height=400)
        )
    )
    
    organ_ui <- function() tagList(
        p(class="page-title", "Organ Weights"),
        p(class="page-sub",   "Organ weight as percentage of body weight at d42"),
        div(class="card",
            div(class="ctrl-row",
                div(class="ctrl-group",
                    div(class="lbl2", "Organ"),
                    tags$select(class="styled", id="organ_sel",
                                onchange="Shiny.setInputValue('organ_sel',this.value)",
                                tags$option(value="Liver",        "Liver"),
                                tags$option(value="Right Breast", "Right Breast"),
                                tags$option(value="Spleen",       "Spleen"),
                                tags$option(value="Heart",        "Heart"),
                                tags$option(value="Bursa",        "Bursa")
                    )
                ),
                div(class="ctrl-group",
                    div(class="lbl2", "Chart Type"),
                    div(class="radio-group",
                        tags$label(tags$input(type="radio", name="org_type", value="bar", checked=NA,
                                              onchange="Shiny.setInputValue('org_type','bar')"), "Bar Chart"),
                        tags$label(tags$input(type="radio", name="org_type", value="box",
                                              onchange="Shiny.setInputValue('org_type','box')"), "Box Plot")
                    )
                )
            ),
            div(class="card-title", "Selected Organ by ARI Group"),
            div(class="card-sub",   "% of body weight"),
            plotlyOutput("organ_main", height=340)
        ),
        div(class="card",
            div(class="card-title", "All Organs"),
            div(class="card-sub",   "Data expressed as a percentage of final BW"),
            plotlyOutput("organ_all", height=290)
        )
    )
    
    output$stat_n <- renderText({
        r_bw_long() %>% distinct(ID) %>% nrow()
    })
    output$stat_bw <- renderText({
        v <- bw %>% filter(ARI %in% sel_ari()) %>% pull(d42) %>% mean(na.rm=TRUE)
        sprintf("%.3f", v)
    })
    output$stat_fcr <- renderText({
        sprintf("%.2f", mean(r_fcr_long()$FCR, na.rm=TRUE))
    })
    
    # wrap ggplot -> plotly with consistent styling
    as_plotly <- function(p, show_legend = TRUE) {
        leg <- if (show_legend) {
            list(orientation="h", x=0, y=-0.2, bgcolor="rgba(0,0,0,0)")
        } else {
            list(visible = FALSE)
        }
        ggplotly(p, tooltip=c("x","y","colour","fill")) %>%
            layout(
                paper_bgcolor = "#FFFFFF",
                plot_bgcolor  = "#FFFFFF",
                font   = list(family="sans-serif", color="#444"),
                legend = leg
            ) %>%
            config(displayModeBar=FALSE)
    }
    
    output$ov_bw <- renderPlotly({
        df <- r_bw_long() %>%
            group_by(ARI, Day) %>%
            summarise(M=mean(BW,na.rm=TRUE), S=sd(BW,na.rm=TRUE), .groups="drop")
        p <- ggplot(df, aes(Day, M, colour=ARI, fill=ARI)) +
            geom_ribbon(aes(ymin=M-S, ymax=M+S), alpha=.10, colour=NA) +
            geom_line(linewidth=1.05) +
            geom_point(size=2.6) +
            scale_colour_manual(values=ari_cols, name="ARI") +
            scale_fill_manual(values=ari_cols, name="ARI") +
            scale_x_continuous(breaks=c(0,7,14,21,28,35,42)) +
            labs(x="Day", y="Body Weight (kg)") +
            theme_clean()
        as_plotly(p, show_legend=FALSE)
    })
    
    output$ov_gain <- renderPlotly({
        df <- r_gain_long() %>%
            group_by(ARI, Interval) %>%
            summarise(M=mean(Gain,na.rm=TRUE), .groups="drop")
        p <- ggplot(df, aes(Interval, M, colour=ARI, group=ARI)) +
            geom_line(linewidth=1) +
            geom_point(size=2.4) +
            scale_colour_manual(values=ari_cols, name="ARI") +
            labs(x="Interval (days)", y="Mean Gain (kg)") +
            theme_clean()
        as_plotly(p, show_legend=FALSE)
    })
    
    output$ov_fcr <- renderPlotly({
        df <- r_fcr_long() %>%
            group_by(ARI, Interval) %>%
            summarise(M=mean(FCR,na.rm=TRUE), .groups="drop")
        p <- ggplot(df, aes(Interval, M, colour=ARI, group=ARI)) +
            geom_line(linewidth=1) +
            geom_point(size=2.4) +
            scale_colour_manual(values=ari_cols, name="ARI") +
            labs(x="Interval (days)", y="Mean FCR") +
            theme_clean()
        as_plotly(p, show_legend=FALSE)
    })
    
    output$bw_main <- renderPlotly({
        df <- r_bw_long() %>%
            group_by(ARI, Day) %>%
            summarise(M=mean(BW,na.rm=TRUE), S=sd(BW,na.rm=TRUE), .groups="drop")
        bw_type <- if (is.null(input$bw_type)) "line" else input$bw_type
        if (bw_type == "line") {
            p <- ggplot(df, aes(Day, M, colour=ARI, fill=ARI)) +
                geom_ribbon(aes(ymin=M-S, ymax=M+S), alpha=.10, colour=NA) +
                geom_line(linewidth=1.05) +
                geom_point(size=2.6) +
                scale_colour_manual(values=ari_cols, name="ARI") +
                scale_fill_manual(values=ari_cols, name="ARI") +
                scale_x_continuous(breaks=c(0,7,14,21,28,35,42)) +
                labs(x="Day", y="Body Weight (kg)") +
                theme_clean()
        } else {
            p <- ggplot(df, aes(factor(Day), M, fill=ARI)) +
                geom_col(position=position_dodge(0.72), width=0.68, alpha=.92) +
                geom_errorbar(aes(ymin=M-S, ymax=M+S),
                              position=position_dodge(0.72), width=0.22, colour="#888", linewidth=0.4) +
                scale_fill_manual(values=ari_cols, name="ARI") +
                labs(x="Day", y="Mean Body Weight (kg)") +
                theme_clean()
        }
        as_plotly(p)
    })
    
    output$gain_main <- renderPlotly({
        df <- r_gain_long() %>%
            group_by(ARI, Interval) %>%
            summarise(M=mean(Gain,na.rm=TRUE), S=sd(Gain,na.rm=TRUE), .groups="drop")
        gain_type <- if (is.null(input$gain_type)) "line" else input$gain_type
        if (gain_type == "line") {
            p <- ggplot(df, aes(Interval, M, colour=ARI, group=ARI)) +
                geom_errorbar(aes(ymin=M-S, ymax=M+S), width=0.12, linewidth=0.4,
                              position=position_dodge(0.08), colour="#AAAAAA") +
                geom_line(linewidth=1.05, position=position_dodge(0.08)) +
                geom_point(size=2.6, position=position_dodge(0.08)) +
                scale_colour_manual(values=ari_cols, name="ARI") +
                labs(x="Interval (days)", y="Weight Gain (kg)") +
                theme_clean()
        } else {
            p <- ggplot(df, aes(Interval, M, fill=ARI)) +
                geom_col(position=position_dodge(0.72), width=0.68, alpha=.92) +
                geom_errorbar(aes(ymin=M-S, ymax=M+S),
                              position=position_dodge(0.72), width=0.22, colour="#888", linewidth=0.4) +
                scale_fill_manual(values=ari_cols, name="ARI") +
                labs(x="Interval (days)", y="Mean Weight Gain (kg)") +
                theme_clean()
        }
        as_plotly(p)
    })
    
    output$fcr_main <- renderPlotly({
        df <- r_fcr_long() %>%
            group_by(ARI, Interval) %>%
            summarise(M=mean(FCR,na.rm=TRUE), S=sd(FCR,na.rm=TRUE), .groups="drop")
        fcr_type <- if (is.null(input$fcr_type)) "line" else input$fcr_type
        if (fcr_type == "line") {
            p <- ggplot(df, aes(Interval, M, colour=ARI, group=ARI)) +
                geom_errorbar(aes(ymin=M-S, ymax=M+S), width=0.12, linewidth=0.4,
                              position=position_dodge(0.08), colour="#AAAAAA") +
                geom_line(linewidth=1.05, position=position_dodge(0.08)) +
                geom_point(size=2.6, position=position_dodge(0.08)) +
                scale_colour_manual(values=ari_cols, name="ARI") +
                labs(x="Interval (days)", y="FCR") +
                theme_clean()
        } else {
            p <- ggplot(df, aes(Interval, M, fill=ARI)) +
                geom_col(position=position_dodge(0.72), width=0.68, alpha=.92) +
                geom_errorbar(aes(ymin=M-S, ymax=M+S),
                              position=position_dodge(0.72), width=0.22, colour="#888", linewidth=0.4) +
                scale_fill_manual(values=ari_cols, name="ARI") +
                labs(x="Interval (days)", y="Mean FCR") +
                theme_clean()
        }
        as_plotly(p)
    })
    
    output$organ_main <- renderPlotly({
        sel      <- if (is.null(input$organ_sel)) "Liver" else input$organ_sel
        org_type <- if (is.null(input$org_type))  "bar"   else input$org_type
        df <- r_org_long() %>% filter(Organ == sel)
        if (org_type == "bar") {
            sm <- df %>%
                group_by(ARI) %>%
                summarise(M=mean(Pct,na.rm=TRUE), S=sd(Pct,na.rm=TRUE), .groups="drop")
            p <- ggplot(sm, aes(ARI, M, fill=ARI)) +
                geom_col(width=0.5, alpha=.92) +
                geom_errorbar(aes(ymin=M-S, ymax=M+S), width=0.14, colour="#888", linewidth=0.45) +
                scale_fill_manual(values=ari_cols) +
                labs(x=NULL, y=paste0(sel," (% BW)")) +
                theme_clean() +
                theme(legend.position="none")
        } else {
            p <- ggplot(df, aes(ARI, Pct, fill=ARI)) +
                geom_boxplot(alpha=.85, outlier.size=1.4, outlier.colour="#AAAAAA", linewidth=0.45) +
                scale_fill_manual(values=ari_cols) +
                labs(x=NULL, y=paste0(sel," (% BW)")) +
                theme_clean() +
                theme(legend.position="none")
        }
        as_plotly(p, show_legend=FALSE)
    })
    
    output$organ_all <- renderPlotly({
        df <- r_org_long() %>%
            group_by(ARI, Organ) %>%
            summarise(M=mean(Pct,na.rm=TRUE), .groups="drop")
        p <- ggplot(df, aes(Organ, M, fill=ARI)) +
            geom_col(position=position_dodge(0.72), width=0.68, alpha=.92) +
            scale_fill_manual(values=ari_cols, name="ARI") +
            labs(x=NULL, y="Mean % BW") +
            theme_clean()
        as_plotly(p)
    })
}

shinyApp(ui, server)