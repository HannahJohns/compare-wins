library(tidyverse)
library(patchwork)





# Create dataframe for plot with overall line
test_data_overall_fn <- data.frame(level=1:5,
                                   level_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing", "Overall"),
                                win_inherit=c(0,15,25,33,0),
                                win=c(15,10,8,15,48),
                                tie=c(80,60,40,15,15),
                                loss=c(5,10,12,10,37),
                                loss_inherit=c(0,5,15,27,0),
                                win_ratio=c(3,1,0.67,1.5,1.30),
                                ci_lower=c(2.6,0.6,0.27,1.1,1.10),
                                ci_upper=c(3.4,1.4,1.07,1.9,1.50),
                                win_ratio_cum=c(3,1.67,1.22,1.30,1.30),
                                ci_lower_cum=c(2.6,1.32,0.92,1.10,1.10),
                                ci_upper_cum=c(3.4,2.02,1.52,1.50,1.50))

test_data_overall_fnV2 <- data.frame(level=1:6,
                                   level_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing", "Independence", "Overall"),
                                   win_inherit=c(0,15,25,33,48,0),
                                   win=c(15,10,8,15,8,56),
                                   tie=c(80,60,40,15,5,5),
                                   loss=c(5,10,12,10,2,39),
                                   loss_inherit=c(0,5,15,27,37,0),
                                   win_ratio=c(3,1,0.67,1.5,4.0,1.44),
                                   ci_lower=c(2.6,0.6,0.27,1.1,3.6,1.24),
                                   ci_upper=c(3.4,1.4,1.07,1.9,4.4,1.64),
                                   win_ratio_cum=c(3,1.67,1.22,1.30,1.44,1.44),
                                   ci_lower_cum=c(2.6,1.32,0.92,1.10,1.24,1.24),
                                   ci_upper_cum=c(3.4,2.02,1.52,1.50,1.64,1.64))



# Win ratio plot function AND COLOURED BACKGROUND reordered for red on left and green on right
wrplotfn_colourpanel_reorder <- function(df)
{
        # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables 
        # are included in the dataframe and stop and report an error message if they are missing or named differently
        if(class(df) != "data.frame"){
                stop("Incorrect argument class: dataframe argument must be a dataframe")
        }
        if(("level" %in% names(df)) != TRUE){
                stop("Missing or different variable name: check level variable is labelled 'level'")
        }
        if(("level_names" %in% names(df)) != TRUE){
                stop("Missing or different variable name: check level name variable is labelled 'level_names'")
        }
        if(("win" %in% names(df)) != TRUE){
                stop("Missing or different variable name: check win variable is labelled 'win'")
        }
        
        # Sort data in ascending level in case dataframe is out of level order
        df <- df[order(df$level),]
        
        # Create a list of level names to use later in plot (reverse order given the plot is reversed)
        level_label_names_list <- rev(as.list(df$level_names))
        
        df_long <- pivot_longer(df,
                                c(loss_inherit,loss,tie,win,win_inherit),
                                names_to = "pairType",values_to = "n")
        
        df_long$pairType <- factor(df_long$pairType,  
                                   levels = c("loss_inherit", "loss", "tie", "win", "win_inherit"), 
                                   labels = c("Inherited losses", "Losses", "Ties", "Wins", "Inherited wins"))
        
        df_long$p <- df_long$n
        
        for(i in unique(df_long$level)) df_long[df_long$level == i,"p"] <- df_long[df_long$level == i,"p"]/sum(df_long[df_long$level == i,"p"])
        
        df_long <- do.call("rbind",by(df_long,df_long$level,function(df_rb){
                df_rb$p_prev <- ((cumsum(df_rb$p)-df_rb$p))
                df_rb
        }))
        
        
        # Set box width
        df_long$width <- ifelse(df_long$level==max(df_long$level), 0.6,1)
        
        # Win ratio plot with no space between levels and overall line
        wr_box_overall2 <- ggplot(df_long) +
                geom_rect(aes(xmin=level-width/2,xmax=level+width/2,
                              ymin=p_prev,ymax=p_prev+p,fill=pairType), color="black") +
                scale_x_reverse(breaks=c(max(df_long$level):1),
                                labels=level_label_names_list, 
                                minor_break=NULL, limits=c(max(df_long$level)+0.3,0.5)) +
                scale_fill_manual(values = c("tan1", "red2", "khaki1", "forestgreen", "#97d42c")) +
                guides(fill = guide_legend(title = "Treatment comparison", title.theme = element_text(face = "bold"))) +
                coord_flip() +
                ylab(expression(bold("Proportion (intervention vs control)"))) +
                theme_bw() +
                theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
        
        background_colourpanel <- data.frame(favours_int=c(0,1),
                                             xmin=c((max(df$level)+0.3),(max(df$level)+0.3)),
                                             xmax=c(0.5,0.5),
                                             ymin=c(0,1),
                                             ymax=c(1,max(df$ci_upper)))
        
        background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,  
                                                     levels = c(0,1), 
                                                     labels = c("Favours control", "Favours intervention"))
        
        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio (annotated version has axis limit adjusted to make extra space)
        wr_forest_overall_diamond_cum2 <- ggplot(df) +
                geom_rect(data=background_colourpanel, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=favours_int), alpha=0.15) +
                scale_fill_manual(values=c("#D10707", "#45E928")) +
                geom_hline(yintercept=1,color="#1A2678") +
                geom_errorbar(data=df[df$level<max(df$level), ], 
                              aes(x=level-0.1, ymin=ci_lower, ymax=ci_upper, color="Level win ratio"), width=0) +
                geom_point(data=df[df$level<max(df$level), ], 
                           aes(x=level-0.1, y=win_ratio, color="Level win ratio"), size=2.5, shape=4) +
                geom_line(data=df[df$level<max(df$level), ], 
                          aes(x=level+0.1,y=win_ratio_cum), color="black", linetype="dashed") +
                geom_errorbar(data=df[df$level<max(df$level), ], 
                              aes(x=level+0.1, ymin=ci_lower_cum, ymax=ci_upper_cum, color="Cumulative win ratio"), width=0) +
                geom_point(data=df[df$level<max(df$level), ], 
                           aes(x=level+0.1, y=win_ratio_cum, color="Cumulative win ratio"), size=2.5) +
                geom_polygon(data = data.frame(x=c(max(df$level)-0.18, max(df$level), 
                                                   max(df$level)+0.18, max(df$level)), 
                                               y=c(df[df$level==max(df$level), "win_ratio"], 
                                                   df[df$level==max(df$level), "ci_upper"], 
                                                   df[df$level==max(df$level), "win_ratio"], 
                                                   df[df$level==max(df$level), "ci_lower"])), 
                             aes(x=x, y=y)) +
                scale_x_reverse(breaks=NULL, limits=c(max(df$level)+0.3,0.5)) +
                scale_y_continuous(limits=c(0,NA)) +
                xlab(NULL) +
                ylab(expression(bold("Win ratio"))) +
                guides(color = guide_legend(reverse = TRUE, title = NULL), fill = guide_legend(title = NULL)) +
                scale_color_manual(values=c("black", "grey40")) +
                coord_flip() +
                theme_bw()
        
        # Combine plots
        wr_box_overall2 + wr_forest_overall_diamond_cum2 + plot_layout(guides = 'collect')
}



# Test with data set
wrplotfn_colourpanel_reorder(test_data_overall_fn)

# Test with different data set with extra level
wrplotfn_colourpanel_reorder(test_data_overall_fnV2)


