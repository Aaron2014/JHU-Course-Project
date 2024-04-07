library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
  output$plot1 <- renderPlot({
    
    set.seed(2024-04-04)
    x_var <- input$x_var
    x_var1 <- switch(x_var,
                    'Number of Cylinders' = "cyl",
                    'Displacement (cu.in.)' = "disp", 
                    'Gross horsepower' = "hp", 
                    'Rear axle ratio'= "drat",
                    'Weight (1000 lbs)'= "wt", 
                    '1/4 mile time'="qsec", 
                    'Engine' = "vs",
                    'Transmission' = "am",
                    'Number of forward gears' = "gear",
                    'Number of carburetors' = "carb")
    plot_type <- input$plt_type
    clr_type <- input$color_type
    clr_type <- switch(clr_type,
                       'Number of Cylinders' = "cyl",
                       'Engine' = "vs",
                       'Transmission' = "am",
                       'Number of forward gears' = "gear",
                       'Number of carburetors' = "carb",
                       'None' = "None")
    plt_title <- input$plt_title
    xlab <- ifelse(input$show_xlab, x_var, "")
    ylab <- ifelse(input$show_ylab, "MPG", "")
    main <- ifelse(input$show_title, plt_title, "")
    if (plot_type == 'Box Plot'){
      if (x_var1 == "am"){
        ggplot(data = mtcars, aes(x=as.character(am), y=mpg)) +
          geom_boxplot(fill="slateblue", alpha=0.2) +
          stat_boxplot(geom = 'errorbar')  + 
          xlab(xlab) +
          ylab(ylab) +
          ggtitle(main) +
          scale_x_discrete(labels=c("Automatic", "Manual"))
      } else if (x_var1 =="cyl"){
        ggplot(data = mtcars, aes(x=as.character(cyl), y=mpg)) +
          geom_boxplot(fill="slateblue", alpha=0.2) +
          stat_boxplot(geom = 'errorbar')  + 
          xlab(xlab) +
          ylab(ylab) +
          ggtitle(main) 
      } else if (x_var1 =="vs"){
        ggplot(data = mtcars, aes(x=as.character(vs), y=mpg)) +
          geom_boxplot(fill="slateblue", alpha=0.2) +
          stat_boxplot(geom = 'errorbar')  + 
          xlab(xlab) +
          ylab(ylab) +
          scale_x_discrete(labels=c("V-shaped", "Straight")) +
          ggtitle(main) 
      } else if (x_var1 =="gear"){
        ggplot(data = mtcars, aes(x=as.character(gear), y=mpg)) +
          geom_boxplot(fill="slateblue", alpha=0.2) +
          stat_boxplot(geom = 'errorbar')  + 
          xlab(xlab) +
          ylab(ylab) +
          ggtitle(main) 
      } else if (x_var1 =="carb"){
        ggplot(data = mtcars, aes(x=as.character(carb), y=mpg)) +
          geom_boxplot(fill="slateblue", alpha=0.2) +
          stat_boxplot(geom = 'errorbar')  + 
          xlab(xlab) +
          ylab(ylab) +
          ggtitle(main) 
      }

    } else if (plot_type == 'Scatter Plot'){
      if (clr_type == "am"){
        if(x_var1 == "disp"){
          ggplot(data = mtcars, aes(x=disp, y=mpg, color=as.character(am))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Transmission Type',labels=c("Automatic", "Manual"))
        } else if (x_var1 == "hp"){
          ggplot(data = mtcars, aes(x=hp, y=mpg, color=as.character(am))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Transmission Type',labels=c("Automatic", "Manual"))
        } else if (x_var1 == "drat"){
          ggplot(data = mtcars, aes(x=drat, y=mpg, color=as.character(am))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Transmission Type',labels=c("Automatic", "Manual"))
        } else if (x_var1 == "wt"){
          ggplot(data = mtcars, aes(x=wt, y=mpg, color=as.character(am))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Transmission Type',labels=c("Automatic", "Manual"))
        } else if (x_var1 == "qsec"){
          ggplot(data = mtcars, aes(x=qsec, y=mpg, color=as.character(am))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Transmission Type',labels=c("Automatic", "Manual"))
        } 
      } else if (clr_type == "cyl"){
        if(x_var1 == "disp"){
          ggplot(data = mtcars, aes(x=disp, y=mpg, color=as.character(cyl))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Cylinders')
        } else if (x_var1 == "hp"){
          ggplot(data = mtcars, aes(x=hp, y=mpg, color=as.character(cyl))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Cylinders')
        } else if (x_var1 == "drat"){
          ggplot(data = mtcars, aes(x=drat, y=mpg, color=as.character(cyl))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Cylinders')
        } else if (x_var1 == "wt"){
          ggplot(data = mtcars, aes(x=wt, y=mpg, color=as.character(cyl))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Cylinders')
        } else if (x_var1 == "qsec"){
          ggplot(data = mtcars, aes(x=qsec, y=mpg, color=as.character(cyl))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Cylinders')
        }
      } else if (clr_type == "vs"){
        if(x_var1 == "disp"){
          ggplot(data = mtcars, aes(x=disp, y=mpg, color=as.character(vs))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Engine Type',labels=c("V-shaped", "Straight"))
        } else if (x_var1 == "hp"){
          ggplot(data = mtcars, aes(x=hp, y=mpg, color=as.character(vs))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Engine Type',labels=c("V-shaped", "Straight"))
        } else if (x_var1 == "drat"){
          ggplot(data = mtcars, aes(x=drat, y=mpg, color=as.character(vs))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Engine Type',labels=c("V-shaped", "Straight"))
        } else if (x_var1 == "wt"){
          ggplot(data = mtcars, aes(x=wt, y=mpg, color=as.character(vs))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Engine Type',labels=c("V-shaped", "Straight"))
        } else if (x_var1 == "qsec"){
          ggplot(data = mtcars, aes(x=qsec, y=mpg, color=as.character(vs))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Engine Type',labels=c("V-shaped", "Straight"))
        }
      } else if (clr_type == "gear"){
        if(x_var1 == "disp"){
          ggplot(data = mtcars, aes(x=disp, y=mpg, color=as.character(gear))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Forward Gears')
        } else if (x_var1 == "hp"){
          ggplot(data = mtcars, aes(x=hp, y=mpg, color=as.character(gear))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Forward Gears')
        } else if (x_var1 == "drat"){
          ggplot(data = mtcars, aes(x=drat, y=mpg, color=as.character(gear))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Forward Gears')
        } else if (x_var1 == "wt"){
          ggplot(data = mtcars, aes(x=wt, y=mpg, color=as.character(gear))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Forward Gears')
        } else if (x_var1 == "qsec"){
          ggplot(data = mtcars, aes(x=qsec, y=mpg, color=as.character(gear))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of Forward Gears')
        }
      } else if (clr_type == "carb"){
        if(x_var1 == "disp"){
          ggplot(data = mtcars, aes(x=disp, y=mpg, color=as.character(carb))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of carburetors')
        } else if (x_var1 == "hp"){
          ggplot(data = mtcars, aes(x=hp, y=mpg, color=as.character(carb))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of carburetors')
        } else if (x_var1 == "drat"){
          ggplot(data = mtcars, aes(x=drat, y=mpg, color=as.character(carb))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of carburetors')
        } else if (x_var1 == "wt"){
          ggplot(data = mtcars, aes(x=wt, y=mpg, color=as.character(carb))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of carburetors')
        } else if (x_var1 == "qsec"){
          ggplot(data = mtcars, aes(x=qsec, y=mpg, color=as.character(carb))) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) +
            scale_color_discrete(name='Number of carburetors')
        }
      } else if (clr_type == "None"){
        if(x_var1 == "disp"){
          ggplot(data = mtcars, aes(x=disp, y=mpg)) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main)
        } else if (x_var1 == "hp"){
          ggplot(data = mtcars, aes(x=hp, y=mpg)) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main)
        } else if (x_var1 == "drat"){
          ggplot(data = mtcars, aes(x=drat, y=mpg)) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) 
        } else if (x_var1 == "wt"){
          ggplot(data = mtcars, aes(x=wt, y=mpg)) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) 
        } else if (x_var1 == "qsec"){
          ggplot(data = mtcars, aes(x=qsec, y=mpg)) +
            geom_point() +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(main) 
        }
      }
      
    }
    
  })
})