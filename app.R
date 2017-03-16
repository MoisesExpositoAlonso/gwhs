library(shiny)
library(ggplot2)

ui <-
# Define UI for miles per gallon application
fluidPage(
titlePanel("Observed fitness in two environments and two loci depending on selection and LD"),
  # Application title
  # headerPanel("One locus evolution of diploid populations: mutation, migration, selection, and drift"),

sidebarPanel(
    "Selection in environment 1",
    sliderInput("sa1", "Selection in SNP A, sa1:", min=-1, max=1, value=0.5, step=0.01),
    sliderInput("sb1", "Selection in SNP B, sb1:", min=-1, max=1, value=0, step=0.01)#,

),sidebarPanel(
    "Selection in environment 2",
    sliderInput("sa2", "Selection in SNP A, sa2:", min=-1, max=1, value=-0.5, step=0.01),
    sliderInput("sb2", "Selection in SNP B, sb2:", min=-1, max=1, value=0, step=0.01)#,

),sidebarPanel(
   "LD between the SNPs",
    sliderInput("ld", "LD as correlation of allele frequencies between the two biallelic SNPs", min=-0.99, max=0.99, value=0, step=0.01)

),

  mainPanel(
    plotOutput("selplot")
  )
)

# Define server
server <-function(input, output) {


sim_heterogeneous_sel <-function(sa1=0.1,sa2=-0.1,sb1=0,sb2=0,ld=0.5){

  z<- matrix(c(1,ld,1,ld), ncol=2)

  s1 <- (c(sa1, sb1))
  s2 <- (c(sa2, sb2))

  ef1<-1+ z %*% s1
  ef2<-1+ z %*% s2

  df<-data.frame(fitness=c(ef1,ef2), env=c("e1","e1","e2","e2") ,loci=c("a","b","a","b"))

  return(df)
}

sim <- reactive({ sim_heterogeneous_sel(sa1=input$sa1,
                                        sb1=input$sb1,
                                        sa2=input$sa2,
                                        sb2=input$sb2,
                                        ld=input$ld)  } )

output$selplot <- renderPlot({
p<-ggplot(sim()) + geom_hline(yintercept = 1)+ geom_line(aes(y=fitness,x=env,group=loci,color=loci))  + ylim(c(0,+2))
print(p)
})

}


shinyApp(ui = ui, server = server)
