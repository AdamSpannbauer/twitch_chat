shinyServer(function(input, output, session){
  prog_react <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = "Gathering Results", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    inc_progess <- function(amount=.1, message=NULL, detail=NULL) {
      if(is.null(message) | is.null(detail)) {
        if(is.null(message) & is.null(detail)) {
          progress$inc(amount=amount)
        } else if (is.null(message)) {
          progress$inc(amount=amount, detail=detail)
        } else if (is.null(detail)) {
          progress$inc(amount=amount, message=message)
        }
      } else {
        progress$inc(amount=amount, message=message, detail=detail)
      }
    }
    
    test_prog <- function(inc_progess) {
      Sys.sleep(5)
      inc_progess(message="msg update", detail="detail up", .3)
      Sys.sleep(5)
      inc_progess(detail="detail up2", amount=.1)
      Sys.sleep(5)
      inc_progess(message="msg up2", amount=.1)
      Sys.sleep(5)
      inc_progess(amount=.3)
      Sys.sleep(5)
      inc_progess(amount=.15)
      return("Done")
    }
    
    test_prog(inc_progess)
  })
  
  output$progress_reactive <- renderUI({
    req(prog_react())
    HTML(prog_react())
  })
})