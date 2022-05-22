dataStore <- R6::R6Class(
  # my own class
  "shinyStore",

  # private variables
  private = list(
      data = NULL,
      reactiveDep = NULL
  ),
  public = list(
    # initialize (new)
    initialize = function(data) {
      # checks
      stopifnot(is.data.frame(data))
      # set data
      private$data <- data
      # initiate reactivity
      private$reactiveDep <- reactiveVal(0)
    },
    reactive = function() {
      reactive({
        private$reactiveDep()
        self
      })
    },
    set = function(data) {
      private$reactiveDep(isolate(private$reactiveDep()) + 1)
      private$data <- data
    },
    get = function(data) {
      private$data
    }

  )
)

