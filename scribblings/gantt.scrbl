#lang scribble/manual
@require[@for-label[gantt
                    graph
                    racket/base]]

@title{gantt}
@author{thoughtstem}

@defmodule[gantt]

Basic workflow: 

@itemize{
  @item{Assemble your @racket[task]s using the @racket[task] struct}
  @item{Convert it to a task dependency graph with @racket[dep-graph]}
  @item{Automatically set start dates}
  @item{Render to charts (i.e. Gantt)}
}

@defproc[(dep-graph [task task?] ...) dep-graph?]{
  Constructs a graph out of the given tasks and their dependencies.

  Note that dep graphs are assumed to have a single start task and a single end task.  You don't have to pass in these meta tasks, though.  They will be in the graph returned.

}
