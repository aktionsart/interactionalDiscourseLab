# Interactional Discourse Lab

The interactional discourse lab (IDLab) is a free tool available to researchers analysing spoken interaction. This web tool
was initially developed as a novel method for the quantitative analysis of the discourse of interdisciplinary research engagement,
but it can be used in any interactional context (e.g. classrooms, meetings, conversations).
Uniquely, it focuses on the interaction itself, rather than on merely spoken content. The tool is still in development and currently
provides fairly basic graphic and network representations of interactional relationships.

See the [official website](http://interactionaldiscourselab.net/) for more information and some [instructions](http://interactionaldiscourselab.net/tutorial.html).

You can use the tool here: <https://interactionaldiscourselab.shinyapps.io/IDLab/> but this is the most up-to-date version and it runs locally on your machine.

This is an `R` package that uses `shiny` to render its user interface. To install `R`, go to <http://www.r-project.org/>.

# Install
From `R`, install `devtools` if you don't have it:

    install.packages('devtools')

Then install IDLab with:

    devtools::install_github("/interactionalDiscourseLab")

You only to do this once. 

# Running the lab
From `R`, use:

    library("interactionalDiscourseLab")

Then

    launchIDLab()
