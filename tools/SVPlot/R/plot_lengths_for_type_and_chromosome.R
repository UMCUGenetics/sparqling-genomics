#' Plot the distribution of mutation types in the database
#'
#' @param endpoint        The endpoint address to get data from.
#' @param type            The type to plot for.
#' @param chromosome      The chromosome to plot for.
#' @param maximum_length  (optional)  The maximum SV length. [default=5000]
#'
#' @import SPARQL
#' @import ggplot2
#'
#' @export

plot_lengths_for_type_and_chromosome <- function (endpoint, type, chromosome,
                                                  maximum_length = 5000)
{
    endpoint <- "http://localhost:8890/sparql"
    type <- "DEL"
    chromosome <- "1"
    maximum_length <-  5000

    # The following query retrieves a table containing  the following columns:
    # variant, position1, chromosome1, position2, chromosome2, svtype
    query <- paste ("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX : <http://localhost:5000/cth/>

SELECT DISTINCT (?length) COUNT(?length) as ?count
{
  ?variant rdf:type          :StructuralVariant ;
           :genome_position  ?p1 ;
           :genome_position2 ?p2 ;
           :type             ?svtype ;
           :filter           ?filter .

  ?p1 :position ?position1 ;
      :chromosome ?chromosome .

  ?p2 :position ?position2 ;
      :chromosome ?chromosome2 .

  BIND ((?position2 - ?position1) AS ?length)

  FILTER (?length < ", maximum_length ,")
  FILTER (?svtype = \"", type, "\")
  FILTER (?chromosome = \"", chromosome, "\")
  FILTER (?chromosome = ?chromosome2)
  FILTER (?filter != \"LowQual\")
}
GROUP BY ?length", sep="")

    # Perform the query.
    query_data <- SPARQL (endpoint, query)

    # Create the plot.
    qty_plot <- ggplot (data=query_data$results, aes(x=length,y=count)) +
        geom_line() + 
        geom_area(stat="identity", fill="#cc0000", alpha=0.75) +
        #geom_bar (stat="identity", color="red") +
        xlab ("Length of the SV") +
        ylab ("Number of SVs with length") +
        ggtitle("SV length distribution\n(not taking confidence intervals into account)") + 
        theme (plot.title = element_text(lineheight=1.0, face="bold"))

    return (qty_plot)
}
