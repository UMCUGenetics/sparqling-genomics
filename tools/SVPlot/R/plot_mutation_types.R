#' Plot the number of mutations per type in the database.
#'
#' @param endpoint     The endpoint address to get data from.
#'
#' @import SPARQL
#' @import ggplot2
#'
#' @export

plot_mutations_per_type <- function (endpoint)
{
    # The following query retrieves a table containing  the following columns:
    # variant, position1, chromosome1, position2, chromosome2, svtype
    query <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX : <http://localhost:5000/cth/>

SELECT ?type COUNT(?type) as ?count {
  ?variant  rdf:type          :StructuralVariant .
  ?variant  :type             ?type .
}
GROUP BY ?type"

    # Perform the query.
    query_data <- SPARQL (endpoint, query)

    # Create the plot.
    qty_plot <- ggplot (data=query_data$results, aes(x=type, y=count)) +
        geom_bar (stat="identity") +
        xlab ("SV type") +
        ylab ("Number of SVs in the database") +
        ggtitle("Number of mutations per type") + 
        theme (plot.title = element_text(lineheight=1.0, face="bold"))

    return (qty_plot)
}
