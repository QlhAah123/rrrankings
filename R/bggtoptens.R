#' The top 10 games according to Board Game Geek's (BGG) "geek rating" from Jan 1 each year from 2017 to 2024.
#'
#' @docType data
#' @name bggtoptens
#' @usage data(bggtoptens)
#' @format  A data frame with 80 observations of 7 variables.
#' \describe{
#' \item{\code{Year}}{ Year observed}
#' \item{\code{Name}}{ Tile of game, as recorded in BGG's database}
#' \item{\code{Release Year}}{ Year the game was first released}
#' \item{\code{Rank}}{ Ranking based on Bayes average column}
#' \item{\code{Average}}{ Raw average rating out of 10 by BGG users}
#' \item{\code{Bayes average}}{ Also known as "geek rating," the average of user votes and some constant amount of dummy votes rating each game 5.5.}
#' \item{\code{Users rated}}{ Number of users who rated the game}
#' }
#' @source
#' Data scraped by Markus Shepherd of reccommend.games, collected from BoardGameGeek.
#' \url{https://framagit.org/r.g/bgg-ranking-historicals}
NA