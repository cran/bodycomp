#'
#' @title Percent Body Fat Estimates Using Slaughter et al. (1988).
#'
#'
#' @name Slaughter.2sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 2 sites skinfold measurements using the equations of Slaughter et al. (1988). Population Specifications: Both sexes. Boys (aged 8 - 18) and Girls (aged 8 - 18)
#'
#'
#' @return Returns a data frame with the values of the \% body fat from a subject assessed by skinfold measurements
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param CA a value or a vector representing Calf Skinfold measurements. Direction of Fold: Vertical (medial aspect). Anatomical Reference: Maximal calf circumference. Measurement: Fold is lifted at level of maximal calf circumference on medial aspect of calf with knee and hip flexed to 90°.
#'
#'
#' @details Sex: Use 0 for Male and 1 for Female.
#'
#'
#' @references Slaughter, M. H. et al. Skinfold equations for estimation of body fatness in children and youth. Human Biology, n. 60, p. 709-23, 1988.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML, et al.  Skinfold thicknesses and measurements technique.  In:  Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80.
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Eston, R. (Ed.). (2008). Kinanthropometry and Exercise Physiology Laboratory Manual: Tests, Procedures and Data: Volume One: Anthropometry (3rd ed.). Routledge. https://doi.org/10.4324/9780203868744
#'
#' Lohman TG. Advances in Body Composition Assessment: Current Issues in Exercise Science, Monograph Number 3. Champaign, IL: Human Kinetics, 1992.
#'
#'
#' @examples
#'
#' #Predicting % body fat
#'
#' df <- Slaughter.2sites(Sex = 1, TR = 17.1, CA = 24.3)
#'
#'
#' @export

Slaughter.2sites <- function(Sex, TR, CA) {
  PBF <- Sex_options_Slaughter(Sex, TR, CA)
  df <- output.equation(Output.format = 1, PBF, BD = 0)
  return(df)
}
