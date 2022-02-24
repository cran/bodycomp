#'
#' @title Percent Body Fat Estimates Using Petroski (1995).
#'
#'
#' @name Petroski.2sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 2 sites skinfold measurements using the equations of Petroski (1995). Population Specifications: Brazilian Males aged 18-66.
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements
#'
#'
#' @param TR a value or a vector representing Triceps Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param MA Site:  a value or a vector representing Midaxillary  Skinfold measurements.  Direction of Fold:  Oblique (Adapted from Author). Anatomical Reference:  Xiphisternal junction (point where costal cartilage of ribs 5-6 articulates with sternum, slightly above inferior tip of xiphoid process). Measurement:  Fold is taken on midaxillary line at level of xiphisternal junction. Petroski choose modifications in the verification of the AM fold for obliques because it's most used procedures in Brazil.
#'
#' @param Age a value or a vector representing the age (in years) from subject assessed.
#'
#' @param Equation desired estimation equation. Two possible choices: 1 or 2.
#'
#' @param Output.format desired output information. Tree possible choices: 0, 1 or 2.
#'
#'
#' @details  Equation: Use 1 for Siri (1961) equation or 2 for Brozek et al. (1963) equation. Output.format: Use 0 to display Percent Body Fat (\%BF) and Body Density (BD) together; use 1 to display \%BF only; use 2 to display BD only.
#'
#'
#' @references Petroski, E. L. Desenvolvimento e validação de equações generalizadas para a estimativa da densidade corporal em adultos. 1995. Tese (Doutorado) – UFSM, Santa Maria.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML,  et al.  Skinfold thicknesses and measurements technique.  In:  Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Eston, R. (Ed.). (2008). Kinanthropometry and Exercise Physiology Laboratory Manual: Tests, Procedures and Data: Volume One: Anthropometry (3rd ed.). Routledge. https://doi.org/10.4324/9780203868744
#'
#' Siri, W. E. Body composition from fluid space and density. In: BROZEK, J.; HANSCHEL, A. (Eds.). Techniques for measuring body composition. Washing, D.C.:National Academy of Science, 1961. p. 223-224.
#'
#' Brozek J., F. Grande, J. Anderson, et al. 1963. Densitometric analysis of body composition: Revision of some quantitative assumptions. Annals of the New York Academy of Sciences 110: 113-140.
#'
#'
#' @examples
#'
#' # Predicting % body fat
#'
#' df<- Petroski.2sites(TR = c(23,24), MA = c(34,33), Age = c(24,27), Equation = 1, Output.format = 0)
#'
#'
#' @export


Petroski.2sites <- function(TR, MA, Age, Equation, Output.format) {
  BD <- 1.10098229 - (0.00145899 * (TR + MA)) + (0.00000701 * ((TR + MA)^2)) - (0.00032770 * (Age))
  PBF <- PBF.Equation(Equation, BD)
  df <- output.equation(Output.format, PBF, BD)
  return(df)
}
