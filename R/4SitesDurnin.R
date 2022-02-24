
#' @title Percent Body Fat Estimates Using Durnin and Womersley (1974).
#'
#'
#' @name Durnin.4sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 4 sites skinfold measurements using the equations of Durnin and Womersley (1974). Population Specifications: Adults (both sexes) non athletes from UK; Male (aged 17 - 72) and Female (aged 16 - 68).
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements.
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param BI a value a vector representing Biceps Skinfold. Direction of Fold measurements: Vertical (midline). Anatomical Reference: Biceps brachii. Measurement: Fold is lifted over belly of the biceps brachii at the level marked for the triceps and on line with anterior border of the acromial process and the antecubital fossa. Caliper is applied 1 cm below fingers.
#'
#' @param SB a value or a vector representing Subscapular  Skinfold measurements. Direction of Fold: Diagonal. Anatomical Reference: Inferior angle of scapula. Measurement: Fold is along natural cleavage line of skin just inferior to inferior angle of scapula, with caliper applied 1 cm below fingers.
#'
#' @param SI a value a vector representing Suprailiac Skinfold measurements. Direction of Fold: Oblique. Anatomical Reference: Iliac crest. Measurement: Fold is grasped posteriorly to midaxillary line and superiorly to iliac crest along natural cleavage of skin with caliper applied 1 cm below fingers.
#'
#' @param Age a value or a vector representing the age (in years) from subject assessed.
#'
#' @param Equation desired estimation equation. Two possible choices: 1 or 2.
#'
#' @param Output.format desired output information. Tree possible choices: 0, 1 or 2.
#'
#'
#' @details Sex:Use 0 for Male and 1 for Female. Equation: Use 1 for Siri (1961) equation or 2 for Brozek et al. (1963) equation. Output.format: Use 0 to display Percent Body Fat (\%BF) and Body Density (BD) together; use 1 to display \%BF only; use 2 to display BD only.
#'
#'
#' @references Durnin, J.V.G.A., and J. Womersley. 1974. Body fat assessed from total body density and its estimation from skinfold thickness: Measurements on 481 men and women aged from 16 to 72 years. British Journal of Nutrition 32: 77-97.
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Eston, R. (Ed.). (2008). Kinanthropometry and Exercise Physiology Laboratory Manual: Tests, Procedures and Data: Volume One: Anthropometry (3rd ed.). Routledge. https://doi.org/10.4324/9780203868744
#'
#' Siri, W. E. Body composition from fluid space and density. In: BROZEK, J.; HANSCHEL, A. (Eds.). Techniques for measuring body composition. Washing, D.C.:National Academy of Science, 1961. p. 223-224.
#'
#' Brozek J., F. Grande, J. Anderson, et al. 1963. Densitometric analysis of body composition: Revision of some quantitative assumptions. Annals of the New York Academy of Sciences 110: 113-140.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML,  et al.  Skinfold thicknesses and measurements technique.  In: Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80.
#'
#'
#' @examples
#'
#' #Predicting % body fat
#'
#' df <- Durnin.4sites(Sex = 1, TR = 14, BI = 12, SB = 18, SI = 18,
#'                     Age = 28, Equation = 2, Output.format = 2)
#'
#'
#' @export

Durnin.4sites <- function(Sex, TR, BI, SB, SI, Age, Equation, Output.format) {
  BD <- Sex_options_Durnin4(Sex, TR, BI, SB, SI, Age)
  PBF <- PBF.Equation(Equation, BD)
  df <- output.equation(Output.format, PBF, BD)
  return(df)
}



