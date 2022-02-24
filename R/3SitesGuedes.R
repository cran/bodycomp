#'
#' @title Percent Body Fat Estimates Using Guedes (1985).
#'
#'
#' @name Guedes.3sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 3 sites skinfold measurements using the equations of Guedes (1965). Population Specifications: Both sexes; Brazilian university students; Male and Female (aged 18-30).
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurement. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param AB a value or a vector representing Abdominal Skinfold. Direction of Fold measurement: Horizontal. Anatomical Reference: Umbilicus. Measurement: Fold is taken 3 cm lateral and 1 cm inferior to center of the umbilicus.
#'
#' @param SI a value or a vector representing Suprailiac Skinfold measurement. Direction of Fold: Oblique. Anatomical Reference: Iliac crest. Measurement: Fold is grasped posteriorly to midaxillary line and superiorly to iliac crest along natural cleavage of skin with caliper applied 1 cm below fingers.
#'
#' @param TH a value or a vector representing Thigh Skinfold measurement. Direction of Fold: Vertical (midlin). Anatomical Reference: Inguinal crease and patella. Measurement: Fold is lifted on anterior aspect of thigh midway between inguinal crease and proximal border of patella. Body weight is shifted to left foot and caliper is applied 1 cm below fingers.
#'
#' @param SB a value or a vector representing Subscapular Skinfold measurement. Direction of Fold: Diagonal. Anatomical Reference: Inferior angle of scapula. Measurement: Fold is along natural cleavage line of skin just inferior to inferior angle of scapula, with caliper applied 1 cm below fingers.
#'
#' @param Equation desired estimation equation. Two possible choices: 1 or 2.
#'
#' @param Output.format desired output information. Tree possible choices: 0, 1 or 2.
#'
#'
#' @details Sex:Use 0 for Male and 1 for Female. Equation: Use 1 for Siri (1961) equation or 2 for Brozek et al. (1963) equation. Output.format: Use 0 to display Percent Body Fat (\%BF) and Body Density (BD) together; use 1 to display \%BF only; use 2 to display BD only.
#'
#'#'
#' @references Guedes, D.P. Estudo da gordura corporal através da mensuração dos valores de densidade corporal e espessura de dobras cutâneas em universitários.(Dissertação de Mestrado). Santa Maria (RS), Universidade Federal de Santa Maria, 1985.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML, et al.  Skinfold thicknesses and measurements technique.  In:  Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80.
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Jackson, A.S., and M.L. Pollock. 1978. Generalized equations for predicting body density of men. British Journal of Nutrition 40: 497-504.
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
#' df <- Guedes.3sites(Sex = 1, TR = 34, AB = 24, SI = 33, TH = 24, SB = 19,
#'                     Equation = 1, Output.format = 0)
#'
#'
#'
#' @export

Guedes.3sites <- function(Sex, TR, AB, SI, TH, SB, Equation, Output.format) {
  BD <- Sex_options_Guedes(Sex, TR, AB, SI, TH, SB)
  PBF <- PBF.Equation(Equation, BD)
  df <- output.equation(Output.format, PBF, BD)
  return(df)
}

