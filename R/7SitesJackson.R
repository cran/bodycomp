#'
#' @title Percent Body Fat Estimates Using Jackson, Pollock (1978) and Jackson et al. (1980).
#'
#'
#' @name Jackson.7sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 7 sites skinfold measurements using the equations of Jackson, Pollock (1978) and Jackson et al. (1980). Population Specifications: Both sexes, Non Athletes: Males (aged 18-61) and Females (aged 18 - 55).
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param TH a value or a vector representing Thigh Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Inguinal crease and patella. Measurement: Fold is lifted on anterior aspect of thigh midway between inguinal crease and proximal border of patella. Body weight is shifted to left foot and caliper is applied 1 cm below fingers.
#'
#' @param SB a value representing Subscapular Skinfold. Direction of Fold: Oblique. Anatomical Reference: Vertebral border and inferior angle of scapula. Measurement: Fold is taken on diagonal line coming from the vertebral border, 1-2 cm below the inferior angle.
#'
#' @param SI a value representing Suprailiac Skinfold. Direction of Fold: Diagonal. Anatomical Reference: Iliac crest. Measurement: Fold is taken diagonally above the iliac crest along the anterior axillary line.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param CH a value representing Chest Skinfold. Direction of Fold: Diagonal. Anatomical Reference: Axilla and nipple. Measurement: Fold is taken 1/2 the distance between the anterior axillary line and nipple for men and 1/3 of this distance for women.
#'
#' @param AB a value representing Abdominal Skinfold. Direction of Fold: Vertical. Anatomical Reference: Umbilicus. Measurement: Fold is taken vertically 2 cm lateral to the umbilicus.
#'
#' @param MA a value representing Midaxillary Skinfold. Direction of Fold: Vertical. Anatomical Reference: Xiphoid process of sternum. Measurement: Fold is taken at level of xiphoid process along the midaxillary line.
#'
#' @param Age a value or a vector representing the age (in years) from subject assessed.
#'
#' @param Equation desired estimation equation. Two possible choices: 1 or 2.
#'
#' @param Output.format desired output information. Tree possible choices: 0, 1 or 2.
#'
#'
#' @details Sex: Use 0 for Male and 1 for Female. Equation: Use 1 for Siri (1961) equation or 2 for Brozek et al. (1963) equation. Output.format: Use 0 to display Percent Body Fat (\%BF) and Body Density (BD) together; use 1 to display \%BF only; use 2 to display BD only.
#'
#'
#' @references Jackson, A.S., and M.L. Pollock. 1978. Generalized equations for predicting body density of men. British Journal of Nutrition 40: 497-504.
#'
#' Jackson, A.S., M.L. Pollock, and A. Ward. 1980. Generalized equations for predicting body density of women. Medicine & Science in Sports & Exercise 12: 175-18.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML, et al.  Skinfold thicknesses and measurements technique.  In:  Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80.
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Eston, R. (Ed.). (2008). Kinanthropometry and Exercise Physiology Laboratory Manual: Tests, Procedures and Data: Volume One: Anthropometry (3rd ed.). Routledge. https://doi.org/10.4324/9780203868744
#'
#' Siri, W. E. Body composition from fluid space and density. In: BROZEK, J.; HANSCHEL, A. (Eds.). Techniques for measuring body composition. Washing, D.C.: National Academy of Science, 1961. p. 223-224.
#'
#' Brozek J., F. Grande, J. Anderson, et al. 1963. Densitometric analysis of body composition: Revision of some quantitative assumptions. Annals of the New York Academy of Sciences 110: 113-140.
#'
#' @examples
#'
#' # Predicting % body fat
#'
#' df <- Jackson.7sites(Sex = 1, TH = 12, SB = 13, SI = 14, TR = 15, CH = 15, AB = 16,
#'                      MA = 14, Age = 32, Equation = 1, Output.format = 2)
#'
#'
#'
#' @export

Jackson.7sites <- function(Sex, TH, SB, SI, TR, CH, AB, MA, Age, Equation, Output.format) {
  BD <- Sex_options_Jackson7(Sex, TH, SB, SI, TR, CH, AB, MA, Age)
  PBF <- PBF.Equation(Equation, BD)
  df <- output.equation(Output.format, PBF, BD)
}

