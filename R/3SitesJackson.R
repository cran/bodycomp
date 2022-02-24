#'
#' @title Percent Body Fat Estimates Using Jackson and Pollock (1978) and Jackson et al. (1980).
#'
#'
#' @name Jackson.3sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 3 sites skinfold measurements using the equations of Pollock (1978) and Jackson et al. (1980). Population Specifications: Both sexes; Male (aged 18-61) and Female (aged 18-55).
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param CH a value or a vector representing Chest Skinfold measurement. Direction of Fold: Diagonal. Anatomical Reference: Axilla and nipple. Measurement: Fold is taken between axilla and nipple as high as possible on anterior axillary fold, with measurement taken 1 cm below fingers.
#'
#' @param AB a value or a vector representing Abdominal Skinfold measurement. Direction of Fold: Horizontal. Anatomical Reference: Umbilicus. Measurement: Fold is taken 3 cm lateral and 1 cm inferior to center of the umbilicus.
#'
#' @param TH a value or a vector representing Thigh Skinfold measurement. Direction of Fold: Vertical (midlin). Anatomical Reference: Inguinal crease and patella. Measurement: Fold is lifted on anterior aspect of thigh midway between inguinal crease and proximal border of patella. Body weight is shifted to left foot and caliper is applied 1 cm below fingers.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurement. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param SI a value or a vector representing Suprailiac Skinfold measurement. Direction of Fold: Oblique. Anatomical Reference: Iliac crest. Measurement: Fold is grasped posteriorly to midaxillary line and superiorly to iliac crest along natural cleavage of skin with caliper applied 1 cm below fingers.
#'
#' @param Age a value or a vector representing the age (in years) from subject assessed.
#'
#' @param Equation desired estimation equation. Two possible choices: 1 or 2.
#'
#' @param Output.format desired output information. Tree possible choices: 0, 1 or 2.
#'
#'
#' @details Sex: Use 0 for Male (Jackson and Pollock. 1978) or 1 for Female (Jackson et al. 1980). Use 1 for Siri (1961) equation or 2 for Brozek et al. (1963) equation. Output.format: Use 0 to display Percent Body Fat (\%BF) and Body Density (BD) together; use 1 to display \%BF only; use 2 to display BD only.
#'
#'
#' @references Jackson, A.S., and M.L. Pollock. 1978. Generalized equations for predicting body density of men. British Journal of Nutrition 40: 497-504.
#'
#' Jackson, A.S., M.L. Pollock, and A. Ward. 1980. Generalized equations for predicting body density of women. Medicine & Science in Sports & Exercise 12: 175-181.
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
#'
#' # Predicting % body fat
#'
#' df <- Jackson.3sites(Sex = 1, CH = 34, AB = 24, TH = 24, TR = 17, SI = 33,
#'                      Age = 28, Equation = 1, Output.format = 0)
#'
#'
#' @export

Jackson.3sites <- function(Sex, CH, AB, TH, TR, SI, Age, Equation, Output.format) {
  BD <- Sex_options_Jackson3(Sex, CH, AB, TH, TR, SI, Age)
  PBF <- PBF.Equation(Equation, BD)
  df <- output.equation(Output.format, PBF, BD)
  return(df)
}

