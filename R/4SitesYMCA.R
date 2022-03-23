
#' @title Percent Body Fat Estimates Using Golding, Myers and Sinning (1989).
#'
#'
#' @name YMCA.4sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 4 sites skinfold measurements using the equations of Jackson and Pollock (1978); Jackson, Pollock and Ward (1980) adapted by Golding, Myers and Sinning (1989). Population Specifications:  Both sexes (Males: aged 18-61 and Females: aged 18 - 55) from Young Men's Christian Association (YMCA).
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param AB a value representing Abdominal Skinfold. Direction of Fold: Vertical. Anatomical Reference: Umbilicus. Measurement: Fold is taken vertically 2 cm lateral to the umbilicus.
#'
#' @param SI a value representing Suprailiac Skinfold. Direction of Fold: Diagonal. Anatomical Reference: Iliac crest. Measurement: Fold is taken diagonally above the iliac crest along the anterior axillary line.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param TH a value or a vector representing Thigh Skinfold measurements. Direction of Fold: Vertical (midlin). Anatomical Reference: Inguinal crease and patella. Measurement: Fold is lifted on anterior aspect of thigh midway between inguinal crease and proximal border of patella. Body weight is shifted to left foot and caliper is applied 1 cm below fingers.
#'
#'
#' @param Age a value or a vector representing the age (in years) from subject assessed.
#'
#'
#'
#'
#' @details Sex:Use 0 for Male and 1 for Female.
#'
#'
#'
#' @references Golding, L., C. Myers, and W. Sinning. 1989. Y’s way to physical fitness. Champaign, IL: Human Kinetics.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML,  et al.  Skinfold thicknesses and measurements technique.  In:  Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Siri, W. E. Body composition from fluid space and density. In: BROZEK, J.; HANSCHEL, A. (Eds.). Techniques for measuring body composition. Washing, D.C.:National Academy of Science, 1961. p. 223-224.
#'
#' Brozek J., F. Grande, J. Anderson, et al. 1963. Densitometric analysis of body composition: Revision of some quantitative assumptions. Annals of the New York Academy of Sciences 110: 113-140.
#'
#' Jackson, A.S., and M.L. Pollock. 1978. Generalized equations for predicting body density of men. British Journal of Nutrition 40: 497-504.
#'
#' Jackson, A.S., M.L. Pollock, and A. Ward. 1980. Generalized equations for predicting body density of women. Medicine & Science in Sports & Exercise 12: 175-18.
#'
#'
#'
#' @examples
#'
#' #Predicting % body fat
#'
#' df <- YMCA.4sites(Sex = 1, AB = 19, SI = 17, TR = 13, TH = 22,
#'                   Age = 27)
#'
#'
#'
#' @export
#'

YMCA.4sites <- function(Sex, AB, SI, TR, TH, Age) {
  PBF <- Sex_options_4YMCA(Sex, AB, SI, TR, TH, Age)
  df <- output.equation(1, PBF, NA)
  return(df)
}
