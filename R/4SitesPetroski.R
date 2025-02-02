
#' @title Percent Body Fat Estimates Using Petroski (1995).
#'
#'
#' @name Petroski.4sites
#'
#'
#' @description This function calculates the percent body mass estimates based on 4 sites skinfold measurements using the equations of Petroski (1995). Population Specifications: Brazilians Male (aged 18-66) and Female (aged 18-51).
#'
#'
#' @return Returns a data frame with the values of the \% body fat and body density from a subject assessed by skinfold measurements
#'
#'
#' @param Sex a value or a vector representing the sex declared by the subject. Two possible choices: 0 or 1.
#'
#' @param TR a value or a vector representing Triceps Skinfold measurements. Direction of Fold: Vertical (midline). Anatomical Reference: Acromial process of scapula and olecranon process of ulna. Measurement: Using a tape measure, distance between lateral projection of acromial process and inferior margin of olecranon process is measured on lateral aspect of arm with elbow flexed 90°. Midpoint is marked on lateral side of arm. Fold is lifted 1 cm above marked line on posterior aspect of arm. Caliper is applied at marked level.
#'
#' @param SB a value or a vector representing Subscapular  Skinfold measurements. Direction of Fold: Diagonal. Anatomical Reference: Inferior angle of scapula. Measurement: Fold is along natural cleavage line of skin just inferior to inferior angle of scapula, with caliper applied 1 cm below fingers.
#'
#' @param SI a value a vector representing Suprailiac Skinfold measurements. Direction of Fold: Oblique. Anatomical Reference: Iliac crest. Measurement: Fold is grasped posteriorly to midaxillary line and superiorly to iliac crest along natural cleavage of skin with caliper applied 1 cm below fingers.
#'
#' @param CA a value or a vector representing Calf Skinfold measurements. Direction of Fold: Vertical (medial aspect). Anatomical Reference: Maximal calf circumference. Measurement: Fold is lifted at level of maximal calf circumference on medial aspect of calf with knee and hip flexed to 90°.
#'
#' @param MA a value or a vector representing Midaxillary Skinfold measurements. Direction of Fold: Horizontal. Anatomical Reference: Xiphisternal junction (point where costal cartilage of ribs 5-6 articulates with sternum, slightly above inferior tip of xiphoid process). Measurement: Fold is taken on midaxillary line at level of xiphisternal junction.
#'
#' @param TH a value or a vector representing Thigh Skinfold measurements. Direction of Fold: Vertical (midlin). Anatomical Reference: Inguinal crease and patella. Measurement: Fold is lifted on anterior aspect of thigh midway between inguinal crease and proximal border of patella. Body weight is shifted to left foot and caliper is applied 1 cm below fingers.
#'
#' @param Age a value or a vector representing the age (in years) from subject assessed.
#'
#' @param Weight a value a vector representing the body weight in kilograms (kg).
#'
#' @param Height a value a vector representing the body stature in centimeters (cm).
#'
#' @param Equation desired estimation equation. Two possible choices: 1 or 2.
#'
#' @param Output.format desired output information. Tree possible choices: 0, 1 or 2.
#'
#'
#' @details Sex:Use 0 for Male and 1 for Female. Equation: Use 1 for Siri (1961) equation or 2 for Brozek et al. (1963) equation. Output.format: Use 0 to display Percent Body Fat (\%BF) and Body Density (BD) together; use 1 to display \%BF only; use 2 to display BD only.
#'
#'
#' @references PETROSKI, E. L. Desenvolvimento e validação de equações generalizadas para a estimativa da densidade corporal em adultos. 1995. Tese (Doutorado) – UFSM, Santa Maria.
#'
#' Harrison GG, Buskirk ER, Carter JEL, Johnston FE, Lohman TG, Pollock ML,  et al.  Skinfold thicknesses and measurements technique.  In:  Lohman TG, Roche AF, Martorell R, editors. Anthropometric standardizing reference manual. Champaign (Illinois): Human Kinetics Books; 1991. p.55-80
#'
#' International Society for the Advancement of Kinanthropometry (ISAK), 2001. International Standards for Anthropometric Assessment. (Underdale, SA, Australia.)
#'
#' Siri, W. E. Body composition from fluid space and density. In: BROZEK, J.; HANSCHEL, A. (Eds.). Techniques for measuring body composition. Washing, D.C.:National Academy of Science, 1961. p. 223-224.
#'
#' Brozek J., F. Grande, J. Anderson, et al. 1963. Densitometric analysis of body composition: Revision of some quantitative assumptions. Annals of the New York Academy of Sciences 110: 113-140.
#'
#' @examples
#'
#' #Predicting % body fat
#'
#' df <- Petroski.4sites(Sex = 1, SI = 9, CA = 17, MA = 33, TH = 12, Age = 27,
#'                       Weight = 70, Height = 180, Equation = 1, Output.format = 0)
#'
#'
#' @export
#'

Petroski.4sites <- function(Sex, TR, SB, SI, CA, MA, TH, Age, Weight, Height, Equation, Output.format) {
  BD <- Sex_options_Petroski4(Sex, TR, SB, SI, CA, MA, TH, Age, Weight, Height)
  PBF <- PBF.Equation(Equation, BD)
  df <- output.equation(Output.format, PBF, BD)
  return(df)
}
