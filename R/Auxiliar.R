
output.equation <- function(Output.format, PBF, BD) { #formato de saída do data frame da função
  saida <- data.frame(PBF, BD)
  colnames(saida) <- c("%BF", "BD")
  if (Output.format == 0) return(saida)
  if (Output.format == 1 | Output.format == 2) return(saida[Output.format])
}


PBF.Equation <- function(Equation, BD) { # equação para cálculo da densidade (1 = SIRI ou 2 = BROZEK)
  ifelse (Equation == 1,
          PBF <- ((4.95/BD) - 4.50) * 100,
          PBF <- ((4.57/BD) - 4.142) * 100)
  return(PBF)
}


Sex_options_Sloan <- function(Sex, TH, SB, SI, TR) {
  ifelse (Sex == 0,
          BD <- (1.1043 - (0.00133*TH) - (0.00131*SB)),
          BD <- (1.0764 - (0.00081*SI) - (0.00088*TR)))
  return(BD)
}


Sex_options_Guedes <- function(Sex, TR, AB, SI, TH, SB) {
  ifelse (Sex == 0,
          BD <- 1.1714 - (0.0671 * (log10(TR + AB + SI))),
          BD <- 1.1665 - (0.0706 * (log10(SI + TH + SB))))
  return(BD)
}


Sex_options_Jackson3 <- function(Sex, CH, AB, TH, TR, SI, Age) {
  ifelse (Sex == 0,
          BD <- 1.10938 - (0.0008267*(CH + AB + TH)) + (0.0000016 * ((CH + AB + TH)^2)) - (0.0002574*(Age)),
          BD <- 1.0994921 - (0.0009929 * (TR + SI + TH)) + (0.0000023 * ((TR + SI + TH)^2)) - (0.0001392*(Age)))
  return(BD)
}


Sex_options_Petroski3 <- function(Sex, TR, CH, SB, SI, TH, Age, Weight, Height) {
  ifelse(Sex == 0,
         BD <- 1.10404686 - (0.00111938 * (SB + TR + CH)) + (0.00000391 * ((SB + TR + CH)^2)) - (0.00027884 * (Age)),
         BD <- 1.04127059 - (0.00087756*(SB + SI + TH)) + (0.00000380*((SB +SI + TH)^2)) - (0.00025821*(Age)) - (0.00059076*(Weight)) + (0.00051050*(Height)))
}


Sex_options_Petroski4 <- function(Sex, TR, SB, SI, CA, MA, TH, Age, Weight, Height) {
  ifelse (Sex == 0,
          BD <- 1.10726863 - (0.00081201*(TR + SB + SI + CA)) + (0.00000212*((TR + SB + SI + CA)^2)) - (0.00041761*(Age)),
          BD <- 1.03465850 - (0.00063129*(MA + SI + TH + CA)) + (0.00000187*((MA + SI + TH + CA)^2)) - (0.00031165*(Age)) - (0.00048890*(Weight)) + (0.00051345*(Height)))
  return(BD)
}


Sex_options_Durnin4 <- function(Sex, TR, BI, SB, SI, Age) {
  ifelse (Sex == 0 & Age < 20, BD <- 1.1620 - (0.0630*log10(TR + BI + SB + SI)),
          ifelse (Sex == 0 & Age < 30, BD <- 1.1631 - (0.0632*log10(TR + BI + SB + SI)),
          ifelse (Sex == 0 & Age < 40, BD <- 1.1422 - (0.0544*log10(TR + BI + SB + SI)),
          ifelse (Sex == 0 & Age < 50, BD <- 1.1620 - (0.0700*log10(TR + BI + SB + SI)),
          ifelse (Sex == 0 & Age < 73, BD <- 1.1631 - (0.0632*log10(TR + BI + SB + SI)),
          ifelse (Sex == 1 & Age < 20, BD <- 1.1549 - (0.0678*log10(TR + BI + SB + SI)),
          ifelse (Sex == 1 & Age < 30, BD <- 1.1599 - (0.0717*log10(TR + BI + SB + SI)),
          ifelse (Sex == 1 & Age < 40, BD <- 1.1423 - (0.0612*log10(TR + BI + SB + SI)),
          ifelse (Sex == 1 & Age < 50, BD <- 1.1333 - (0.0645*log10(TR + BI + SB + SI)),
          BD <- 1.1339 - (0.0645*log10(TR + BI + SB + SI)))))))))))
    return(BD)
}


Sex_options_Jackson7 <- function(Sex, TH, SB, SI, TR, CH, AB, MA, Age) {
  ifelse (Sex == 1,
          BD <- 1.0970 - (0.00046971 * (sum(TH, SB, SI, TR, CH, AB, MA))) + (0.00000056 *((sum(TH, SB, SI, TR, CH, AB, MA))^2)) - (0.00012828 * Age),
          BD <- 1.112 - (0.00043499 * (sum(TH, SB, SI, TR, CH, AB, MA))) + (0.00000055 * ((sum(TH, SB, SI, TR, CH, AB, MA))^2)) - (0.00028826 * Age))
  return(BD)
}


Sex_options_Slaughter <- function(Sex, TR, CA) {
  ifelse(Sex == 0, PBF <- (0.735 * (TR + CA)) + 1.0, PBF <- (0.610 * (TR + CA)) + 5.1)
  return(PBF)
}


Sex_options_Slaughter_Mat <- function(Sex, Matur_lv, Race, TR, SB) {
  ifelse (sum(TR,SB) > 35 & Sex == 0, PBF <- (0.783 * (TR + SB)) + 1.6,
          ifelse (sum(TR,SB) > 35 & Sex == 1, PBF <- (0.546 * (TR + SB)) + 9.7,
          ifelse(sum(TR,SB) < 35 & Sex == 0 & Race == 0 & Matur_lv == 1, PBF <- 1.21 * (TR + SB) - (0.008 * (TR + SB)^2) - 1.7,
          ifelse(sum(TR,SB) < 35 & Sex == 0 & Race == 1 & Matur_lv == 1, PBF <- 1.21 * (TR + SB) - (0.008 * (TR + SB)^2) - 3.2,
          ifelse(sum(TR,SB) < 35 & Sex == 0 & Race == 0 & Matur_lv == 2, PBF <- 1.21 * (TR + SB) - (0.008 * (TR + SB)^2) - 3.4,
          ifelse(sum(TR,SB) < 35 & Sex == 0 & Race == 1 & Matur_lv == 2, PBF <- 1.21 * (TR + SB) - (0.008 * (TR + SB)^2) - 5.2,
          ifelse(sum(TR,SB) < 35 & Sex == 0 & Race == 0 & Matur_lv == 3, PBF <- 1.21 * (TR + SB) - (0.008 * (TR + SB)^2) - 5.5,
          ifelse(sum(TR,SB) < 35 & Sex == 0 & Race == 1 & Matur_lv == 3, PBF <- 1.21 * (TR + SB) - (0.008 * (TR + SB)^2) - 6.8,
                 PBF <- 1.33 * (TR + SB) - (0.013 * (TR + SB)) - 2.5))))))))
}



Sex_options_4YMCA <- function(Sex, AB, SI, TR, TH, Age) {
  ifelse (Sex == 0, PBF <- 0.29288 * sum(AB, SI, TR, TH) - 0.0005 * (sum(AB, SI, TR, TH)^2) + 0.15845 * (Age) - 5.76377, 0.29699 * sum(AB, SI, TR, TH) - 0.00043 * (sum(AB, SI, TR, TH)^2) + 0.02963 * (Age) - 1.4072)
}



Sex_options_3YMCA <- function(Sex, AB, SI, TR, Age) {
  ifelse (Sex == 0, PBF <- 0.39287 * sum(AB, SI, TR) - 0.00105 * (sum(AB, SI, TR)^2) + 0.15772 * (Age) - 5.18845, 0.41563 * sum(AB, SI, TR) - 0.00112 * (sum(AB, SI, TR)^2) + 0.03661 * (Age) + 4.03653)
}





body.adult.data <- data.frame(Sex = c(0,1,0,1,0,0,1,0,1,1),
                              Age = c(20,21,28,37,48,57,68,12,14,29),
                              Weight = c(80,84,76,68,71,65,91,47,51,60),
                              Height = c(171,162,173,177,161,174,177,127,140,160),
                              CH = c(17, 27, 22, 13, 22, 24, 22, 18, 16, 30),
                              SB = c(25, 22, 23, 12, 20, 27, 21, 28, 11, 23),
                              MA = c(24, 14, 11, 23, 18, 21, 22, 11, 23, 18),
                              SI = c(12, 20, 13, 19, 26, 15, 21, 23, 19, 29),
                              AB = c(17, 23, 20, 11, 28, 28, 26, 14, 12, 19),
                              TR = c(24, 15, 15, 24, 19, 26, 27, 27, 23, 16),
                              BI = c(12, 10, 20, 11, 28, 18, 24, 13, 20, 14),
                              TH = c(14, 19, 14, 28, 16, 29, 20, 22, 23, 11),
                              CA = c(27, 24, 23, 21, 30, 25, 22, 11, 20, 19))


body.children.data <- data.frame(Sex = c(0,1,0,1,0,0,1,0,1,1),
                              Age = c(11,12,13,14,15,16,13,17,9,10),
                              Weight = c(50,54,56,58,61,55,91,79,41,50),
                              Height = c(141,152,153,157,151,164,137,177,120,130),
                              Matur_lv = c(1,2,1,3,2,3,1,2,1,3),
                              Race = c(0,1,0,1,1,1,0,0,1,1),
                              CH = c(17, 27, 22, 13, 22, 24, 22, 18, 16, 30),
                              SB = c(25, 22, 23, 12, 20, 27, 21, 28, 11, 23),
                              MA = c(24, 14, 11, 23, 18, 21, 22, 11, 23, 18),
                              SI = c(12, 20, 13, 19, 26, 15, 21, 23, 19, 29),
                              AB = c(17, 23, 20, 11, 28, 28, 26, 14, 12, 19),
                              TR = c(24, 15, 15, 24, 19, 26, 27, 27, 23, 16),
                              BI = c(12, 10, 20, 11, 28, 18, 24, 13, 20, 14),
                              TH = c(14, 19, 14, 28, 16, 29, 20, 22, 23, 11),
                              CA = c(27, 24, 23, 21, 30, 25, 22, 11, 20, 19))

