getwd()
setwd('RPG_PJT')
game <- read.csv('Video_Games(정제2).csv')
View(game)
summary(game)
# Name, U_Score, Rating 칼럼을 제거하고 새로운 df생성해서 사용
Game <- game[, !(names(game) %in% c("Name", "U_Score", "Rating"))]
summary(Game)



#####################################################################################
# 모델링!!!!!!!!!!!!!###############################################################


# 장르/평점에 따른 글로벌매출 예측 모델 개발 -----------------------------------

# 데이터셋을 훈련 데이터와 테스트 데이터로 분할
set.seed(123)
train_indices <- sample(1:nrow(Game), nrow(Game) * 0.9)  # 90%를 훈련 데이터로 사용
train_data <- Game[train_indices, ]
test_data <- Game[-train_indices, ]


# 다중 선형 회귀 모델 구축
model <- lm(Global_ ~ Genre + C_Score, data = train_data)

# 모델의 예측 성능 평가
predictions <- predict(model, newdata = test_data)
errors <- test_data$Global_ - predictions
mean_squared_error <- mean(errors^2)
mean_squared_error


# 모델 사용해보기------------------

# 예측에 사용할 입력 변수
OVERWATCH <- data.frame(Genre = "Shooter", C_Score = 91)
OVERWATCH 
# 모델을 통한 예측
predicted_value <- predict(model, newdata = OVERWATCH2)
predicted_value







# 글로벌매출 - 외 모든 종속변수 담은 예측 모델 개발 ------------------------------


set.seed(123)
train_indices <- sample(1:nrow(Game), nrow(Game) * 0.7)  # 70%를 훈련 데이터로 사용
train_data <- Game[train_indices, ]
test_data <- Game[-train_indices, ]

# 다중 선형 회귀 모델 구축
model <- lm(Global_ ~ ., data = train_data)

# 테스트 데이터에 대한 예측 수행
y_pred <- predict(model, newdata = test_data)

# 실제 값과 예측 값 비교 및 평가 지표 계산
mse <- mean((y_pred - test_data$Global_)^2)
mae <- mean(abs(y_pred - test_data$Global_))
r_squared <- summary(model)$r.squared

# 평가 지표 출력
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("R-squared:", r_squared))









# 모델 요약 출력
summary(model)


# Call:
#   lm(formula = Global_ ~ ., data = Game)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.193309 -0.006312 -0.000807  0.003683  0.240762 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                                     -1.559e-01  2.878e-01   -0.542  0.58799    
# Platform3DO                                     -3.866e-04  2.976e-02   -0.013  0.98963    
# Platform3DS                                      2.257e-03  6.395e-03    0.353  0.72407    
# PlatformDC                                       2.088e-03  7.502e-03    0.278  0.78081    
# PlatformDS                                       4.718e-04  5.738e-03    0.082  0.93447    
# PlatformGB                                       1.379e-03  6.207e-03    0.222  0.82414    
# PlatformGBA                                      2.348e-03  5.434e-03    0.432  0.66567    
# PlatformGC                                       2.175e-03  5.550e-03    0.392  0.69517    
# PlatformGEN                                      1.471e-03  9.072e-03    0.162  0.87116    
# PlatformGG                                       1.706e-04  3.821e-02    0.004  0.99644    
# PlatformN64                                      2.271e-03  5.430e-03    0.418  0.67584    
# PlatformNES                                      9.379e-04  5.907e-03    0.159  0.87384    
# PlatformNG                                       9.812e-04  1.568e-02    0.063  0.95012    
# PlatformPC                                       4.510e-03  5.949e-03    0.758  0.44843    
# PlatformPCFX                                     4.012e-04  4.920e-02    0.008  0.99349    
# PlatformPS                                      -9.813e-04  5.058e-03   -0.194  0.84616    
# PlatformPS2                                     -1.373e-03  5.443e-03   -0.252  0.80080    
# PlatformPS3                                     -1.217e-03  6.039e-03   -0.202  0.84030    
# PlatformPS4                                      1.725e-03  6.667e-03    0.259  0.79581    
# PlatformPSP                                      2.410e-04  5.831e-03    0.041  0.96703    
# PlatformPSV                                      2.088e-03  6.538e-03    0.319  0.74947    
# PlatformSAT                                     -1.334e-04  5.740e-03   -0.023  0.98146    
# PlatformSCD                                      5.038e-03  1.621e-02    0.311  0.75601    
# PlatformSNES                                     1.156e-03  5.398e-03    0.214  0.83047    
# PlatformTG16                                    -1.177e-03  3.144e-02   -0.037  0.97014    
# PlatformWii                                      2.211e-03  5.853e-03    0.378  0.70565    
# PlatformWiiU                                    -3.188e-03  6.977e-03   -0.457  0.64770    
# PlatformWS                                       2.252e-03  1.639e-02    0.137  0.89073    
# PlatformX360                                     6.373e-04  5.954e-03    0.107  0.91477    
# PlatformXB                                       1.005e-03  5.499e-03    0.183  0.85493    
# PlatformXOne                                     6.147e-03  6.819e-03    0.901  0.36742    
# Year                                             7.813e-05  1.447e-04    0.540  0.58923    
# GenreAdventure                                  -1.682e-03  1.426e-03   -1.180  0.23819    
# GenreFighting                                   -3.032e-03  1.567e-03   -1.935  0.05298 .  
# GenreMisc                                       -1.767e-04  1.215e-03   -0.145  0.88440    
# GenrePlatform                                   -8.632e-04  1.506e-03   -0.573  0.56661    
# GenrePuzzle                                      2.290e-05  1.866e-03    0.012  0.99021    
# GenreRacing                                     -1.367e-03  1.367e-03   -1.000  0.31730    
# GenreRole-Playing                               -3.609e-03  1.334e-03   -2.706  0.00681 ** 
#   GenreShooter                                    -1.816e-04  1.300e-03   -0.140  0.88890    
# GenreSimulation                                 -1.551e-03  1.559e-03   -0.995  0.31978    
# GenreSports                                     -4.276e-05  1.124e-03   -0.038  0.96965    
# GenreStrategy                                   -2.154e-04  1.721e-03   -0.125  0.90041    
# Publisher1C Company                              2.640e-02  3.098e-02    0.852  0.39410    
# Publisher20th Century Fox Video Games            7.246e-03  2.801e-02    0.259  0.79587    
# Publisher2D Boy                                 -5.077e-03  4.379e-02   -0.116  0.90770    
# Publisher3DO                                    -4.686e-03  2.280e-02   -0.206  0.83716    
# Publisher49Games                                -1.420e-03  4.377e-02   -0.032  0.97412    
# Publisher505 Games                               5.040e-03  2.206e-02    0.228  0.81930    
# Publisher5pb                                    -3.378e-04  2.243e-02   -0.015  0.98799    
# Publisher7G//AMES                                2.852e-02  3.097e-02    0.921  0.35716    
# Publisher989 Sports                              4.098e-02  4.378e-02    0.936  0.34923    
# Publisher989 Studios                             3.891e-03  2.417e-02    0.161  0.87211    
# PublisherAbylight                                2.561e-04  4.377e-02    0.006  0.99533    
# PublisherAcclaim Entertainment                   3.198e-03  2.208e-02    0.145  0.88487    
# PublisherAccolade                                2.526e-02  3.098e-02    0.815  0.41495    
# PublisherAckkstudios                            -6.316e-04  2.496e-02   -0.025  0.97981    
# PublisherAcquire                                 4.417e-03  2.430e-02    0.182  0.85574    
# PublisherActivision                              1.131e-03  2.193e-02    0.052  0.95888    
# PublisherActivision Blizzard                    -4.758e-03  4.379e-02   -0.109  0.91347    
# PublisherActivision Value                       -4.221e-03  2.300e-02   -0.183  0.85441    
# PublisherAdeline Software                       -3.893e-02  4.378e-02   -0.889  0.37384    
# PublisherAerosoft                               -4.179e-03  3.101e-02   -0.135  0.89281    
# PublisherAgatsuma Entertainment                 -2.788e-03  3.098e-02   -0.090  0.92829    
# PublisherAgetec                                  1.163e-02  2.567e-02    0.453  0.65042    
# PublisherAksys Games                            -2.076e-04  2.569e-02   -0.008  0.99355    
# PublisherAlawar Entertainment                    5.949e-02  3.462e-02    1.718  0.08573 .  
# PublisherAlchemist                               1.772e-03  2.263e-02    0.078  0.93760    
# PublisherAlternative Software                   -3.531e-03  2.683e-02   -0.132  0.89528    
# PublisherAltron                                 -2.286e-03  4.379e-02   -0.052  0.95836    
# PublisherAlvion                                 -1.379e-03  3.460e-02   -0.040  0.96822    
# PublisherAmerican Softworks                     -2.890e-02  4.378e-02   -0.660  0.50918    
# PublisherAngel Studios                           1.239e-03  3.107e-02    0.040  0.96820    
# PublisherAnswer Software                         1.119e-02  4.396e-02    0.255  0.79906    
# PublisherAQ Interactive                         -1.313e-02  2.769e-02   -0.474  0.63531    
# PublisherAqua Plus                               7.922e-04  2.328e-02    0.034  0.97286    
# PublisherAques                                   8.450e-04  4.378e-02    0.019  0.98460    
# PublisherArc System Works                       -5.935e-03  2.308e-02   -0.257  0.79705    
# PublisherArena Entertainment                    -3.572e-03  3.549e-02   -0.101  0.91983    
# PublisherAria                                    2.429e-03  4.376e-02    0.056  0.95573    
# PublisherArika                                  -1.843e-02  3.095e-02   -0.595  0.55158    
# PublisherArtDink                                -8.205e-03  2.528e-02   -0.325  0.74552    
# PublisherAruze Corp                              1.727e-02  3.099e-02    0.557  0.57739    
# PublisherASC Games                               5.221e-03  3.099e-02    0.169  0.86618    
# PublisherAscaron Entertainment                  -5.529e-03  4.378e-02   -0.126  0.89950    
# PublisherAscaron Entertainment GmbH             -3.777e-03  3.097e-02   -0.122  0.90295    
# PublisherASCII Entertainment                    -2.971e-03  2.352e-02   -0.126  0.89945    
# PublisherASCII Media Works                       6.593e-04  2.528e-02    0.026  0.97919    
# PublisherAsgard                                 -1.516e-03  2.570e-02   -0.059  0.95298    
# PublisherASK                                     2.887e-04  4.389e-02    0.007  0.99475    
# PublisherAsmik Ace Entertainment                 2.987e-03  3.095e-02    0.097  0.92312    
# PublisherAsmik Corp                             -1.058e-03  3.472e-02   -0.030  0.97570    
# PublisherAspyr                                  -1.494e-03  2.567e-02   -0.058  0.95360    
# PublisherAstragon                                1.774e-02  2.616e-02    0.678  0.49767    
# PublisherAsylum Entertainment                    1.093e-02  2.681e-02    0.408  0.68357    
# PublisherAtari                                   4.665e-03  2.199e-02    0.212  0.83200    
# PublisherAthena                                  2.025e-04  3.461e-02    0.006  0.99533    
# PublisherAtlus                                   3.822e-03  2.239e-02    0.171  0.86446    
# PublisherAvalon Interactive                     -9.711e-03  2.682e-02   -0.362  0.71732    
# PublisherAvanquest                              -1.731e-03  2.311e-02   -0.075  0.94029    
# PublisherAvanquest Software                      1.253e-02  2.568e-02    0.488  0.62567    
# PublisherAxela                                  -3.929e-02  4.378e-02   -0.898  0.36943    
# PublisherBAM! Entertainment                      8.163e-03  2.282e-02    0.358  0.72059    
# PublisherBandai Namco Entertainment              6.969e-03  4.377e-02    0.159  0.87350    
# PublisherBanpresto                               7.554e-04  2.234e-02    0.034  0.97302    
# PublisherBenesse                                 8.722e-03  2.682e-02    0.325  0.74500    
# PublisherBerkeley                                7.875e-04  4.378e-02    0.018  0.98565    
# PublisherBethesda Softworks                     -3.524e-03  2.233e-02   -0.158  0.87464    
# PublisherBig Ben Interactive                     1.319e-02  2.616e-02    0.504  0.61429    
# PublisherBig Fish Games                          2.530e-02  3.462e-02    0.731  0.46494    
# PublisherBigben Interactive                     -1.511e-02  2.431e-02   -0.621  0.53438    
# PublisherBigBen Interactive                      8.854e-02  4.376e-02    2.023  0.04305 *  
#   PublisherbitComposer Games                      -2.988e-02  2.772e-02   -1.078  0.28110    
# PublisherBlack Bean Games                       -3.673e-04  2.285e-02   -0.016  0.98717    
# PublisherBlack Label Games                       1.985e-03  4.376e-02    0.045  0.96382    
# PublisherBlast! Entertainment Ltd               -4.319e-03  2.680e-02   -0.161  0.87198    
# PublisherBlue Byte                              -3.691e-03  3.464e-02   -0.107  0.91516    
# PublisherBMG Interactive Entertainment           1.740e-03  2.620e-02    0.066  0.94706    
# PublisherBohemia Interactive                    -5.192e-03  4.379e-02   -0.119  0.90562    
# PublisherBomb                                    1.113e-02  4.396e-02    0.253  0.80008    
# PublisherBoost On                                3.128e-04  4.378e-02    0.007  0.99430    
# PublisherBPS                                    -2.787e-04  3.469e-02   -0.008  0.99359    
# PublisherBrash Entertainment                     1.371e-02  2.497e-02    0.549  0.58297    
# PublisherBroccoli                               -3.696e-06  2.334e-02    0.000  0.99987    
# PublisherBushiRoad                               3.759e-04  4.378e-02    0.009  0.99315    
# PublisherCapcom                                  5.365e-03  2.198e-02    0.244  0.80721    
# PublisherCave                                    6.903e-04  2.498e-02    0.028  0.97796    
# PublisherCBS Electronics                         1.150e-03  4.396e-02    0.026  0.97912    
# PublisherCCP                                    -1.802e-03  3.463e-02   -0.052  0.95850    
# PublisherCDV Software Entertainment              1.112e-02  2.680e-02    0.415  0.67812    
# PublisherChunSoft                                1.326e-03  2.366e-02    0.056  0.95531    
# PublisherCity Interactive                        2.661e-03  2.355e-02    0.113  0.91004    
# PublisherCloud Imperium Games Corporation        2.247e-03  3.463e-02    0.065  0.94827    
# PublisherCoconuts Japan                         -3.932e-03  3.464e-02   -0.114  0.90962    
# PublisherCodemasters                             1.571e-03  2.213e-02    0.071  0.94341    
# PublisherCodemasters Online                     -1.513e-03  4.379e-02   -0.035  0.97244    
# PublisherCokeM Interactive                      -9.333e-04  4.376e-02   -0.021  0.98298    
# PublisherColeco                                 -2.523e-03  2.801e-02   -0.090  0.92823    
# PublisherComfort                                 5.542e-04  2.682e-02    0.021  0.98352    
# PublisherCommseed                               -1.246e-03  4.376e-02   -0.028  0.97729    
# PublisherCompile                                 3.166e-03  2.687e-02    0.118  0.90618    
# PublisherCompile Heart                           3.271e-04  2.343e-02    0.014  0.98886    
# PublisherConspiracy Entertainment               -5.667e-04  2.412e-02   -0.023  0.98125    
# PublisherCore Design Ltd.                        1.194e-02  3.463e-02    0.345  0.73026    
# PublisherCPG Products                            1.120e-02  4.396e-02    0.255  0.79894    
# PublisherCrave Entertainment                     7.044e-04  2.235e-02    0.032  0.97486    
# PublisherCreative Core                          -3.451e-04  3.094e-02   -0.011  0.99110    
# PublisherCrimson Cow                             2.560e-02  3.463e-02    0.739  0.45977    
# PublisherCrystal Dynamics                       -3.231e-03  2.772e-02   -0.117  0.90721    
# PublisherCrytek                                  1.272e-01  4.380e-02    2.904  0.00369 ** 
#   PublisherCTO SpA                                 6.271e-03  3.463e-02    0.181  0.85630    
# PublisherCulture Brain                           2.118e-04  2.898e-02    0.007  0.99417    
# PublisherCulture Publishers                      8.562e-04  4.378e-02    0.020  0.98440    
# PublisherCyberFront                             -1.666e-04  2.413e-02   -0.007  0.99449    
# PublisherCygames                                -2.788e-03  4.382e-02   -0.064  0.94927    
# PublisherD3Publisher                             6.708e-03  2.206e-02    0.304  0.76112    
# PublisherDaedalic                               -2.698e-04  3.099e-02   -0.009  0.99305    
# PublisherDaedalic Entertainment                  5.954e-02  3.100e-02    1.921  0.05478 .  
# PublisherDaito                                   3.985e-05  2.897e-02    0.001  0.99890    
# PublisherData Age                                1.119e-02  3.486e-02    0.321  0.74809    
# PublisherData Design Interactive                -2.438e-03  3.097e-02   -0.079  0.93726    
# PublisherData East                               2.036e-03  3.467e-02    0.059  0.95317    
# PublisherDatam Polystar                          2.376e-03  3.460e-02    0.069  0.94525    
# PublisherDeep Silver                            -1.100e-04  2.216e-02   -0.005  0.99604    
# PublisherDestination Software, Inc               1.130e-02  2.448e-02    0.462  0.64426    
# PublisherDestination Software, Inc.              5.880e-02  4.378e-02    1.343  0.17927    
# PublisherDestineer                              -5.584e-04  2.261e-02   -0.025  0.98029    
# PublisherDetn8 Games                            -6.266e-02  4.377e-02   -1.432  0.15229    
# PublisherDevolver Digital                        6.100e-02  3.462e-02    1.762  0.07809 .  
# PublisherDHM Interactive                         2.626e-02  3.096e-02    0.848  0.39634    
# PublisherDigiCube                                1.162e-03  4.377e-02    0.027  0.97883    
# PublisherDisney Interactive Studios              2.473e-03  2.204e-02    0.112  0.91067    
# PublisherDorart                                 -1.100e-03  3.463e-02   -0.032  0.97465    
# Publisherdramatic create                        -2.381e-03  2.688e-02   -0.089  0.92942    
# PublisherDreamCatcher Interactive               -5.485e-03  2.374e-02   -0.231  0.81726    
# PublisherDreamWorks Interactive                  1.760e-03  4.379e-02    0.040  0.96794    
# PublisherDSI Games                              -5.177e-03  2.768e-02   -0.187  0.85166    
# PublisherDTP Entertainment                       4.008e-03  2.263e-02    0.177  0.85942    
# PublisherDusenberry Martin Racing                6.615e-02  3.465e-02    1.909  0.05630 .  
# PublisherEA Games                               -9.937e-02  4.378e-02   -2.270  0.02322 *  
#   PublisherEA Sports                               1.892e-03  4.378e-02    0.043  0.96554    
# PublisherEasy Interactive                       -1.722e-03  3.461e-02   -0.050  0.96032    
# PublisherEcole                                   3.582e-03  4.378e-02    0.082  0.93479    
# PublisherEdia                                   -1.019e-03  3.463e-02   -0.029  0.97652    
# PublisherEidos Interactive                       5.245e-04  2.206e-02    0.024  0.98103    
# PublisherElectronic Arts                         2.015e-04  2.193e-02    0.009  0.99267    
# PublisherElectronic Arts Victor                 -1.364e-02  3.463e-02   -0.394  0.69371    
# PublisherElf                                     7.891e-04  3.474e-02    0.023  0.98188    
# PublisherElite                                   2.561e-03  4.379e-02    0.058  0.95337    
# PublisherEmpire Interactive                     -4.645e-03  2.250e-02   -0.206  0.83649    
# PublisherEncore                                 -5.025e-04  2.896e-02   -0.017  0.98616    
# PublisherEnix Corporation                        3.796e-04  2.302e-02    0.016  0.98684    
# PublisherEnjoy Gaming ltd.                      -1.226e-03  4.376e-02   -0.028  0.97765    
# PublisherEnterbrain                              1.343e-03  2.398e-02    0.056  0.95533    
# PublisherEON Digital Entertainment               1.203e-03  4.378e-02    0.027  0.97808    
# PublisherEpic Games                             -3.267e-03  4.380e-02   -0.075  0.94055    
# PublisherEpoch                                   2.474e-05  2.628e-02    0.001  0.99925    
# PublisherErtain                                 -4.218e-04  3.460e-02   -0.012  0.99027    
# PublisherESP                                     1.542e-03  2.777e-02    0.056  0.95571    
# PublisherEssential Games                         7.881e-04  3.095e-02    0.025  0.97968    
# [ reached getOption("max.print") -- omitted 437 rows ]
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.03788 on 16079 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 1.849e+06 on 636 and 16079 DF,  p-value: < 2.2e-16


# 모델 성능평가--------------------







