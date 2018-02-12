# 백화점 세그멘테이션
"""
백화점을 이용한 고객들을 RFM을 통해 세분화 시켜보자
"""


shpInfo.A01.rfm.data <- shpInfo.A01.rfm$rfm


#recency 별로 구별
shpInfo.A01.rfm.data.rec1 <- shpInfo.A01.rfm.data[shpInfo.A01.rfm.data$RecencyClass == 1, ]
shpInfo.A01.rfm.data.rec2 <- shpInfo.A01.rfm.data[shpInfo.A01.rfm.data$RecencyClass == 2, ]
shpInfo.A01.rfm.data.rec3 <- shpInfo.A01.rfm.data[shpInfo.A01.rfm.data$RecencyClass == 3, ]


#recency-> frequency

shpInfo.A01.rfm.data.rec1.freq1 <- shpInfo.A01.rfm.data.rec1[shpInfo.A01.rfm.data.rec1$FrequencyClass == 1, ]
shpInfo.A01.rfm.data.rec1.freq2 <- shpInfo.A01.rfm.data.rec1[shpInfo.A01.rfm.data.rec1$FrequencyClass == 2, ]
shpInfo.A01.rfm.data.rec1.freq3 <- shpInfo.A01.rfm.data.rec1[shpInfo.A01.rfm.data.rec1$FrequencyClass == 3, ]

shpInfo.A01.rfm.data.rec2.freq1 <- shpInfo.A01.rfm.data.rec2[shpInfo.A01.rfm.data.rec2$FrequencyClass == 1, ]
shpInfo.A01.rfm.data.rec2.freq2 <- shpInfo.A01.rfm.data.rec2[shpInfo.A01.rfm.data.rec2$FrequencyClass == 2, ]
shpInfo.A01.rfm.data.rec2.freq3 <- shpInfo.A01.rfm.data.rec2[shpInfo.A01.rfm.data.rec2$FrequencyClass == 3, ]

shpInfo.A01.rfm.data.rec3.freq1 <- shpInfo.A01.rfm.data.rec3[shpInfo.A01.rfm.data.rec3$FrequencyClass == 1, ]
shpInfo.A01.rfm.data.rec3.freq2 <- shpInfo.A01.rfm.data.rec3[shpInfo.A01.rfm.data.rec3$FrequencyClass == 2, ]
shpInfo.A01.rfm.data.rec3.freq3 <- shpInfo.A01.rfm.data.rec3[shpInfo.A01.rfm.data.rec3$FrequencyClass == 3, ]

# recency -> frequency -> monetary


shpInfo.A01.rfm.data.rec1.freq1.monet1 <- shpInfo.A01.rfm.data.rec1.freq1[shpInfo.A01.rfm.data.rec1.freq1$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec1.freq1.monet2 <- shpInfo.A01.rfm.data.rec1.freq1[shpInfo.A01.rfm.data.rec1.freq1$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec1.freq1.monet3 <- shpInfo.A01.rfm.data.rec1.freq1[shpInfo.A01.rfm.data.rec1.freq1$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec1.freq2.monet1 <- shpInfo.A01.rfm.data.rec1.freq2[shpInfo.A01.rfm.data.rec1.freq2$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec1.freq2.monet2 <- shpInfo.A01.rfm.data.rec1.freq2[shpInfo.A01.rfm.data.rec1.freq2$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec1.freq2.monet3 <- shpInfo.A01.rfm.data.rec1.freq2[shpInfo.A01.rfm.data.rec1.freq2$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec1.freq3.monet1 <- shpInfo.A01.rfm.data.rec1.freq3[shpInfo.A01.rfm.data.rec1.freq3$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec1.freq3.monet2 <- shpInfo.A01.rfm.data.rec1.freq3[shpInfo.A01.rfm.data.rec1.freq3$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec1.freq3.monet3 <- shpInfo.A01.rfm.data.rec1.freq3[shpInfo.A01.rfm.data.rec1.freq3$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec2.freq1.monet1 <- shpInfo.A01.rfm.data.rec2.freq1[shpInfo.A01.rfm.data.rec2.freq1$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec2.freq1.monet2 <- shpInfo.A01.rfm.data.rec2.freq1[shpInfo.A01.rfm.data.rec2.freq1$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec2.freq1.monet3 <- shpInfo.A01.rfm.data.rec2.freq1[shpInfo.A01.rfm.data.rec2.freq1$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec2.freq2.monet1 <- shpInfo.A01.rfm.data.rec2.freq2[shpInfo.A01.rfm.data.rec2.freq2$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec2.freq2.monet2 <- shpInfo.A01.rfm.data.rec2.freq2[shpInfo.A01.rfm.data.rec2.freq2$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec2.freq2.monet3 <- shpInfo.A01.rfm.data.rec2.freq2[shpInfo.A01.rfm.data.rec2.freq2$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec2.freq3.monet1 <- shpInfo.A01.rfm.data.rec2.freq3[shpInfo.A01.rfm.data.rec2.freq3$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec2.freq3.monet2 <- shpInfo.A01.rfm.data.rec2.freq3[shpInfo.A01.rfm.data.rec2.freq3$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec2.freq3.monet3 <- shpInfo.A01.rfm.data.rec2.freq3[shpInfo.A01.rfm.data.rec2.freq3$MonetaryClass == 3, ]


shpInfo.A01.rfm.data.rec3.freq1.monet1 <- shpInfo.A01.rfm.data.rec3.freq1[shpInfo.A01.rfm.data.rec3.freq1$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec3.freq1.monet2 <- shpInfo.A01.rfm.data.rec3.freq1[shpInfo.A01.rfm.data.rec3.freq1$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec3.freq1.monet3 <- shpInfo.A01.rfm.data.rec3.freq1[shpInfo.A01.rfm.data.rec3.freq1$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec3.freq2.monet1 <- shpInfo.A01.rfm.data.rec3.freq2[shpInfo.A01.rfm.data.rec3.freq2$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec3.freq2.monet2 <- shpInfo.A01.rfm.data.rec3.freq2[shpInfo.A01.rfm.data.rec3.freq2$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec3.freq2.monet3 <- shpInfo.A01.rfm.data.rec3.freq2[shpInfo.A01.rfm.data.rec3.freq2$MonetaryClass == 3, ]

shpInfo.A01.rfm.data.rec3.freq3.monet1 <- shpInfo.A01.rfm.data.rec3.freq3[shpInfo.A01.rfm.data.rec3.freq3$MonetaryClass == 1, ]
shpInfo.A01.rfm.data.rec3.freq3.monet2 <- shpInfo.A01.rfm.data.rec3.freq3[shpInfo.A01.rfm.data.rec3.freq3$MonetaryClass == 2, ]
shpInfo.A01.rfm.data.rec3.freq3.monet3 <- shpInfo.A01.rfm.data.rec3.freq3[shpInfo.A01.rfm.data.rec3.freq3$MonetaryClass == 3, ]




#데이터 확인
#recency 1 frequency 1  롯데 이용률이 낮은 고객
mean(shpInfo.A01.rfm.data.rec1.freq1.monet1$Monetary)    # row:  3114   mean  193601.3  r*f*m = 1
mean(shpInfo.A01.rfm.data.rec1.freq1.monet2$Monetary)    # row:  511    mean 1127293    r*f*m = 2
mean(shpInfo.A01.rfm.data.rec1.freq1.monet3$Monetary)    # row:  28     mean 4560876    r*f*m = 3
#recency 1 frequency 2
mean(shpInfo.A01.rfm.data.rec1.freq2.monet1$Monetary)    # row:  290    mean  413801    r*f*m = 2
mean(shpInfo.A01.rfm.data.rec1.freq2.monet2$Monetary)    # row:  864    mean 1507712    r*f*m = 4
mean(shpInfo.A01.rfm.data.rec1.freq2.monet3$Monetary)    # row:  183    mean 5810591    r*f*m = 6
#recency 1 frequency 3
mean(shpInfo.A01.rfm.data.rec1.freq3.monet1$Monetary)    # row:  3      mean  519626.7  r*f*m = 3
mean(shpInfo.A01.rfm.data.rec1.freq3.monet2$Monetary)    # row:  85     mean 2119927    r*f*m = 6
mean(shpInfo.A01.rfm.data.rec1.freq3.monet3$Monetary)    # row:  137    mean 8600420    r*f*m = 9


#recency 2 frequency 1
mean(shpInfo.A01.rfm.data.rec2.freq1.monet1$Monetary)    # row:  924    mean  270973.5    r*f*m = 2
mean(shpInfo.A01.rfm.data.rec2.freq1.monet2$Monetary)    # row:  320    mean 1144455      r*f*m = 4
mean(shpInfo.A01.rfm.data.rec2.freq1.monet3$Monetary)    # row:  14     mean 4962579      r*f*m = 6
#recency 2 frequency 2
mean(shpInfo.A01.rfm.data.rec2.freq2.monet1$Monetary)    # row:  301    mean  436236.2    r*f*m = 4
mean(shpInfo.A01.rfm.data.rec2.freq2.monet2$Monetary)    # row:  1465   mean 1641479      r*f*m = 8
mean(shpInfo.A01.rfm.data.rec2.freq2.monet3$Monetary)    # row:  506    mean 5751054      r*f*m = 12
#recency 2 frequency 3
mean(shpInfo.A01.rfm.data.rec2.freq3.monet1$Monetary)    # row:  11     mean  512480      r*f*m = 6
mean(shpInfo.A01.rfm.data.rec2.freq3.monet2$Monetary)    # row:  438    mean 2069826      r*f*m = 12
mean(shpInfo.A01.rfm.data.rec2.freq3.monet3$Monetary)    # row:  1328  mean 10897294      r*f*m = 18



#recency 3 frequency 1
mean(shpInfo.A01.rfm.data.rec3.freq1.monet1$Monetary)    # row:  374    mean  275838.4    r*f*m = 3
mean(shpInfo.A01.rfm.data.rec3.freq1.monet2$Monetary)    # row:  111    mean 1177928      r*f*m = 6
mean(shpInfo.A01.rfm.data.rec3.freq1.monet3$Monetary)    # row:  12     mean 5160923      r*f*m = 9


#recency 3 frequency 2
mean(shpInfo.A01.rfm.data.rec3.freq2.monet1$Monetary)    # row:  194    mean  444896      r*f*m = 6
mean(shpInfo.A01.rfm.data.rec3.freq2.monet2$Monetary)    # row:  932    mean 1638087      r*f*m = 12
mean(shpInfo.A01.rfm.data.rec3.freq2.monet3$Monetary)    # row:  333    mean 5736974      r*f*m = 18

#recency 3 frequency 3
mean(shpInfo.A01.rfm.data.rec3.freq3.monet1$Monetary)    # row:  13     mean  541566.9    r*f*m = 9
mean(shpInfo.A01.rfm.data.rec3.freq3.monet2$Monetary)    # row:  617    mean 2171237      r*f*m = 18
mean(shpInfo.A01.rfm.data.rec3.freq3.monet3$Monetary)    # row:  2529  mean 16302741      r*f*m = 27







