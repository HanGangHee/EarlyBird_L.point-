shpA01.arules <- apriori(data = createSectorTransaction(shpA01),
                         parameter = list(support = 0.01,
                                          confidence = 0.20,
                                          minlen = 2))

shpA02.arules <- apriori(data = createSectorTransaction(shpA02),
                         parameter = list(support = 0.01,
                                          confidence = 0.20,
                                          minlen = 2))


shpA03.arules <- apriori(data = createSectorTransaction(shpA03),
                         parameter = list(support = 0.01,
                                          confidence = 0.20,
                                          minlen = 2))


shpA04.arules <- apriori(data = createSectorTransaction(shpA04),
                         parameter = list(support = 0.01,
                                          confidence = 0.20,
                                          minlen = 2))


shpA05.arules <- apriori(data = createSectorTransaction(shpA05),
                         parameter = list(support = 0.01,
                                          confidence = 0.20,
                                          minlen = 2))

temp.rules <- apriori(data = shp.rec1.freq1.mone1.transaction,
                         parameter = list(support = 0.01,
                                          confidence = 0.10,
                                          minlen = 2))

#temp.rules에 확인하고 싶은 rule을 본다.

summary(temp.rules)
inspect(sort(temp.rules, by = "lift"))
inspect(sort(temp.rules, by = "confidence"))
inspect(sort(temp.rules, by = "support"))

