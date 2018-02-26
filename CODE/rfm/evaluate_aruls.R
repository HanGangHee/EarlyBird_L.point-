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

temp.rules <- apriori(data = x,
                         parameter = list(support = 0.03,
                                          confidence = 0.10,
                                          minlen = 2))

shpA02.mon9.rfm.data.1.transaction <- createTransaction(A02, shpA02.mon9.rfm.data.1)
shpA02.mon9.rfm.data.1.rules <- apriori(data = shpA02.mon9.rfm.data.1.transaction,
                      parameter = list(support = 0.03,
                                       confidence = 0.10,
                                       minlen = 2))
plot(shpA02.mon9.rfm.data.1.rules)
shpA02.mon9.rfm.data.2.transaction <- createTransaction(A02, shpA02.mon9.rfm.data.2)
shpA02.mon9.rfm.data.2.rules <- apriori(data = shpA02.mon9.rfm.data.2.transaction,
                                        parameter = list(support = 0.03,
                                                         confidence = 0.10,
                                                         minlen = 2))
plot(shpA02.mon9.rfm.data.2.rules)

shpA02.mon9.rfm.data.3.transaction <- createTransaction(A02, shpA02.mon9.rfm.data.3)
shpA02.mon9.rfm.data.3.rules <- apriori(data = shpA02.mon9.rfm.data.3.transaction,
                                        parameter = list(support = 0.03,
                                                         confidence = 0.10,
                                                         minlen = 2))
plot(shpA02.mon9.rfm.data.3.rules)

shpA02.mon9.rfm.data.4.transaction <- createTransaction(A02, shpA02.mon9.rfm.data.4)
shpA02.mon9.rfm.data.4.rules <- apriori(data = shpA02.mon9.rfm.data.4.transaction,
                                        parameter = list(support = 0.03,
                                                         confidence = 0.10,
                                                         minlen = 2))
plot(shpA02.mon9.rfm.data.4.rules)

getwd()
save(list = "shpA02.mon9.rfm.data.1.transaction", file = paste("shpA02.mon9.rfm.data.1.transaction", ".Rda", sep = ""))
save(list = "shpA02.mon9.rfm.data.2.transaction", file = paste("shpA02.mon9.rfm.data.2.transaction", ".Rda", sep = ""))
save(list = "shpA02.mon9.rfm.data.3.transaction", file = paste("shpA02.mon9.rfm.data.3.transaction", ".Rda", sep = ""))
save(list = "shpA02.mon9.rfm.data.4.transaction", file = paste("shpA02.mon9.rfm.data.4.transaction", ".Rda", sep = ""))




shpA03.mon9.rfm.data.1.transaction <- createTransaction(A03, shpA03.mon9.rfm.data.1)
shpA03.mon9.rfm.data.2.transaction <- createTransaction(A03, shpA03.mon9.rfm.data.2)
shpA03.mon9.rfm.data.3.transaction <- createTransaction(A03, shpA03.mon9.rfm.data.3)
shpA03.mon9.rfm.data.4.transaction <- createTransaction(A03, shpA03.mon9.rfm.data.4)
save(list = "shpA03.mon9.rfm.data.1.transaction", file = paste("shpA03.mon9.rfm.data.1.transaction", ".Rda", sep = ""))
save(list = "shpA03.mon9.rfm.data.2.transaction", file = paste("shpA03.mon9.rfm.data.2.transaction", ".Rda", sep = ""))
save(list = "shpA03.mon9.rfm.data.3.transaction", file = paste("shpA03.mon9.rfm.data.3.transaction", ".Rda", sep = ""))
save(list = "shpA03.mon9.rfm.data.4.transaction", file = paste("shpA03.mon9.rfm.data.4.transaction", ".Rda", sep = ""))



shpA04.mon9.rfm.data.1.transaction <- createTransaction(A04, shpA04.mon9.rfm.data.1)
shpA04.mon9.rfm.data.2.transaction <- createTransaction(A04, shpA04.mon9.rfm.data.2)
shpA04.mon9.rfm.data.3.transaction <- createTransaction(A04, shpA04.mon9.rfm.data.3)
shpA04.mon9.rfm.data.4.transaction <- createTransaction(A04, shpA04.mon9.rfm.data.4)
save(list = "shpA04.mon9.rfm.data.1.transaction", file = paste("shpA04.mon9.rfm.data.1.transaction", ".Rda", sep = ""))
save(list = "shpA04.mon9.rfm.data.2.transaction", file = paste("shpA04.mon9.rfm.data.2.transaction", ".Rda", sep = ""))
save(list = "shpA04.mon9.rfm.data.3.transaction", file = paste("shpA04.mon9.rfm.data.3.transaction", ".Rda", sep = ""))
save(list = "shpA04.mon9.rfm.data.4.transaction", file = paste("shpA04.mon9.rfm.data.4.transaction", ".Rda", sep = ""))



class(x)
summary(x)
inspect(x[1:10])
itemFrequency(x, type = 'absolute')
itemFrequencyPlot(shpA02.mon9.rfm.data.3.transaction, support = 0.10, main = "A02 그룹 1 item frequency plot above support 10%")



#temp.rules에 확인하고 싶은 rule을 본다.

summary(temp.rules)
inspect(sort(temp.rules, by = "lift"))
inspect(sort(temp.rules, by = "confidence"))
inspect(sort(temp.rules, by = "support"))

