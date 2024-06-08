# Carregar pacotes necessários
library(multcompView)

results <- read.table('./results.csv',sep=',',header=TRUE)

metrics <- list('mAP50','mAP75','mAP','precision','recall','fscore')

tukey_result <- TukeyHSD(aov(formula = results$mAP50 ~ results$ml)) # Aplicar método Tukey HSD (Honest Significant Difference)
cld <- multcompLetters4(aov(formula = results$mAP50 ~ results$ml), tukey_result) # Adicionar as letras compactas de diferença (CLD)

print('1 mAP50'); print(tukey_result); print(cld)


tukey_result <- TukeyHSD(aov(formula = results$mAP75 ~ results$ml))
cld <- multcompLetters4(aov(formula = results$mAP75 ~ results$ml), tukey_result)

print('2 mAP75'); print(tukey_result); print(cld)


tukey_result <- TukeyHSD(aov(formula = results$mAP ~ results$ml))
cld <- multcompLetters4(aov(formula = results$mAP ~ results$ml), tukey_result)

print('3 mAP'); print(tukey_result); print(cld)


tukey_result <- TukeyHSD(aov(formula = results$precision ~ results$ml))
cld <- multcompLetters4(aov(formula = results$precision ~ results$ml), tukey_result)

print('4 precision'); print(tukey_result); print(cld)


tukey_result <- TukeyHSD(aov(formula = results$recall ~ results$ml))
cld <- multcompLetters4(aov(formula = results$recall ~ results$ml), tukey_result)

print('5 recall'); print(tukey_result); print(cld)


tukey_result <- TukeyHSD(aov(formula = results$fscore ~ results$ml))
cld <- multcompLetters4(aov(formula = results$fscore ~ results$ml), tukey_result)

print('6 fscore'); print(tukey_result); print(cld)