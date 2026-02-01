library(randomForest)
library(ggplot2)
library(pheatmap)
library(pROC)
library(caret)
# 训练模型
# 读取分组
design = read.table("group.txt",header = T, row.names = 1, sep = "\t")
#将$Type列转换成因子factor
design$Type=as.factor(design$Type)
# 读取domain矩阵
otu_table = read.table("matrix1.txt",header = T, row.names = 1, sep = "\t")
# otu_table = read.table("matrix2.txt",header = T, row.names = 1, sep = "\t")
# otu_table = read.table("matrix3.txt",header = T, row.names = 1, sep = "\t")
otu_table = t(otu_table)
# 取出"group1"(训练集)
design_sub = subset(design, Group %in% c("group1"))
summary(design_sub)


#判断"design_sub"的行名（细菌编号）是否在结构域矩阵"out_table"中
idx = rownames(design_sub) %in% colnames(otu_table)
# 取出Group1对应的数据集
design_sub = design_sub[idx,]
otu_sub = otu_table[, rownames(design_sub)]
summary(design_sub)


# 训练构建模型
set.seed(1001)
rf = randomForest(t(otu_sub), design_sub$Type, importance=TRUE, proximity=T, ntree = 2000)
print(rf)
# 保存模型
save(rf,file = 'model1_group3_1.RData')
# 直接加载模型
#load('Gram_model.RData')
# 交叉验证选择Features
set.seed(827) # 随机数据保证结果可重复，必须
# rfcv是随机森林交叉验证函数：Random Forest Cross Validation
result = rfcv(t(otu_sub), design_sub$Type, cv.fold=5)
save(result,file = 'result_rfcv1_group3_1.RData')

# 查看错误率表，68时错误率最低，为最佳模型
result$error.cv
error_data <- as.data.frame(result$error.cv)
write.table(error_data,file = 'error1_group3_1.txt',sep = '\t',row.names = T,
            quote = F,col.names = NA)
# 绘制验证结果 
with(result,plot(n.var, error.cv, log="x", type="o", lwd=2))# 交叉验证的结果建议多做5-6次，将结果统一在一张图上

# 导出训练集观测值与预测结果
train.p = predict(rf, type = "response")
df = data.frame(observed = design_sub$Type, predict = train.p)  #提取数据的子集作为另一部分数据再预测一下
# 保存预测结果与真实结果比较
write.table(df,file = "train_predict1_group3_1.txt",quote = F,sep = '\t', row.names = T, col.names = T)



# 导出feature重要性
imp= as.data.frame(rf$importance)
imp = imp[order(imp[,1],decreasing = T),]
head(imp,n=10)
write.table(imp,file = "importance_class1_group3_1.txt",quote = F,sep = '\t', row.names = T, col.names = T)
# 简单可视化
varImpPlot(rf, main = "Top 10 - Feature importance",n.var = 8, bg = par("bg"), color = par("fg"), gcolor = par("fg"), lcolor = "gray" )



# ggplot2美化feature贡献度
# 读取所有feature贡献度
imp = read.table("importance_class1_group3_1.txt", header=T, row.names= 1, sep="\t") 
# 分析选择top23分组效果最好
imp = head(imp, n=8)
# 反向排序X轴，让柱状图从上往下画
imp = imp[order(1:8,decreasing = T),]
#将imp的按第三列从小到大排序
imp = imp[order(imp[,3]),]
# 取出列名
imp$Domain = gsub("","",rownames(imp),perl=TRUE) 

imp$Domain=factor(imp$Domain,levels = imp$Domain)

# 图1. feature重要性柱状图
library(ggplot2)
p=ggplot(data = imp, mapping = aes(x=Domain,y=MeanDecreaseAccuracy,fill=Domain)) + 
  geom_bar(stat="identity")+coord_flip()+theme_bw()+
  labs(x = "Feature", y = "MeanDecreaseAccuracy", fill = "Feature")
p
font_family <- "Times"
ggsave(p,filename = "imp_shape1.pdf",width = 8,height = 5,family = font_family)

# group2验证
# design = read.table("group_Gr_all.txt",header = T, row.names = 1)
design_test = subset(design, Group %in% c("group2")) 
summary(design_test)
idx = rownames(design_test) %in% colnames(otu_table)
design_test = design_test[idx,]
otu_sub = otu_table[,rownames(design_test)]
summary(design_test)
# 转置，并添加分组信息
otutab_t = as.data.frame(t(otu_sub))
# 将Group2的分组信息添加到domain矩阵中
# 表示按照行名将矩阵"design"中"Group"列的内容添加到矩阵"otutab_t"中并将列名设置为"otutab_t$Group中的"Group",
otutab_t$Type = design[rownames(otutab_t),]$Type

set.seed(13)
otutab.pred = predict(rf, t(otu_sub) )  
pre_tab = table(observed=otutab_t[,"Type"],predicted=otutab.pred) 
pre_tab
# 整理样本原始分组和预测分类
predict = data.frame(Type = otutab_t[,"Type"], predicted=otutab.pred)
# 保存预测结果表
write.table("SampleID\t", file=paste("RF_prediction_binary.txt",sep=""),append = F, quote = F, eol = "", row.names = F, col.names = F)
write.table(predict, file = "RF_prediction_binary.txt", 
            append = file.exists("RF_prediction_binary.txt"), 
            quote = FALSE, 
            row.names = TRUE, 
            col.names = !file.exists("RF_prediction_binary.txt"), 
            sep = "\t")


# 读取group1_matrix.txt中所有样本的ID
all_samples <- colnames(otu_table)

# 读取group.txt中已知样本的ID (group1和group2)
known_samples <- rownames(design)

# 找出未知样本ID
unknown_samples <- setdiff(all_samples, known_samples)

# 提取未知样本数据
otu_unknown <- otu_table[, unknown_samples]

# 数据处理
otu_unknown[is.na(otu_unknown)] <- 0
otu_unknown[is.nan(otu_unknown)] <- 0
otu_unknown[is.infinite(otu_unknown)] <- 0
# 使用已训练的模型进行预测
unknown_pred <- predict(rf, t(otu_unknown))

# 保存预测结果
unknown_pred_df <- data.frame(SampleID = unknown_samples, PredictedGroup = unknown_pred)
write.table(unknown_pred_df, file = "unknown_samples_prediction.txt", 
            sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


# 混淆矩阵
#训练集的预测结果
train.p = predict(rf, type = "response")
train_confusion_matrix = table(observed = design_sub$Type, predicted = train.p)
print(train_confusion_matrix)
write.table(train_confusion_matrix,file="rf_train_result.txt",quote = F,sep = '\t', row.names = T, col.names = T)

# rf训练集指标
rf_train_train_confusion_matrix <- read.csv("rf_train_result.txt", sep="\t")
actual <- c(sum(rf_train_train_confusion_matrix[1,]), sum(rf_train_train_confusion_matrix[2,]))
train_confusion_matrix <- confusionMatrix(as.table(as.matrix(rf_train_train_confusion_matrix)))
rf_train_accuracy <- as.character((round(train_confusion_matrix$overall["Accuracy"], 4)*100))
rf_train_kappa <- as.character((round(train_confusion_matrix$overall["Kappa"], 4)*100))
rf_train_negative_Recall <- as.character((round(rf_train_train_confusion_matrix[1,1]/sum(rf_train_train_confusion_matrix[1,]), 4)*100))
rf_train_positive_Recall <- as.character((round(rf_train_train_confusion_matrix[2,2]/sum(rf_train_train_confusion_matrix[2,]), 4)*100))

# 测试集的预测结果
otutab.pred = predict(rf, t(otu_sub))
test_confusion_matrix = table(observed = otutab_t$Type, predicted = otutab.pred)
print(test_confusion_matrix)
write.table(test_confusion_matrix,file="rf_test_result.txt",quote = F,sep = '\t', row.names = T, col.names = T)

# rf测试集指标
rf_test_test_confusion_matrix <- read.csv("rf_test_result.txt", sep="\t")
actual <- c(sum(rf_test_test_confusion_matrix[1,]), sum(rf_test_test_confusion_matrix[2,]))
test_confusion_matrix <- confusionMatrix(as.table(as.matrix(rf_test_test_confusion_matrix)))
rf_test_accuracy <- as.character((round(test_confusion_matrix$overall["Accuracy"], 4)*100))
rf_test_kappa <- as.character((round(test_confusion_matrix$overall["Kappa"], 4)*100))
rf_test_negative_Recall <- as.character((round(rf_test_test_confusion_matrix[1,1]/sum(rf_test_test_confusion_matrix[1,]), 4)*100))
rf_test_positive_Recall <- as.character((round(rf_test_test_confusion_matrix[2,2]/sum(rf_test_test_confusion_matrix[2,]), 4)*100))

index_train <- c(rf_train_accuracy, round(as.numeric(rf_train_kappa)/100,2), rf_train_negative_Recall, rf_train_positive_Recall)
index_test <- c(rf_test_accuracy, round(as.numeric(rf_test_kappa)/100,2), rf_test_negative_Recall,rf_test_positive_Recall)
index <- data.frame(index_train, index_test)
write.table(index, file="各指标.txt", quote = F, sep = "\t", row.names = F, col.names = T)

