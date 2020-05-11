# load the data
AssessmentTrain = read.csv("assessment_train.csv")
AssessmentTest = read.csv("assessment_test.csv")

# remove observations with missing value(s)
AssessmentTrain = na.omit(AssessmentTrain)
AssessmentTest = na.omit(AssessmentTest)

# find the unordered and ordered factors
summary(AssessmentTrain)

# set the reference level of the factor
AssessmentTrain$raceeth = relevel(AssessmentTrain$raceeth, "White")
AssessmentTest$raceeth = relevel(AssessmentTest$raceeth, "White")

# build model
lmScore = lm(readingScore ~ ., data = AssessmentTrain)
summary(lmScore)

# new model
lmScore2 = lm(readingScore ~ grade + male + raceeth + expectBachelors + motherBachelors + fatherBachelors + computerForSchoolwork + read30MinsADay + publicSchool + schoolSize, data = AssessmentTrain)

# test using model 1
predict_test1 = predict(lmScore, newdata = AssessmentTest)
SSE1 = sum((predict_test1 - AssessmentTest$readingScore)^2)
SST = sum((mean(AssessmentTrain$readingScore) - AssessmentTest$readingScore)^2)
R2_1 = 1-SSE1/SST
R2_1

# test using model 2
predict_test2 = predict(lmScore2, newdata = AssessmentTest)
SSE2 = sum((predict_test2 - AssessmentTest$readingScore)^2)
R2_2 = 1-SSE2/SST
R2_2
