before = c(56, 47, 49, 37, 38, 60, 50, 43, 43, 59, 50, 56, 54, 58)
after = c(53, 21, 32, 49, 45, 38, 44, 33, 32, 43, 53, 46, 36, 48, 39, 35, 37, 36, 39, 45)

print('Before Change - Five Point summary')
print('Min. 1st Qu.  Median  3rd Qu.    Max.')
print(fivenum(before))

print('After Change - Five Point summary')
print('Min. 1st Qu.  Median  3rd Qu.    Max.')
print(fivenum(after))


par(mfrow=c(1,2))
print('Box Plot before change')
boxplot(before,range=1,horizontal = FALSE, main="Box Plot - Before change")

print('Box Plot after change')
boxplot(after,range=1,horizontal = FALSE, main="Box Plot - After change")