a = read.table("data-de")[,2]
b = read.table("data-gradient")[,2]
c = read.table("data-gradient_selection")[,2]
d = read.table("data-kmeans")[,2]
print(c(min(a),max(a), mean(a), median(a), sd(a) ))
print(c(min(b),max(b), mean(b), median(b), sd(b) ))
print(c(min(c),max(c), mean(c), median(c), sd(c) ))
print(c(min(d),max(d), mean(d), median(d), sd(d) ))
