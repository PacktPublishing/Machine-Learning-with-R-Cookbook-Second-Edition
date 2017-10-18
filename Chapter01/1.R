x=123
class(x)
x="ABC"
class(x)
x=c(1,2,3,4) # c(1:4)
x
x * 2
sqrt(x)
y = x==2
class(y)
t = c(1:10)
t
y = list(1,2,3,"ABC")
y
y = list(c(1,2,3),c("A","B","C"))
y
t = array(c(1,2,3,4), dim=c(2,2)) # Create two dimensional array
t
arr = array(c(1:24), dim=c(3,4,2)) # Creating three dimensional array
arr
m = matrix(c(1,2,3,4,5,6), nrow=3)
m
p = c(1,2,3)
q = c("A","B","C")
r = c(TRUE, FALSE, FALSE)
d = data.frame(No=p, Name=q, Attendance=r)
d
d$No
d$Name
d$Name[1]
temp = c(1:100) # Creates a vector of 100 elements from 1 to 100
temp[14:16]  # Part from vector
m[,2]    # To access second column from matrix m
m[3,]  # To access third row from matrix m
m[2,1]  # To access single element from matrix m
m[c(1,3), c(2)] # Access [1,2] and [3,2]
temp = data.frame()
edit(temp)

# Code for Reading and writing data
data()
data(iris)
class(iris)
save(iris, file="myData.RData")
load("myData.RData")
test.data = read.table(header = TRUE, text = "
a b
1 2
3 4
")

test.data = read.table(header = TRUE, text = "
a b
1 2
3 4",
col.names=c("a","b"),
row.names=c("first","second")
)
class(test.data)
write.table(test.data, file = "test.txt" , sep = " ")
write.csv(test.data, file = "test.csv")
csv.data = read.csv("test.csv", header = TRUE, row.names=1)
head(csv.data)
install.packages("RPostgreSQL")
require("RPostgreSQL")
driver = dbDriver("PostgreSQL")
connection = dbConnect(driver, dbname="restapp", host="localhost",
                       port=5432, user="postgres", password="postgres")
dbExistsTable(connection, "country")
data = dbGetQuery(connection, "select * from country")
class(data)
data


