# 
# 
# 
# n = 12345
# vec_1 = sample(12, n, replace = TRUE)
# head(vec_1)
# 
# vec_2 <- vec_1 == 3
# vec_2
# # use vec_2 to retrieve all of the 3 elements of vec_1
# vec_1[vec_2]


# n = 12345
# vec_1 = sample(12, n, replace = TRUE)
# head(vec_1)
# length(vec_1)
# sum(vec_1 == 3)

# n = 15
# for (i in 1:n)
# {
#   print(paste0("This is loop iteration:", i))
# }

# n = 17
# vec_1 = sample(10, n, replace = TRUE)
# vec_1
# for (i in 1:n)
# {
#   print(paste0("The element of vec_1 at index ",i,' is ', vec_1[i],'.'))
# }

create_and_print_vec = function(n, min=1, max=10)
{
  vec_1 = sample(min:max, n, replace = TRUE)
  for (i in 1:n)
  {
    print(paste0("The element of vec_1 at index ",i,' is ', vec_1[i],'.'))
  }
}
create_and_print_vec(8, min = 100, max = 2000)
