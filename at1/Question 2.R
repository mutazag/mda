# Question 2 

m_xz = c(0,0)
m_y = c(0)

sigma_xz = (1/4) * matrix(c(3,1,1,3), nrow = 2, byrow = TRUE)
sigma_y = (1/4) * matrix(c(4), nrow = 1)
sigma_xz_y = (1/4) * matrix(c(2,2), nrow=2, byrow=TRUE)
sigma_y_xz = t(sigma_xz_y)

y = 4

conditional_variance = sigma_xz - ( sigma_xz_y %*% solve(sigma_y) %*% sigma_y_xz)
print(conditional_variance)


conditional_mean = t(m_xz) + sigma_xz_y %*% solve(sigma_y) %*% (y - m_y)
