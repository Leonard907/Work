import matplotlib.pyplot as plt
import numpy as np

# Read data
data = open("/Users/leonard/Coursera-ML-AndrewNg-Notes/code/ex1_linear_regression/ex1/ex1data1.txt")
# Store data in a matrix (97 x 2)
data_grid = np.array([list(map(float, line.split(","))) for line in data])

# Initialize Ɵ
theta = np.zeros((1, data_grid.shape[1]))
# Initialize X by adding bias
X = np.vstack([np.ones(data_grid.shape[0]), data_grid[:, 0]])
# Initialize y
y = np.array([data_grid[:, 1]])
# Learning rate
alpha = 0.01
# number of training examples
m = X.shape[1]


# Update Ɵ
def update():
    global theta, X, y, alpha, m
    theta = theta - alpha * (np.dot(theta, X) - y).dot(X.T) / m


def compute_cost():
    return np.sum(np.sum((np.dot(theta, X) - y))) ** 2 / (2 * m)


cost_iter = []
for i in range(1000):  # epochs = 100
    update()
    cost_iter.append(compute_cost())

# plot training examples
plt.subplot(121)
plt.scatter(data_grid.T[0], data_grid.T[1], c="blue", marker=".", label="Training data")

# plot regression line
x_val = np.linspace(5, 30)
y_val = theta[:, 0] + theta[:, 1] * x_val
plt.plot(x_val, y_val, color="red", label="Regression Line")
plt.xlabel("Population of City in 10,000s")
plt.ylabel("Profit in $10,000s")
plt.legend()

# plot cost descent
plt.subplot(122)
plt.plot(np.linspace(1, 1000, 1000), cost_iter)
plt.xlabel("Iterations")
plt.ylabel("Cost")
plt.show()


