import numpy as np
import matplotlib.pyplot as plt

# Read data
data = open("/Users/leonard/Coursera-ML-AndrewNg-Notes/code/ex2_logistic_regression/ex2/ex2data1.txt")
# Store data in a matrix (97 x 2)
data_grid = np.array([list(map(float, line.split(","))) for line in data])

# Separate data in different categories
data_neg = np.array([datum for datum in data_grid if datum[-1] == 0])
data_pos = np.array([datum for datum in data_grid if datum[-1] == 1])

# Initialize Ɵ
theta = np.zeros((1, data_grid.shape[1]))
# Initialize X
X = np.vstack([np.ones(data_grid.shape[0]), data_grid[:, :-1].T])
# Initialize y
y = np.array([data_grid[:, -1]])
# learning rate
alpha = 0.003
# number of training examples
m = X.shape[1]


def sigmoid(x):
    return 1 / (1 + np.exp(-x))


# Regularization is considered
def update():
    global theta, X, y, alpha, m
    theta = theta - alpha * ((sigmoid(np.dot(theta, X)) - y).dot(X.T)) / m


def cost():
    return -np.sum(np.log(sigmoid(np.dot(theta, X))) * y + np.log(1 - sigmoid(np.dot(theta, X))) * (1 - y)) / m


cost_iter = []
for i in range(200000):  # epochs = 100
    update()
    cost_iter.append(cost())

# Plot training examples
plt.subplot(121)
plt.scatter(data_neg[:, 0], data_neg[:, 1], color='red', linewidths=1, label="Rejected")
plt.scatter(data_pos[:, 0], data_pos[:, 1], color='blue', linewidths=1, label="Admitted")
plt.xlabel("Exam 1 score")
plt.ylabel("Exam 2 score")
# Plot boundary
x_val = np.linspace(25, 100)
y_val = -1 / theta[0][2] * (theta[0][0] + theta[0][1] * x_val)
plt.plot(x_val, y_val, label="Boundary")
plt.legend()

# Show cost
plt.subplot(122)
plt.plot(np.linspace(1, 200000, 200000), cost_iter)
plt.xlabel("Iterations")
plt.ylabel("Cost")
plt.show()



