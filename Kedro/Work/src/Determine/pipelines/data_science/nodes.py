# Copyright 2018-2019 QuantumBlack Visual Analytics Limited
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
# NONINFRINGEMENT. IN NO EVENT WILL THE LICENSOR OR OTHER CONTRIBUTORS
# BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF, OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# The QuantumBlack Visual Analytics Limited ("QuantumBlack") name and logo
# (either separately or in combination, "QuantumBlack Trademarks") are
# trademarks of QuantumBlack. The License does not grant you any right or
# license to the QuantumBlack Trademarks. You may not use the QuantumBlack
# Trademarks or any confusingly similar mark as a trademark for your product,
#     or use the QuantumBlack Trademarks in any other manner that might cause
# confusion in the marketplace, including but not limited to in advertising,
# on websites, or on software.
#
# See the License for the specific language governing permissions and
# limitations under the License.

"""Example code for the nodes in the example pipeline. This code is meant
just for illustrating basic Kedro features.

PLEASE DELETE THIS FILE ONCE YOU START WORKING ON YOUR OWN PROJECT!
"""
# pylint: disable=invalid-name

import logging
import pandas as pd
import random
import numpy as np
import matplotlib.pyplot as plt
from sklearn.neural_network import MLPClassifier
import matplotlib


def train_model(
    train_x: pd.DataFrame, train_y: pd.DataFrame, test_x: pd.DataFrame
) -> np.ndarray:
    mlp = MLPClassifier(activation='logistic', solver='sgd',
                         tol=0, learning_rate_init=.08)
    mlp.fit(train_x, train_y)
    return mlp.predict(test_x)


def report_accuracy(predictions: np.ndarray, test_y: pd.DataFrame) -> None:
    """Node for reporting the accuracy of the predictions performed by the
    previous node. Notice that this function has no outputs, except logging.
    """
    numbers_correct = 0
    for i in range(predictions.shape[0]):
        if predictions[i] == test_y.values[i]:
            numbers_correct += 1

    accuracy = numbers_correct / predictions.shape[0]
    # Log the accuracy of the model
    log = logging.getLogger(__name__)
    log.info("Model accuracy on test set: %0.2f%%", accuracy * 100)


def visualize_digit(image):
    plt.matshow(image.reshape(20, 20), cmap=matplotlib.cm.binary)  # black and white
    plt.xticks(np.array([]))  # just get rid of ticks
    plt.yticks(np.array([]))
    plt.show()


def test_result(test_x: pd.DataFrame, predictions: np.ndarray):
    while True:
        has_next = input("Another image? y/n\n")
        if has_next == "y":
            pick = random.randint(0, test_x.shape[0] - 1)
            image = test_x.values[pick]
            print("This should be:", predictions[pick] % 10)
            visualize_digit(image)
        elif has_next == "n":
            break
        else:
            continue
