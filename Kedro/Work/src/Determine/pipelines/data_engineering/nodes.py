# Best of Kedro challenge:
# Digit Recognition

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split


def split_data(X: pd.DataFrame, y: pd.DataFrame):
    # split data into training set & test set
    X_train, X_test, y_train, y_test = train_test_split(X, y)
    return dict(
        train_x=X_train,
        train_y=y_train,
        test_x=X_test,
        test_y=y_test
    )


