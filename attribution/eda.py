import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
# import category_encoders as ce
from matplotlib import pyplot as plt

plt.rcParams.update({'figure.figsize': (12.0, 8.0)})
plt.rcParams.update({'font.size': 14})

cortex = pd.read_csv("cortex_pull.csv")
print(cortex.info())
print(cortex.describe())

# find the best method of imputation here instead of dropping

cortex = cortex.dropna()
print(cortex.describe())
print(cortex.info())

# unique concat id's need to be dropped for categorical variable assignment

X = cortex
y = cortex['$']

# taking samples here for testing machine learning method code

# X = cortex.head(n=100)
# y = cortex.head(n=100)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25,
                                                    random_state=12)
# print(X_train.describe())
# print(y_train.describe())

categorical = [var for var in cortex.columns if cortex[var].dtype=='O']
print('There are {} categorical variables\n'.format(len(categorical)))
print('The categorical variables are :\n\n', categorical)
print(cortex[categorical].isnull().sum())

encoder = ce.OneHotEncoder(cols=['ConcatId', 'Program Name', 'Retailers',
                                 'Tactic', 'Tactic Category', 'Vendor',
                                 'Tactic Start Date', 'Tactic End Date',
                                 'Brand', 'Left_Right_ConcatId',
                                 'RMN', 'Right_ConcatId'])

# X_train = encoder.fit_transform(X_train)
# X_test = encoder.transform(X_test)
# y_train = encoder.fit_transform(y_train)
# y_test = encoder.transform(y_test)

print(X_train.head())

# rfc = RandomForestClassifier(random_state=0)
# rfc.fit(X_train, y_train)

