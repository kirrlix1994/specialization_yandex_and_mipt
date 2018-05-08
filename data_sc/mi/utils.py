
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.preprocessing import LabelBinarizer, LabelEncoder
import matplotlib.pyplot as plt
from matplotlib import cm
import matplotlib.image as image
import warnings
import itertools

from scikitplot.metrics import plot_lift_curve, plot_cumulative_gain

from sklearn.metrics import classification_report, fbeta_score, confusion_matrix, f1_score,\
                            accuracy_score, precision_score, recall_score, roc_auc_score, auc,\
                            log_loss, average_precision_score, roc_curve, precision_recall_curve

from datetime import datetime

import pickle

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
from matplotlib import cm
import matplotlib.image as image

class Dummyfier(BaseEstimator, TransformerMixin):

    def __init__(self, features=[], drop_old=True):
        self.features = features
        self.drop_old = drop_old
        self.info = {}


    def create_dummy(self, df, index, col_name, label_binariser=None, colnames=[]):
        '''Dummyfy variables.

           If there are any NAs, fill them with some temporary unique value.
           Afterwards, drop the corresponding column, and fill with NAs
           the initial rows containing NAs.
        '''
        v_unique = df[col_name].unique()
        na_indices = None
        if any(df[col_name].isnull()):
            ## fill in with some big value
            fill_na = 'NA'
            na_indices = np.where(df[col_name].isnull())[0]
            df[col_name] = df[col_name].fillna(fill_na)
            v_unique = df[col_name].unique()
        ## if there are only two unique values,
        ## the number of columns will be one,
        ## otherwise equal to the number of columns.
        n_unique = len(v_unique)
        n_cols = n_unique if n_unique > 2 else 1
        if colnames == []:
            colnames = list(map(lambda x : col_name + '_dummy_' + str(v_unique[x]), np.arange(n_cols)))
        if label_binariser is None:
            label_binariser = LabelBinarizer()
            tmp_df = pd.DataFrame(label_binariser.fit_transform(df[col_name].values), columns=colnames, index=index)
        else:
            tmp_df = pd.DataFrame(label_binariser.transform(df[col_name].values), columns=colnames, index=index)
        # if na_indices is not None:
            ## drop last column for NA class
            ## by default LabelBinarizer create columns for all unique values
            ## thus, one column will be redundant
            # if col_name + '_dummy_nan' in tmp_df.columns:
            #     tmp_df = tmp_df.drop(col_name + '_dummy_nan', axis=1)
            ## fill with NAs rows
            # tmp_df.iloc[na_indices, :] = np.nan
        ## drop last unnecessary column
        if n_cols > 2:
            tmp_df = tmp_df.drop(tmp_df.columns[-1:], axis=1)#col_name + '_dummy_nan', axis=1)
        return tmp_df, label_binariser, colnames


    def fit_transform(self, X):

        if not self.features:
            self.features = X.features

        if len(set(self.features) & set(X.columns)) != len(set(self.features)):
            raise ValueError('The listed features are not contained in the data')

        for feature in self.features:
            tmp_df, label_binariser, colnames = self.create_dummy(X, X.index, feature)
            self.info[feature] = {
                'df': tmp_df,
                'names': colnames,
                'lb': label_binariser
            }
            X = pd.concat([X, tmp_df], axis=1)

        if self.drop_old:
            X = X.drop(self.features, axis=1)

        return X


    def transform(self, X):

        if not self.info:
            raise ValueError('Must train encoder before it can be used to transform data.')

        if len(set(self.features) & set(X.columns)) != len(set(self.features)):
            raise ValueError('The listed features are not contained in the data')

        for feature in self.features:
            tmp_df, _, _ = self.create_dummy(X, X.index, feature,
                                        label_binariser=self.info[feature]['lb'],
                                        colnames=self.info[feature]['names'])
            X = pd.concat([X, tmp_df], axis=1)

        if self.drop_old:
            X = X.drop(self.features, axis=1)

        return X

    def dump(self, file_path):
        pickle.dump({'info': self.info, 'features': self.features}, open(file_path, "wb"))
        print('done')

    def load(self, file_path):
        loader = pickle.load(open(file_path, "rb"))
        self.info = loader['info']
        self.features = loader['features']
        return self



font = {'size' : 10}
plt.rc('font', **font)

import itertools

def plot_confusion_matrix(cm, classes,
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

#     print(cm)

    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, cm[i, j],
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')











    # новые фичи
def fl_pens(series):
    if series['FL_PENS'] == 0:
        return 0
    elif series['FL_PENS_DOC'] == 0:
        return 1
    else:
        return 2

def arrange_on_circle(value, unique):
    """arrange some categorial feature on circle for linear models"""
    angle = np.linspace(0, 2*np.pi, unique)[value]
    return np.sin(angle), np.cos(angle)

def zodiac(date):
    Month = date.month
    Day = date.day
    if (Month==12 and Day>=22) or (Month==1 and Day<=19):
        return "Capricorn"
    elif (Month==1 and Day>=20) or (Month==2 and Day<=17):
        return "Aquarium"
    elif (Month==2 and Day>=18) or (Month==3 and Day<=19):
        return "Pices"
    elif (Month==3 and Day>=20) or (Month==4 and Day<=19):
        return "Aries"
    elif (Month==4 and Day>=20) or (Month==5 and Day<=20):
        return "Taurus"
    elif (Month==5 and Day>=21) or (Month==6 and Day<=20):
        return "Gemini"
    elif (Month==6 and Day>=21) or (Month==7 and Day<=22):
        return "Cancer"
    elif (Month==7 and Day>=23) or (Month==8 and Day<=22):
        return "Leo"
    elif (Month==8 and Day>=23) or (Month==9 and Day<=22):
        return "Virgo"
    elif (Month==9 and Day>= 23) or (Month==10 and Day<=22):
        return "Libra"
    elif (Month==10 and Day>= 23) or (Month==11 and Day<=21):
        return "Scorpio"
    elif (Month==11 and Day>= 22) or (Month==12 and Day<=21):
        return "Sagittarius"

def prepare(df):

    # Удалим транзакционные фичи, у которых 0.75 квартиль 0:
    tr_to_drop = ['CODE_17_MEAN_180',
        'CODE_26_MEAN_180', 'CODE_26_SUM_180', 'CODE_2_MEAN_180',
        'CODE_35_MEAN_180', 'CODE_35_SUM_180', 'CODE_46_MEAN_180',
        'CODE_59_MAX_180', 'CODE_NA_MEAN_180', 'CODE_NA_MEAN_90',
        'CODE_NA_N_180', 'CODE_NA_N_90', 'TOTAL_BAL_NEG_CHNG_MAX_180',
        'TOTAL_BAL_NEG_CHNG_MAX_31', 'TOTAL_BAL_NEG_CHNG_MAX_7',
        'TOTAL_BAL_NEG_CHNG_MAX_90', 'TOTAL_BAL_NEG_CHNG_MIN_180',
        'TOTAL_BAL_NEG_CHNG_MIN_31', 'TOTAL_BAL_NEG_CHNG_MIN_90',
        'TOTAL_BAL_NEG_CHNG_N_7', 'TOTAL_BAL_NEG_CHNG_SUM_180',
        'TOTAL_BAL_NEG_CHNG_SUM_31', 'TOTAL_BAL_NEG_CHNG_SUM_7',
        'TOTAL_BAL_NEG_CHNG_SUM_90', 'TOTAL_BAL_POS_CHNG_MAX_7',
        'TOTAL_BAL_POS_CHNG_N_7', 'TOTAL_DEBT_NEG_CHNG_MAX_180',
        'TOTAL_DEBT_NEG_CHNG_MIN_180', 'TOTAL_DEBT_NEG_CHNG_N_180',
        'TOTAL_DEBT_POS_CHNG_MAX_180', 'TOTAL_DEBT_POS_CHNG_MIN_180',
        'TOTAL_DEBT_POS_CHNG_SUM_180', 'TOTAL_MEAN_31',
        'TOTAL_N_31', 'TOTAL_SUM_31', 'TXN_CHN_Internet purchase_MEAN_180',
        'TXN_CHN_Internet purchase_MEAN_90', 'TXN_CHN_Internet purchase_N_180',
        'TXN_CHN_Internet purchase_N_90', 'TXN_CHN_Internet purchase_SUM_180',
        'TXN_CHN_Internet purchase_SUM_90', 'TXN_CHN_Our ATM_MEAN_180',
        'TXN_CHN_Our ATM_N_180', 'TXN_CHN_POS terminal_MAX_31',
        'TXN_CHN_POS terminal_MEAN_31', 'TXN_CHN_POS terminal_MEAN_90',
        'TXN_CHN_POS terminal_N_31', 'TXN_CHN_POS terminal_N_90',
        'TXN_CHN_POS terminal_SUM_90', 'TXN_CHN_Unknown_N_180',
        'TXN_TP_ATM_MEAN_180', 'TXN_TP_ATM_N_180', 'TXN_TP_Credit_MEAN_90',
        'TXN_TP_Credit_N_90', 'TXN_TP_MT_MAX_180', 'TXN_TP_MT_MEAN_180',
        'TXN_TP_MT_N_180', 'TXN_TP_POS_MAX_90', 'TXN_TP_POS_MEAN_31',
        'TXN_TP_POS_MEAN_90', 'TXN_TP_POS_N_31', 'TXN_TP_POS_N_90',
        'TXN_TP_POS_SUM_90', 'TXN_TP_Purposeful credit_SUM_180']

	
    df = df.drop(tr_to_drop, axis=1)

    df['AGE'] = (datetime.today() - df['BIRTH_DATE']).apply(lambda x: int(x.days/365.25))
    df['SIN_BD_MONTH'], df['COS_BD_MONTH'] =zip(*df['BIRTH_DATE'].apply(lambda x: arrange_on_circle(x.month-1, 12)))
    df['ZODIAC'] = df['BIRTH_DATE'].apply(zodiac)

    df['FL_PEN_DOC'] = df.apply(lambda row: fl_pens(row), axis=1)

    df['LAST_CARD_STATUS_TIME'] = (df['START_DATE'] - df['LAST_CARD_STATUS_DT']).dt.days
    df['LAST_CONTR_STATUS_TIME'] = (df['START_DATE'] - df['LAST_CONTR_STATUS_DT']).dt.days
    df['CODE_67_90'] = df['CODE_67_N_90'].apply(lambda x: 0 if x == 0 else 1)

    # изменение старых фичей
    df['CODE_67_N_180'] = df['CODE_67_N_180'].apply(lambda x: 3.5 if x >= 3 else x)
    df['IS_LOYALTY_AUTO'] = df['IS_LOYALTY_AUTO'].apply(lambda x: 0 if x == 0 else 1)
    df['QTY_CHLD'] = df['QTY_CHLD'].apply(lambda x: 4 if x >= 4 else x)
#
    ##### Кодирование признака образование
    education = {
        'POS': 7, # Неполное среднее
        'SEC': 11, # Среднее
        'TEC': 11 + 1, # Среднетехническое/специальное
        'UND': 11 + 3, # Неоконченное высшее
        'HIG': 11 + 5, # Высшее
        'TWO': 11 + 5 + 3, # Два и более высших образования (закодировано аналогично высшему
        # образованию ввиду статистической незначимости различия категорий). Предыдущий вариант:
    #     'TWO': 11 + 5 + 3, # Два и более высших образования
                }
    df['CL_EDU'] = df['CL_EDU'].map(education)

    ##### Кодирование статуса карт
    card_status = {
        14: 14,
        176: 176,
        100000022: 176,
        182: 182,
        74: 74,
        63: 63,
        98: 98,
        100000100: 98,
        100000094: 98,
        100000087: 98,
        100000496: 176,
        100000576: 176
    }

    df['LAST_CARD_STATUS'] = df['LAST_CARD_STATUS'].map(card_status)

    ##### Кодирование признака пол
    sex = {
        'F': 0, # Женский
        'M': 1, # Мужской
        }
    df['REP_SEX'] = df['REP_SEX'].map(sex)

    #### Кодирование бинарных признаков
    binary = {
        'N': 0,
        'Y': 1,
    }
    df['FL_GOLD'] = df['FL_GOLD'].map(binary)
    df['FL_ZP'] = df['FL_ZP'].map(binary)
    df['FL_PENS'] = df['FL_PENS'].map(binary)
    df['FL_SOC'] = df['FL_SOC'].map(binary)
    df['FL_P24'] = df['FL_P24'].map(binary)
    df['FL_MOBB'] = df['FL_MOBB'].map(binary)
    df['FL_SMSI'] = df['FL_SMSI'].map(binary)
    df['CL_AUTO_EX'] = df['CL_AUTO_EX'].map(binary)
    df['FL_4P'] = df['FL_4P'].map(binary)
    df['FL_DOC_PROFIT'] = df['FL_DOC_PROFIT'].map(binary)
    df['FL_PENS_DOC'] = df['FL_PENS_DOC'].map(binary)
    df['FL_FRNG_PASSP'] = df['FL_FRNG_PASSP'].map(binary)
    df['FL_FRGN_TRIP'] = df['FL_FRGN_TRIP'].map(binary)

    try:
        df['CONTROL_GROUP_FLG'] = df['CONTROL_GROUP_FLG'].map(binary)
    except:
        pass
    
    list_to_drop = [
        # 'CONTRACT_REF', 'CLIENT_ID', 'SCENARIO_NAME_UNI', 'START_DATE',
        '11_AddrCity2', '11_AddrCountry', '12_AddrCity2',
        '12_AddrCountry', '13_AddrCity2', '13_AddrCountry',
        'BANK',  'CL_CCY_DOP', 'CODE_17_MAX_180',
        'CODE_17_N_180', 'CODE_17_SUM_180', 'CODE_26_MAX_180',
        'CODE_26_N_180', 'CODE_2_MAX_180', 'CODE_2_N_180',
        'CODE_2_SUM_180', 'CODE_35_MAX_180', 'CODE_35_N_180',
        'CODE_46_MAX_180', 'CODE_46_N_180', 'CODE_46_SUM_180',
        'CODE_59_MEAN_180', 'CODE_59_N_180', 'CODE_59_SUM_180',
        'CODE_67_MAX_90', 'CODE_67_MEAN_90', 'CODE_67_N_90',
        'CODE_67_SUM_180', 'CODE_67_SUM_90', 'CODE_NA_MAX_180',
        'CODE_NA_MAX_90', 'CODE_NA_SUM_180', 'CODE_NA_SUM_90',
        'FL_CR_PAN', 'FL_DIR_UCH', 'FL_STUD',
        'LIM_MAX_POS_CHANGE', 'LIM_MIN_POS_CHANGE',
        'TOTAL_BAL_NEG_CHNG_MIN_7', 'CL_CCY_ZP',
        'TOTAL_BAL_POS_CHNG_MAX_31', 'TOTAL_BAL_POS_CHNG_MIN_7',
        'TOTAL_BAL_POS_CHNG_SUM_7', 'TOTAL_DEBT_NEG_CHNG_SUM_180',
        'TOTAL_DEBT_POS_CHNG_N_180', 'TOTAL_MAX_31',
        'TXN_CHN_Internet purchase_MAX_180', 'TXN_CHN_Internet purchase_MAX_90',
        'TXN_CHN_Our ATM_MAX_180', 'TXN_CHN_Our ATM_SUM_180',
        'TXN_CHN_POS terminal_MAX_90', 'TXN_CHN_POS terminal_SUM_31',
        'TXN_CHN_Unknown_MAX_180', 'TXN_CHN_Unknown_MEAN_180',
        'TXN_CHN_Unknown_SUM_180', 'TXN_TP_ATM_MAX_180',
        'TXN_TP_ATM_SUM_180', 'TXN_TP_Credit_MAX_180',
        'TXN_TP_Credit_MAX_90', 'TXN_TP_Credit_SUM_90',
        'TXN_TP_MT_SUM_180', 'TXN_TP_POS_MAX_31', 'TXN_TP_POS_N_180',
        'TXN_TP_POS_SUM_31', 'TXN_TP_Purposeful credit_MAX_180',
        'TXN_TP_Purposeful credit_MEAN_180', 'TXN_TP_Purposeful credit_N_180',
        'BIRTH_DATE',
        'LAST_CARD_STATUS_DT', # Преобразован LAST_CARD_STATUS_TIME
        'LAST_CONTR_STATUS_DT',  # LAST_CONTR_STATUS_TIME
        ]

    #df = df.drop(list_to_drop, axis=1)

    # обработаем некоторые пустые значения:
    df.loc[:, 'CARD_STATUS_14'] = df.loc[:, 'CARD_STATUS_14'].fillna(0)
    df.loc[:, 'CARD_STATUS_176'] = df.loc[:, 'CARD_STATUS_176'].fillna(0)
    df.loc[:, 'CL_AMT_DOP'] = df.loc[:, 'CL_AMT_DOP'].fillna(0)

    return df


def precision_at(y_real, y_proba, at=0.1):
    pr_at = pd.DataFrame(data={
        'y_real': y_real,
        'y_prob': y_proba,
        'y_pred': 1
    })

    pr_at = pr_at[pr_at['y_prob']>np.sort(y_proba)[::-1][int(y_proba.shape[0]*at) - 1]].sort_values('y_prob', ascending=False)

    return precision_score(pr_at['y_real'], pr_at['y_pred'])

def print_metrics(y_real, y_proba, thr, at):
    y_pred = (y_proba >= np.sort(y_proba)[::-1][int(len(y_real) * thr)-1]).astype(int)

    if abs(np.mean(y_pred) - thr) > 0.02:
        warnings.warn('Вероятность, при которой достигается данный порог, встречается слишком часто в предсказании!\nПороговые метрики не репрезентативны')

    scores = [
        roc_auc_score(y_real, y_proba),
        average_precision_score(y_real, y_proba),
        log_loss(y_real, y_proba),
        accuracy_score(y_real, y_pred),
        precision_score(y_real, y_pred),
        precision_at(y_real, y_proba, at),
        recall_score(y_real, y_pred),
        f1_score(y_real, y_pred),
        fbeta_score(y_real, y_pred, beta=0.5),
        fbeta_score(y_real, y_pred, beta=2),
    ]

    return y_pred, scores

def plot_evaluation(cm, classes, true=None, predictions=None, cmap=plt.cm.Blues):

    fig, axes = plt.subplots(2, 3, figsize=(15, 10))

#     Confusion matrix
    axes[0, 0].imshow(cm, interpolation='nearest', cmap=cmap)
    axes[0, 0].title.set_text('Confusion matrix')
    tick_marks = np.arange(len(classes))
    axes[0, 0].set_xticks(tick_marks)
    axes[0, 0].set_yticks(tick_marks)
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        axes[0, 0].text(j, i, cm[i, j],
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")
    axes[0, 0].set_ylabel('True label')
    axes[0, 0].set_xlabel('Predicted label')

#     histogram
    if predictions is not None:
        axes[0, 1].title.set_text('Histogram of probabilities')
        axes[0, 1].hist(predictions[:, 1])
        axes[0, 1].set_xlabel('Probabilities')

# ROC curve
    fpr, tpr, thresholds = roc_curve(true, predictions[:, 1], pos_label=1)
    axes[1, 0].title.set_text('ROC curve')
    axes[1, 0].plot(fpr,tpr)
    axes[1, 0].plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')

    axes[1, 0].set_ylabel('True Positive Rate')
    axes[1, 0].set_xlabel('False Positive Rate')
    axes[1, 0].set_xlim([-0.05, 1.0])
    axes[1, 0].set_ylim([0.0, 1.05])

# PR-curve
    precision, recall, _ = precision_recall_curve(true, predictions[:, 1])
    axes[1, 1].step(recall, precision, color='b', alpha=0.2,
             where='post')
    axes[1, 1].fill_between(recall, precision, step='post', alpha=0.2,
                     color='b')

    axes[1, 1].set_xlabel('Recall')
    axes[1, 1].set_ylabel('Precision')
    axes[1, 1].set_xlim([-0.05, 1.05])
    axes[1, 1].set_ylim([0.0, 1.0])
    axes[1, 1].title.set_text('2-class Precision-Recall curve')

# Cumulative gains curve
    plot_cumulative_gain(true, predictions, ax=axes[0, 2])

# lift curve
    plot_lift_curve(true, predictions, ax=axes[1, 2])


def evaluate_clf(model,
             X_train,
             y_train,
             X_valid=None,
             y_valid=None,
             X_test=None,
             y_test=None,
             at=0.1, thr=False):

    threshold = thr if thr!=False else np.mean(y_train)

    alg = model.fit(X_train, y_train)
    # ***************** Train *****************
    y_train_prob =  alg.predict_proba(X_train)[:, 1]
    y_train_pred, train_scores = print_metrics(y_train, y_train_prob, threshold, at)

    results = pd.DataFrame(data={
        'train': train_scores}, index=['ROC AUC', 'Av Precision', 'logloss', 'Accuracy', 'Precision',
                   'Precision@%d' % (at*100), 'Recall', 'F1', 'F0.5', 'F2'])

    # ***************** Validation *****************
    if X_valid is not None:
        y_val_prob =  alg.predict_proba(X_valid)
        y_val_pred, val_scores = print_metrics(y_valid, y_val_prob[:, 1], threshold, at)
        results['valid'] = val_scores

    # ***************** Test *****************
    if X_test is not None:
        y_test_prob =  alg.predict_proba(X_valid)
        y_test_pred, val_scores = print_metrics(y_valid, y_val_prob[:, 1], threshold, at)
        results['test'] = val_scores

    print(results)
    cnf_matrix = confusion_matrix(y_valid, y_val_pred)
    plot_evaluation(cnf_matrix, classes=['0', '1'], true=y_valid, predictions=y_val_prob)
    plt.show();
    return alg, y_val_prob

def get_hist(mydf, upliftcol, controlcol, outcomecol, p = 10):
    """ calucate the uplift for each decile given an uplift prediction column
    """
    control = mydf[mydf[controlcol]==1]
    target = mydf[mydf[controlcol]==0]
    control = control.sort_values(upliftcol)
    target = target.sort_values(upliftcol)
    control = control.assign(percentile=np.floor(control[upliftcol].rank(method='first', ascending = False) /\
                                              (control.shape[0]+1) * p))
    target = target.assign(percentile=np.floor(target[upliftcol].rank(method='first', ascending = False) /\
                                            (target.shape[0]+1) * p))
    control = (control.groupby(['percentile'])[outcomecol].mean()).reset_index()
    control.columns = ["percentile", 'prob_control']
    target = (target.groupby(["percentile"])[outcomecol].mean()).reset_index()
    target.columns = ["percentile", 'prob_target']
    final = pd.merge(control,target, on = "percentile")
    final = final.assign(uplift = final["prob_target"] - final["prob_control"])
    return final

def uplift(df, controlcol, outcome_col, upliftcol) :
    tmp = df.copy()
    tmp = tmp.sort_values(upliftcol, ascending=False)
    tmp['RT'] = ((1-tmp[controlcol])*tmp[outcome_col]).cumsum()
    tmp['RC'] = (tmp[controlcol]*tmp[outcome_col]).cumsum()
    tmp['NT'] = (1-tmp[controlcol]).cumsum()
    tmp['NC'] = tmp[controlcol].cumsum()
    tmp["upc_1"] =   ((tmp['RT']/tmp['NT']).fillna(0) - (tmp['RC']/tmp['NC']).fillna(0)) *(tmp['NC']+tmp['NT'])
    return tmp["upc_1"].values
