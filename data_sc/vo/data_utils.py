import argparse
import pandas as pd
import itertools
import numpy as np
import warnings
from sklearn.preprocessing import LabelEncoder, LabelBinarizer, OneHotEncoder, StandardScaler
from sklearn.model_selection import train_test_split
import cPickle as pickle
import datetime
import re
warnings.filterwarnings('ignore')

def create_dummy(df, index, col_name, label_binariser=None, fill_na=None):
    '''Dummyfy variables.

       If fill_na provided, replace NAs with it (during inference),
       otherwise, fill_na with values bigger than the maximum by 1.
    '''
    v_unique = sorted(df.unique())
    if any(df.isnull()):
        if fill_na is None:
            fill_na = df.dropna().max() + 1
        df = df.fillna(fill_na)

    ## if there are only two unique values,
    ## the number of columns will be one,
    ## otherwise equal to the number of columns.
    n_unique = len(v_unique)
    n_cols = n_unique if n_unique > 2 else 1
    if label_binariser is None:
        label_binariser = LabelBinarizer()
        tmp_df = pd.DataFrame(label_binariser.fit_transform(df.values), columns=map(lambda x : col_name + '_' + str(v_unique[x]), np.arange(n_cols)), index=index)
    else:
        tmp_df = pd.DataFrame(label_binariser.transform(df.values), columns=map(lambda x : col_name + '_' + str(v_unique[x]), np.arange(n_cols)), index=index)
    return tmp_df, (label_binariser, fill_na)

def n_na(df_usual):
    """Show per-column NA total counts and percentage."""
    return(pd.concat([df_usual.isnull().sum(), df_usual.isnull().sum() / len(df_usual)],
                     axis=1, keys=['total', 'percent']).query('total > 0').sort_values(['percent'], ascending=False))

def prepare_Xy(campaigns_file, transactions_file,
               fill_na, dummyfy, norm,
               output_folder,
               seed,
               verbose=False,
               drop_scen_name=True,
               drop_uhod_kg=False):
    """Prepare training and testing data."""

    ## parse arguments
    if isinstance(fill_na, list):
        fill_na_cmpn, fill_na_trns = fill_na
    else:
        fill_na_cmpn = fill_na_trns = fill_na

    if isinstance(dummyfy, list):
        dummyfy_cmpn, dummyfy_trns = dummyfy
    else:
        dummyfy_cmpn = dummyfy_trns = dummyfy

    if isinstance(norm, list):
        norm_cmpn, norm_trns = norm
    else:
        norm_cmpn = norm_trns = norm

    df_usual = pd.read_csv(campaigns_file, delimiter=';', encoding='cp1251')
    if drop_uhod_kg:
        df_usual = df_usual[~(df_usual['SCENARIO_NAME'] == u'Uhod_\u0458\u0407')].reset_index(drop=True)
    loyalty_columns = list(filter(lambda x: 'IS_LOYALTY' in x,
                                  df_usual.columns))
    df_usual[loyalty_columns] = df_usual[loyalty_columns].fillna(0)
    df_usual['EDU'] = df_usual.CL_EDU.map({'TEC' : 2,
                                           'HIG' : 4,
                                           'POS' : 0,
                                           'SEC' : 1,
                                           'UND' : 3,
                                           'TWO' : 5})
    if verbose:
        print("Creating new feature EDU from CL_EDU")
        print("{'TEC' : 2, 'HIG' : 4, 'POS' : 0, 'SEC' : 1, 'UND' : 3, 'TWO' : 5}")
        print("")
        print("Creating new feature HAS_CHLD")
    df_usual['HAS_CHLD'] = 1 * (df_usual['QTY_CHLD'] > 0)

    config = dict()
    config['campaigns'] = dict()

    bal_columns = list(filter(lambda x: 'BAL' in x, df_usual.columns))
    if verbose:
        print("")
        print("Filling in {} with 0s".format(bal_columns))
    df_usual.loc[:, bal_columns] = df_usual.loc[:, bal_columns].fillna(0)

    trns_sum_cols = list(filter(lambda x: re.match('^N_|^AMT_|^MEAN_', x), df_usual.columns))
    if verbose and (trns_sum_cols != []):
        print("")
        print("Filling in {} with 0s".format(trns_sum_cols))
    df_usual.loc[:, trns_sum_cols] = df_usual.loc[:, trns_sum_cols].fillna(0)

    if fill_na_cmpn:
        config['campaigns']['fill_na'] = dict()
        if verbose:
            print("")
            print("NaNs distribution for campaigns (before filling in)")
            print(n_na(df_usual))

        config['campaigns']['fill_na']['median'] = dict()
        fill_median = ['QTY_CHLD', 'AGE', 'LIM']
        values_median = []
        if verbose:
            print("")
            print("Filling in NAs in {} using the median value".format(fill_median))
        for c_ in fill_median:
            v_median = df_usual[c_].dropna().median()
            values_median.append(v_median)
            df_usual[c_].fillna(v_median, inplace=True)

        config['campaigns']['fill_na']['median'][tuple(fill_median)] = values_median

        config['campaigns']['fill_na']['sampling'] = dict()
        sampling_cols = ['EDU', 'CL_INDUSTR', 'CL_AUTO_EX', 'FL_GOLD', 'FL_ZP', 'FL_P24', 'BANK']
        if verbose:
            print("")
            print("Filling in missing values in {} by sampling".format(sampling_cols))
        for c_ in sampling_cols:
            c_values = df_usual[c_].dropna()
            na_indices = np.where(df_usual[c_].isnull())[0]
            config['campaigns']['fill_na']['sampling'][c_] = c_values.values.tolist()
            df_usual[c_].fillna(dict(zip(na_indices, np.random.choice(c_values, size=len(na_indices)))), inplace=True)

        fill_cols = df_usual.columns.intersection(['POS_ATM', 'LIMIT_CHURN', 'ID', 'sms_cc'])
        fill_number = -1.
        if verbose:
            print("")
            print("Filling in missing values in scenarios cols {} with {}".format(fill_cols, fill_number))
        for c_ in fill_cols:
            df_usual[c_].fillna(fill_number, inplace=True)

    drop_cols = ['REP_RLCL_MAXCRIT', 'PAYMENT', 'REP_CLID', 'DATE_END', 'CLIENT_ID', 'CL_EDU', 'HAS_SD'] + drop_scen_name * ['SCENARIO_NAME']
    config['campaigns']['drop_cols'] = drop_cols
    if verbose:
        print("")
        print("Dropping {}".format(drop_cols))
    df_usual.drop(drop_cols, axis=1, inplace=True)
    if verbose:
        print("")
        print("NaNs distribution for campaigns")
        print(n_na(df_usual))

    if transactions_file is not None:
        config['transactions'] = dict()
        df_trns = pd.read_csv(transactions_file, delimiter=';', encoding='cp1251')
        if fill_na_trns:
            config['transactions']['fill_na'] = dict()
            if verbose:
                print("")
                print("NaNs distribution for transactions (before filling in)")
                print(n_na(df_trns))

            config['transactions']['fill_na']['sampling'] = dict()
            sampling_cols = ['CODE']
            if verbose:
                print("")
                print("Filling in missing values in {} by sampling".format(sampling_cols))
            for c_ in sampling_cols:
                c_values = df_trns[c_].dropna()
                na_indices = np.where(df_trns[c_].isnull())[0]
                config['transactions']['fill_na']['sampling'][c_] = c_values.values.tolist()
                df_trns[c_].fillna(dict(zip(na_indices, np.random.choice(c_values, size=len(na_indices)))), inplace=True)

        if verbose:
            print("")
            print("NaNs distribution for transactions")
            print(n_na(df_trns))

    if verbose:
        print("****" * 5)
        print("Creating stratified train/test splits")

        print("")
        print("Dropping columns with weird age values < 18")
    df_usual.drop(df_usual[df_usual.AGE < 18].index, inplace=True)

    y = df_usual.label
    X = df_usual.drop('label', axis=1)

    np.random.seed(seed)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, stratify=y)

    id_cols = ['CONTRACT_REF', 'DATE_START']

    trn_cref_dt = X_train.ix[:, id_cols]
    tst_cref_dt = X_test.ix[:, id_cols]

    X_train.drop(id_cols, axis=1, inplace=True)
    X_test.drop(id_cols, axis=1, inplace=True)

    if norm_cmpn is not None:
        config['campaigns']['normalisation'] = dict()
        if verbose:
            print("")
            print("{} normalisation".format(norm))
        bal_cols = list(filter(lambda x: 'BAL_' in x, X_train.columns))#['BAL_1', 'BAL_2', 'BAL_3']
        trns_sum_cols = list(filter(lambda x: re.match('^N_|^AMT_|^MEAN_', x), X_train.columns))
        cols_to_normalise = [bal_cols] + (trns_sum_cols + ['AGE', 'QTY_CHLD', 'LIM'])
        for c_ in cols_to_normalise:
            if norm == 'min-max':
                min_val = X_train[c_].dropna().values.min()
                max_val = X_train[c_].dropna().values.max()
                numerator, denominator = min_val, max_val - min_val
                if verbose:
                    print("Columns (before): {}, Min: {}, max: {}".format(c_, min_val, max_val))
                X_train.loc[X_train[c_].notnull().index, c_] = X_train[c_].dropna().apply(lambda x: (x - min_val) / (max_val - min_val))
                X_test.loc[X_test[c_].notnull().index, c_] = X_test[c_].dropna().apply(lambda x: (x - min_val) / (max_val - min_val))
                min_val = X_train[c_].dropna().values.min()
                max_val = X_train[c_].dropna().values.max()
                if verbose:
                    print("Columns (after): {}, Min: {}, max: {}".format(c_, min_val, max_val))
            else:
                mean_val = X_train[c_].dropna().values.mean()
                std_val = X_train[c_].dropna().values.std()
                numerator, denominator = mean_val, std_val
                if verbose:
                    print("Columns (before): {}, Mean: {}, std: {}".format(c_, mean_val, std_val))
                X_train.loc[X_train[c_].notnull().index, c_] = X_train[c_].dropna().apply(lambda x: (x - mean_val) / (std_val))
                X_test.loc[X_test[c_].notnull().index, c_] = X_test[c_].dropna().apply(lambda x: (x - mean_val) / (std_val))
                mean_val = X_train[c_].dropna().values.mean()
                std_val = X_train[c_].dropna().values.std()
                if verbose:
                    print("Columns (after): {}, Mean: {}, std: {}".format(c_, mean_val, std_val))
            if isinstance(c_, list):
                c_ = tuple(c_)
            config['campaigns']['normalisation'][c_] = (numerator, denominator)

    ## Yes/No cols
    yn_cols = ['FL_GOLD', 'FL_ZP', 'FL_P24', 'CONTROL_GROUP_FLG', 'CL_AUTO_EX']
    if verbose:
        print("")
        print("Binarising Y/N columns {}".format(yn_cols))
    for c_ in yn_cols:
        X_train[c_] = X_train[c_].map({'N' : 0,
                                       'Y' : 1})
        X_test[c_] = X_test[c_].map({'N' : 0,
                                     'Y' : 1})

    all_df = pd.concat([X_train, X_test], axis=0)

    ## scenarios
    config['scenarios'] = dict()
    s_cols = all_df.columns[all_df.columns.str.contains('^sms_cc|^ID|^LIMIT_CHURN|^POS_ATM')]
    config['scenarios']['s_cols'] = s_cols
    u_values = dict()
    for c_ in s_cols:
        u_values[c_] = sorted(all_df[c_].unique())
    config['scenarios']['u_values'] = u_values

    ## transform object columns
    cat_cols = all_df.dtypes.pipe(lambda x: x[x == 'object']).index
    config['campaigns']['digitising'] = dict()
    if verbose:
        print("")
        print("Digitising categorical columns {}".format(cat_cols))
    for c_ in cat_cols:
        if c_ == 'SCENARIO_NAME':
            pass
        else:
            label = LabelEncoder()
            all_df[c_] = label.fit_transform(all_df[c_].astype(str))
            if verbose:
                print("")
                print("Column : {}".format(c_))
                print(zip(all_df[c_].unique(), label.inverse_transform(all_df[c_].unique())))

            config['campaigns']['digitising'][c_] = label

    if dummyfy_cmpn:
        config['campaigns']['dummy'] = dict()
        dummy_cols = df_usual.columns.intersection(['EDU', 'ID', 'CL_INDUSTR', 'BANK'])
        if verbose:
            print("")
            print("Dummyfying columns {}".format(dummy_cols))

        for c_ in dummy_cols:
            tmp_df, lbl_bin = create_dummy(all_df[c_], all_df.index, c_, fill_na=None)
            config['campaigns']['dummy'][c_] = lbl_bin
            #tmp_df = pd.get_dummies(all_df[c_], prefix=c_, drop_first=True)
            all_df = pd.concat([all_df, tmp_df],
                               axis=1)
        to_drop = dummy_cols
        if verbose:
            print("")
            print("Dropping no longer needed columns {}".format(to_drop))
        all_df.drop(to_drop, axis=1, inplace=True)

    X_test = all_df[len(X_train):]
    X_train = all_df[:len(X_train)]

    if transactions_file is not None:
        if verbose:
            print("")
            print("Preprocessing transactions data...")
        trn_cref_dt['IS_TRAIN'] = True

        df_rnn = pd.merge(df_trns, trn_cref_dt, on=['CONTRACT_REF', 'DATE_START'], how='left')
        df_rnn['IS_TRAIN'].fillna(False, inplace=True)

        if norm_trns is not None:
            config['transactions']['normalisation'] = dict()
            cols_to_normalise = ['TXN_RUB_AMT', 'n_days']
            if verbose:
                print("")
                print("{} normalisation".format(norm))
            for c_ in cols_to_normalise:
                if norm == 'min-max':
                    min_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.min()
                    max_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.max()
                    if verbose:
                        print("Columns (before): {}, Min: {}, max: {}".format(c_, min_val, max_val))
                    df_rnn.loc[df_rnn[c_].notnull().index, c_] = df_rnn[c_].dropna().apply(lambda x: (x - min_val) / (max_val - min_val))
                    numerator, denominator = min_val, max_val - min_val
                    min_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.min()
                    max_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.max()
                    if verbose:
                        print("Columns (after): {}, Min: {}, max: {}".format(c_, min_val, max_val))
                else:
                    mean_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.mean()
                    std_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.std()
                    if verbose:
                        print("Columns (before): {}, Mean: {}, std: {}".format(c_, mean_val, std_val))
                    df_rnn.loc[df_rnn[c_].notnull().index, c_] = df_rnn[c_].dropna().apply(lambda x: (x - mean_val) / (std_val))
                    numerator, denominator = mean_val, std_val
                    mean_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.mean()
                    std_val = df_rnn.loc[df_rnn['IS_TRAIN'],:][c_].dropna().values.std()
                    if verbose:
                        print("Columns (after): {}, Mean: {}, std: {}".format(c_, mean_val, std_val))
                if isinstance(c_, list):
                    c_ = tuple(c_)
                config['transactions']['normalisation'][c_] = (numerator, denominator)

        if dummyfy_trns:
            config['transactions']['dummy'] = dict()
            dummy_cols = ['TXN_TP', 'CODE']
            if verbose:
                print("")
                print("Dummyfying columns {}".format(dummy_cols))
            for c_ in dummy_cols:
                tmp_df, lbl_bin = create_dummy(df_rnn[c_], df_rnn.index, c_, fill_na=None)
                config['transactions']['dummy'][c_] = lbl_bin
                #tmp_df = pd.get_dummies(all_df[c_], prefix=c_, drop_first=True)
                #tmp_df = pd.get_dummies(all_df[c_], prefix=c_, drop_first=True)
                df_rnn = pd.concat([df_rnn, tmp_df],
                                   axis=1)
                df_rnn.drop(c_, axis=1, inplace=True)

        ## transform object columns
        cat_cols = filter(lambda x: x == 'TXN_TP', df_rnn.dtypes.pipe(lambda x: x[x == 'object']).index)
        config['transactions']['digitising'] = dict()
        if verbose:
            print("")
            print("Digitising categorical columns {}".format(cat_cols))
        for c_ in cat_cols:
            label = LabelEncoder()
            df_rnn[c_] = label.fit_transform(df_rnn[c_].astype(str))
            if verbose:
                print("")
                print("Column : {}".format(c_))
                print(zip(df_rnn[c_].unique(), label.inverse_transform(df_rnn[c_].unique())))

            config['transactions']['digitising'][c_] = label

        drop_cols = ['CONTRACT_REF', 'DATE_START', 'n_trns', 'IS_TRAIN', 'ID']
        n_input = len(df_rnn.columns) - len(drop_cols) + 1 # +id
        config['transactions']['n_input'] = n_input
        trn_rnn_df = df_rnn.loc[df_rnn['IS_TRAIN'],:]
        trn_cref_dt.drop(['IS_TRAIN'], axis=1, inplace=True)
        trn_cref_dt['ID'] = np.arange(len(trn_cref_dt))
        trn_rnn_df = pd.merge(trn_rnn_df, trn_cref_dt, how='left', on=['CONTRACT_REF', 'DATE_START'])
        id_trn = trn_rnn_df.ID.unique()


        trn_rnn = []
        for i in np.arange(len(trn_cref_dt)):
            if i in id_trn:
                tmp = trn_rnn_df.loc[trn_rnn_df['ID'] == i, :].sort_values(by='n_trns', axis=0, ascending=True)
                trn_rnn.append(tmp.drop(drop_cols, axis=1).values)
            else:
                trn_rnn.append(np.zeros(shape=[1, n_input]))

        tst_rnn_df = df_rnn.loc[~df_rnn['IS_TRAIN'],:]

        tst_cref_dt['ID'] = np.arange(len(tst_cref_dt))
        tst_rnn_df = pd.merge(tst_rnn_df, tst_cref_dt, how='left', on=['CONTRACT_REF', 'DATE_START'])

        id_tst = tst_rnn_df.ID.unique()

        tst_rnn = []
        for i in np.arange(len(tst_cref_dt)):
            if i in id_tst:
                tmp = tst_rnn_df.loc[tst_rnn_df['ID'] == i, :].sort_values(by='n_trns', axis=0, ascending=True)
                tst_rnn.append(tmp.drop(drop_cols, axis=1).values)
            else:
                tst_rnn.append(np.zeros(shape=[1, n_input]))
        assert (len(trn_rnn) == len(trn_cref_dt))
        assert (len(tst_rnn) == len(tst_cref_dt))

    if transactions_file is not None:
        all_trn = (trn_rnn, X_train, y_train)
        all_tst = (tst_rnn, X_test, y_test)
    else:
        all_trn = (X_train, y_train)
        all_tst = (X_test, y_test)
        n_input = ''


    file_name = datetime.datetime.today().strftime("%Y-%d-%m") + '_nfeats_' * (transactions_file != None) + str(n_input) + '_norm_' + str(norm) + '_fillNA_' + str(fill_na) + '_dummy_' + str(dummyfy)
    with open(output_folder + '/' + 'train_' + file_name, 'wb') as fp:
        pickle.dump(all_trn, fp)
    with open(output_folder + '/' + 'test_' + file_name, 'wb') as fp:
        pickle.dump(all_tst, fp)
    with open(output_folder + '/' + file_name + '.config', 'wb') as fp:
        pickle.dump(config, fp)
    return (all_trn, all_tst, config)


def prepare_Xscen(campaigns_file,
                  config_file,
                  scenarios_file,
                  transactions_file,
                  verbose=False):
    """Prepare X and scenarios file for inference."""
    df_usual = pd.read_csv(campaigns_file, delimiter=';', encoding='cp1251')
    loyalty_columns = list(filter(lambda x: 'IS_LOYALTY' in x,
                                  df_usual.columns))
    df_usual[loyalty_columns] = df_usual[loyalty_columns].fillna(0)
    df_usual['EDU'] = df_usual.CL_EDU.map({'TEC' : 2,
                                           'HIG' : 4,
                                           'POS' : 0,
                                           'SEC' : 1,
                                           'UND' : 3,
                                           'TWO' : 5})
    if verbose:
        print("Creating new feature EDU from CL_EDU")
        print("{'TEC' : 2, 'HIG' : 4, 'POS' : 0, 'SEC' : 1, 'UND' : 3, 'TWO' : 5}")
        print("")
        print("Creating new feature HAS_CHLD")
    df_usual['HAS_CHLD'] = 1 * (df_usual['QTY_CHLD'] > 0)

    with open(config_file, 'rb') as fp:
        config = pickle.load(fp)

    config_c = config['campaigns']

    bal_columns = list(filter(lambda x: 'BAL' in x, df_usual.columns))
    if verbose:
        print("")
        print("Filling in {} with 0s".format(bal_columns))
    df_usual.loc[:, bal_columns] = df_usual.loc[:, bal_columns].fillna(0)

    trns_sum_cols = list(filter(lambda x: re.match('^N_|^AMT_|^MEAN_', x), df_usual.columns))
    if verbose and (trns_sum_cols != []):
        print("")
        print("Filling in {} with 0s".format(trns_sum_cols))
    df_usual.loc[:, trns_sum_cols] = df_usual.loc[:, trns_sum_cols].fillna(0)

    if 'fill_na' in config_c.keys():
        if verbose:
            print("")
            print("NaNs distribution for campaigns (before filling in)")
            print(n_na(df_usual))

        fill_median, values_median = config_c['fill_na']['median'].items()[0]
        if verbose:
            print("")
            print("Filling in NAs in {} using the median value".format(fill_median))
        for c_, v_ in zip(fill_median, values_median):
            df_usual[c_].fillna(v_, inplace=True)

        sampling_cols = config_c['fill_na']['sampling'].keys()
        if verbose:
            print("")
            print("Filling in missing values in {} by sampling".format(sampling_cols))
        for c_ in sampling_cols:
            na_indices = np.where(df_usual[c_].isnull())[0]
            df_usual[c_].fillna(dict(zip(na_indices, np.random.choice(config_c['fill_na']['sampling'][c_], size=len(na_indices)))), inplace=True)

        fill_cols = ['POS_ATM', 'LIMIT_CHURN', 'ID', 'sms_cc']
        fill_number = -1.
        if verbose:
            print("")
            print("Filling in missing values in scenarios cols {} with {}".format(fill_cols, fill_number))
        for c_ in fill_cols:
            df_usual[c_].fillna(fill_number, inplace=True)

    drop_cols = df_usual.columns.intersection(config_c['drop_cols'])#['REP_RLCL_MAXCRIT', 'PAYMENT', 'REP_CLID', 'DATE_END', 'CLIENT_ID', 'CL_EDU', 'HAS_SD', 'SCENARIO_NAME']
    if verbose:
        print("")
        print("Dropping {}".format(drop_cols))
    df_usual.drop(drop_cols, axis=1, inplace=True)
    if verbose:
        print("")
        print("NaNs distribution for campaigns")
        print(n_na(df_usual))

    if transactions_file is not None:
        df_trns = pd.read_csv(transactions_file, delimiter=';', encoding='cp1251').drop('DATE_START', axis=1)
        config_t = config['transactions']
        if 'fill_na' in config_t.keys():
            if verbose:
                print("")
                print("NaNs distribution for transactions (before filling in)")
                print(n_na(df_trns))

            sampling_cols = config_t['fill_na']['sampling'].keys()
            if verbose:
                print("")
                print("Filling in missing values in {} by sampling".format(sampling_cols))
            for c_ in sampling_cols:
                na_indices = np.where(df_trns[c_].isnull())[0]
                df_trns[c_].fillna(dict(zip(na_indices, np.random.choice(config_t['fill_na']['sampling'][c_], size=len(na_indices)))), inplace=True)
        if verbose:
            print("")
            print("NaNs distribution for transactions")
            print(n_na(df_trns))

    if verbose:
        print("****" * 5)
        print("")
        print("Dropping columns with weird age values < 18")
    df_usual.drop(df_usual[df_usual.AGE < 18].index, inplace=True)

    if 'label' in df_usual.columns:
        X = df_usual.drop('label', axis=1)
    else:
        X = df_usual
    id_cols = ['CONTRACT_REF', 'DATE_START']

    trn_cref_dt = X.ix[:, id_cols]

    X.drop(id_cols, axis=1, inplace=True)

    if 'normalisation' in config_c.keys():
        cols_to_normalise = config_c['normalisation'].keys()
        for c_ in cols_to_normalise:
            if verbose:
                print("")
                print("{} normalisation".format(c_))
            numerator, denominator = config_c['normalisation'][c_]
            if isinstance(c_, tuple):
                c_ = list(c_)
            X.loc[X[c_].notnull().index, c_] = X[c_].dropna().apply(lambda x: (x - numerator) / (denominator))

    ## Yes/No cols
    yn_cols = X.columns.intersection(['FL_GOLD', 'FL_ZP', 'FL_P24', 'CONTROL_GROUP_FLG', 'CL_AUTO_EX'])
    if verbose:
        print("")
        print("Binarising Y/N columns {}".format(yn_cols))
    for c_ in yn_cols:
        X[c_] = X[c_].map({'N' : 0, 'Y' : 1})


    ## transform object columns
    cat_cols = X.columns.intersection(config_c['digitising'].keys())
    if verbose:
        print("")
        print("Digitising categorical columns {}".format(cat_cols))
    for c_ in cat_cols:
        label = config_c['digitising'][c_]
        X[c_] = label.transform(X[c_].astype(str))
        if verbose:
            print("")
            print("Column : {}".format(c_))

    dummy_cols = None
    if 'dummy' in config_c.keys():
        dummy_cols = config_c['dummy'].keys()
        if verbose:
            print("")
            print("Dummyfying columns {}".format(dummy_cols))
        for c_ in dummy_cols:
            label_binariser, fill_na = config_c['dummy'][c_]
            tmp_df, _ = create_dummy(X[c_], X.index, c_, label_binariser=label_binariser, fill_na=fill_na)
            X = pd.concat([X, tmp_df],
                               axis=1)

        to_drop = dummy_cols
        if verbose:
            print("")
            print("Dropping no longer needed columns {}".format(to_drop))
        X.drop(to_drop, axis=1, inplace=True)

    if transactions_file is not None:
        if verbose:
            print("")
            print("Preprocessing transactions data...")
        if 'normalisation' in config_t.keys():
            cols_to_normalise = config_t['normalisation'].keys()
            for c_ in cols_to_normalise:
                if verbose:
                    print("")
                    print("{} normalisation".format(c_))
                numerator, denominator = config_t['normalisation'][c_]
                if isinstance(c_, tuple):
                    c_ = list(c_)
                df_trns.loc[df_trns[c_].notnull().index, c_] = df_trns[c_].dropna().apply(lambda x: (x - numerator) / (denominator))

        if 'dummy' in config_t.keys():
            dummy_cols = config_t['dummy'].keys()
            if verbose:
                print("")
                print("Dummyfying columns {}".format(dummy_cols))
            for c_ in dummy_cols:
                label_binariser, fill_na = config_t['dummy'][c_]
                tmp_df, _ = create_dummy(df_trns[c_], df_trns.index, c_, label_binariser=label_binariser, fill_na=fill_na)
                df_trns = pd.concat([df_trns, tmp_df],
                              axis=1)
                df_trns.drop(c_, axis=1, inplace=True)
            ## transform object columns
        cat_cols = config_t['digitising'].keys()
        if verbose:
            print("")
            print("Digitising categorical columns {}".format(cat_cols))
        for c_ in cat_cols:
            label = config_t['digitising'][c_]
            df_trns[c_] = label.transform(df_trns[c_].astype(str))
            if verbose:
                print("")
                print("Column : {}".format(c_))

        n_input = config_t['n_input'] # +id
        #trn_rnn_df = df_rnn.loc[df_rnn['IS_TRAIN'],:]
        #trn_cref_dt.drop(['IS_TRAIN'], axis=1, inplace=True)
        trn_cref_dt['ID'] = np.arange(len(trn_cref_dt))
        trn_rnn_df = pd.merge(df_trns, trn_cref_dt.drop('DATE_START', axis=1), how='left', on=['CONTRACT_REF'])
        id_trn = trn_rnn_df.ID.unique()
        drop_cols = trn_rnn_df.columns.intersection(['CONTRACT_REF', 'DATE_START', 'n_trns', 'IS_TRAIN', 'ID'])


        trn_rnn = []
        for i in np.arange(len(trn_cref_dt)):
            if i in id_trn:
                tmp = trn_rnn_df.loc[trn_rnn_df['ID'] == i, :].sort_values(by='n_trns', axis=0, ascending=True)
                trn_rnn.append(tmp.drop(drop_cols, axis=1).values)
            else:
                trn_rnn.append(np.zeros(shape=[1, n_input]))
        X = (X, trn_rnn)

    if scenarios_file is not None:
        ## generate all unique scenarios
        s_cols_ = ['sms_cc', 'ID', 'LIMIT_CHURN', 'POS_ATM']
        s_orig_df = pd.read_csv(scenarios_file, sep=';') # unique identifiers for each scenario name
        #s_cols = X.columns[X.columns.str.contains('^sms_cc|^ID|^LIMIT_CHURN|^POS_ATM')]
        s_cols = config['scenarios']['s_cols']
        u_values = config['scenarios']['u_values']

        if verbose:
            print("")
            print("Found scenario columns : {}".format(s_cols))

        ## generate possible combinations
        s_df = pd.DataFrame(columns=s_cols_)
        rows = itertools.product(*map(lambda x: u_values[x], s_cols))
        for i, r in enumerate(rows):
            s_df.loc[i, :] = r

        if dummy_cols is not None:
            for s_ in s_cols_:
                s_dummy = filter(lambda x : (s_ + '_') in x, s_cols)
                if s_dummy != []:
                    s_df = s_df[s_df[s_dummy].sum(axis=1) == 1] # only one dummy variable at a time
                    label_binariser, _ = config_c['dummy'][s_]
                    s_df[s_] = label_binariser.inverse_transform(s_df[s_dummy].values)
        s_df = pd.merge(s_orig_df, s_df, on=s_cols_, how='left')
        if verbose:
            print("")
            print("Scenario data frame")
            print(s_df)
        s_df.reset_index(inplace=True)
        return X, s_df, trn_cref_dt['CONTRACT_REF']
    else:
        return X, trn_cref_dt['CONTRACT_REF']
