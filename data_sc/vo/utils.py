import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import tensorflow as tf
from sklearn.metrics import confusion_matrix, classification_report, accuracy_score, f1_score, precision_score,auc, roc_curve, recall_score
from sklearn.utils import shuffle
from random import sample
import os

def evaluate(probs, y_test, output_folder, file_prefix='test', model_names=None):
    """Plot ROC-curve. Find optimal threshold"""
    colours = ['b', 'g', 'm', 'c', 'y', 'r', 'k']

    if not os.path.isdir(output_folder):
        os.makedirs(output_folder)
    test_log = open(output_folder + '/' + file_prefix + '.log', 'w+')

    fprs, tprs, aucs = [], [], []
    for prob, model_name in zip(probs, model_names):
        test_log.write(model_name + "\n\n")
        pred = prob.argmax(axis=1)
        test_log.write(str(classification_report(y_test, pred)) + '\n')
        test_log.write('\n' + ' Predicted' + '\n')
        test_log.write(str(confusion_matrix(y_test, pred)) + '\n')

        fpr, tpr, thr = roc_curve(y_test, prob[:, 1])
        ## find best threshold : http://www.medicalbiostatistics.com/roccurve.pdf
        dist = np.sqrt((1. - tpr) ** 2 + (fpr) ** 2)
        best_thr = thr[np.argmin(dist)]
        best_thr_pred = (prob[:,1] > best_thr) * 1

        test_log.write('\n' + "Accuracy : " + str((accuracy_score(y_test, pred))) + '\n')
        test_log.write("F1 score : " + str(f1_score(y_test, pred)) + '\n')
        test_log.write("F1 score (thrs : {:.3f}) : ".format(best_thr) + str(f1_score(y_test, best_thr_pred)) + '\n')
        test_log.write("Recall : " + str(recall_score(y_test, pred)) + '\n')
        test_log.write("Precision : " + str(precision_score(y_test, pred)) + '\n\n')

        roc_auc = auc(fpr, tpr)
        fprs.append(fpr)
        tprs.append(tpr)
        aucs.append(roc_auc)

    if len(probs) > 1:
        model_names.extend(['mean', 'geom_mean'])
        test_log.write("Ensemble (mean)\n\n")
        prob = (np.array(probs).sum(axis=0) / 2)
        pred = prob.argmax(axis=1)
        test_log.write(str(classification_report(y_test, pred)) + '\n')
        test_log.write('\n' + ' Predicted' + '\n')
        test_log.write(str(confusion_matrix(y_test, pred)) + '\n')

        test_log.write('\n' + "Accuracy : " + str((accuracy_score(y_test, pred))) + '\n')
        test_log.write("F1 score : " + str(f1_score(y_test, pred)) + '\n')
        test_log.write("Recall : " + str(recall_score(y_test, pred)) + '\n')
        test_log.write("Precision : " + str(precision_score(y_test, pred)) + '\n\n')

        fpr, tpr, _ = roc_curve(y_test, prob[:, 1])
        roc_auc = auc(fpr, tpr)
        fprs.append(fpr)
        tprs.append(tpr)
        aucs.append(roc_auc)

        test_log.write("Ensemble (geom. mean)\n\n")
        prob = (np.array(probs).prod(axis=0) / np.array(probs).prod(axis=0).sum(axis=1)[:, np.newaxis])
        pred = prob.argmax(axis=1)
        test_log.write(str(classification_report(y_test, pred)) + '\n')
        test_log.write('\n' + ' Predicted' + '\n')
        test_log.write(str(confusion_matrix(y_test, pred)) + '\n')

        test_log.write('\n' + "Accuracy : " + str((accuracy_score(y_test, pred))) + '\n')
        test_log.write("F1 score : " + str(f1_score(y_test, pred)) + '\n')
        test_log.write("Recall : " + str(recall_score(y_test, pred)) + '\n')
        test_log.write("Precision : " + str(precision_score(y_test, pred)) + '\n\n')

        fpr, tpr, _ = roc_curve(y_test, prob[:, 1])
        roc_auc = auc(fpr, tpr)
        fprs.append(fpr)
        tprs.append(tpr)
        aucs.append(roc_auc)

    #plt.figure(figsize=(15, 15))
    for fpr, tpr, roc_auc, col, name in zip(fprs, tprs, aucs, colours, model_names):
        plt.plot(fpr, tpr, col, label='[%s] AUC = %0.5f' % (name, roc_auc))

    plt.legend(loc='lower right')
    plt.plot([0, 1], [0, 1], 'r--')
    plt.xlim([0, 1])
    plt.ylim([0, 1])
    plt.ylabel('True Positive Rate')
    plt.xlabel('False Positive Rate')
    plt.savefig(output_folder + '/' + file_prefix + '_auc.png')
    plt.close()

    test_log.close()

def lrelu(x, leak=0.2, name='lrelu'):
    """Leaky ReLU."""
    with tf.variable_scope(name):
        f1 = 0.5 * (1 + leak)
        f2 = 0.5 * (1 - leak)
        return f1 * x + f2 * abs(x)

## balanced dataset
class BalancedDataset():
    '''Generate batches.
    '''
    def __init__(self, X, y, batch_size=64):#X, y, batch_size=2):
        '''
        Args:
            X: feature matrix of size (n_samples, n_features);
            y: binary labels 0 and 1 of size (n_samples,);
            batch_size: total batch_size.
        '''

        self.n_features = X.shape[1]
        self.X = X.values
        self.y = y.values.astype(int)
        self.n_examples = y.shape[0]

        self.dict_examples = dict((k, shuffle(np.where(self.y == k)[0])) for k in np.unique(self.y))
        self.idx_examples = dict((k, 0) for k in np.unique(self.y))
        self.n_classes = len(self.idx_examples)
        self.batch_size = batch_size

    def _generate_batch(self):
        '''
        Generate batch of examples for a given class.

        Returns:
            Array of features with labels as the last column.
        '''
        arr_x = np.empty(shape=(self.batch_size, self.n_features))
        arr_y = np.empty(shape=(self.batch_size, ), dtype=np.int32)
        c_idx = 0 # current idx to fill
        need_examples = self.batch_size
        n_per_class = int(need_examples / self.n_classes)
        ## first take in each class as much as possible
        for i in range(self.n_classes):
            size_class = len(self.dict_examples[i])
            idx = self.idx_examples[i]
            n_ = min(idx + n_per_class, size_class)
            add_ = n_ - idx
            arr_x[c_idx : (c_idx + add_), :] = self.X[self.dict_examples[i][idx : n_], :]
            arr_y[c_idx : (c_idx + add_)] = self.y[self.dict_examples[i][idx : n_]]
            c_idx += add_
            if n_ == size_class: # nullify and shuffle if the end is reached
                self.idx_examples[i] = 0
                self.dict_examples[i] = shuffle(self.dict_examples[i])
            else:
                self.idx_examples[i] = n_
        ## if need_examples > 0 then we can take
        ## by one example from some classes
        #sh_classes = shuffle(range(self.n_classes))
        while True:
            if need_examples - c_idx > 0: ## need to add examples
                ## choose random class
                i = sample(range(self.n_classes), 1)[0]#sh_classes.pop()
                size_class = len(self.dict_examples[i])
                idx = self.idx_examples[i]
                n_ = min(idx + 1, size_class)
                add_ = n_ - idx
                arr_x[c_idx : (c_idx + add_)] = self.X[self.dict_examples[i][idx : n_], :]
                arr_y[c_idx : (c_idx + add_)] = self.y[self.dict_examples[i][idx : n_]]
                c_idx += add_
                if n_ == size_class: # nullify and shuffle if the end is reached
                    self.idx_examples[i] = 0
                    self.dict_examples[i] = shuffle(self.dict_examples[i])
                else:
                    self.idx_examples[i] = n_
            else:
                break
        return (arr_x, arr_y)

    def next_batch(self):
        '''Generate next batch for training.'''
        batch_x, batch_y = self._generate_batch()
        batch_x, batch_y = shuffle(batch_x, batch_y)
        return batch_x, batch_y

class Dataset():
    '''Generate batches.
    '''
    def __init__(self, X, y, batch_size=2):
        '''
        Args:
            X: feature matrix of size (n_samples, n_features);
            y: binary labels 0 and 1 of size (n_samples,);
            batch_size: total batch_size.
        '''

        self.n_features = X.shape[1]
        self.n_examples = X.shape[0]

        self.Xy = np.concatenate([X, y[:, np.newaxis]], axis=1)

        ## initial shuffling
        np.random.shuffle(self.Xy)

        # indicators
        self.n_epochs = 0
        self.idx = 0

        self.batch_size = batch_size

    def _generate_batch(self):
        '''
        Generate batch of examples for a given class.

        Returns:
            Array of features with labels as the last column.
        '''
        arr = np.empty(shape=(self.batch_size, self.n_features + 1)) # n_features + 1 for label
        ## need to work with neg
        c_idx = 0 # current idx to fill
        need_examples = self.batch_size
        while True:
            ## first take as much as possible
            n_ = min(self.idx + need_examples, self.n_examples)
            add_ = n_ - self.idx ## how much else we need
            arr[c_idx : (c_idx + add_)] = self.Xy[self.idx : n_, :]
            # update current idx and the number of examples
            c_idx += add_
            need_examples -= add_
            if need_examples != 0:
                ## means that we reached the end
                self.idx = 0
                ## shuffle
                np.random.shuffle(self.Xy)
                self.n_epochs += 1
                #print("Epoch_neg #: %d" % self.epoch_neg)
            else:
                self.idx += add_
                break
        return arr

    def next_batch(self):
        '''Generate next batch for training.'''
        batch = self._generate_batch()
        np.random.shuffle(batch)
        return batch[:, :-1], batch[:, -1]

slim = tf.contrib.slim
def batch_normalisation(x, name, is_training, activation_fn=None, scale=True):
    with tf.variable_scope(name) as scope:
        output = slim.batch_norm(x,
                                 activation_fn=activation_fn,
                                 is_training=is_training,
                                 updates_collections=None,
                                 scale=scale,
                                 scope=scope)
    return output

class DataGenerator():
    """ Generate sequence of data with dynamic length.
    This class generate samples for training:
    - Class 0: linear sequences (i.e. [0, 1, 2, 3,...])
    - Class 1: random sequences (i.e. [1, 3, 10, 7,...])

    NOTICE:
    We have to pad each sequence to reach 'max_seq_len' for TensorFlow
    consistency (we cannot feed a numpy array with inconsistent
    dimensions). The dynamic calculation will then be performed thanks to
    'seqlen' attribute that records every actual sequence length.
    """
    def __init__(self,
                 data,
                 y,
                 max_seq_len=20, min_seq_len=1):
        self.data = []
        self.labels = y#pd.get_dummies(y).values
        self.seqlen = []
        for el in data:
            c_len = len(el)
            self.seqlen.append(c_len)
            if c_len < max_seq_len:
                # padding
                el = np.pad(el, [(0, max_seq_len - c_len), (0, 0)], 'constant')
            self.data.append(el)
        self.batch_id = 0

    def next(self, batch_size):
        """ Return a batch of data. When dataset end is reached, start over.
        """
        if self.batch_id == len(self.data):
            self.batch_id = 0
            self.data, self.labels, self.seqlen = shuffle(self.data, self.labels, self.seqlen)
        batch_data = (self.data[self.batch_id:min(self.batch_id +
                                                  batch_size, len(self.data))])
        batch_labels = (self.labels[self.batch_id:min(self.batch_id +
                                                  batch_size, len(self.data))])
        batch_seqlen = (self.seqlen[self.batch_id:min(self.batch_id +
                                                  batch_size, len(self.data))])
        self.batch_id = min(self.batch_id + batch_size, len(self.data))
        return batch_data, batch_labels, batch_seqlen

def dynamicRNN(x, seqlen, seq_max_len, n_hidden):

    # Prepare data shape to match `rnn` function requirements
    # Current data input shape: (batch_size, n_steps, n_input)
    # Required shape: 'n_steps' tensors list of shape (batch_size, n_input)

    # Unstack to get a list of 'n_steps' tensors of shape (batch_size, n_input)
    x = tf.unstack(x, seq_max_len, 1)

    # Define a lstm cell with tensorflow
    multi_cell = tf.contrib.rnn.MultiRNNCell([tf.contrib.rnn.BasicLSTMCell(n_h) for n_h in n_hidden])

    # Get lstm cell output, providing 'sequence_length' will perform dynamic
    # calculation.
    outputs, states = tf.contrib.rnn.static_rnn(multi_cell, x, dtype=tf.float32,
                                                sequence_length=seqlen)

    # When performing dynamic calculation, we must retrieve the last
    # dynamically computed output, i.e., if a sequence length is 10, we need
    # to retrieve the 10th output.
    # However TensorFlow doesn't support advanced indexing yet, so we build
    # a custom op that for each sample in batch size, get its length and
    # get the corresponding relevant output.

    # 'outputs' is a list of output at every timestep, we pack them in a Tensor
    # and change back dimension to [batch_size, n_step, n_input]
    outputs = tf.stack(outputs)
    outputs = tf.transpose(outputs, [1, 0, 2])

    # Hack to build the indexing and retrieve the right output.
    batch_size = tf.shape(outputs)[0]
    # Start indices for each sample
    index = tf.range(0, batch_size) * seq_max_len + (seqlen - 1)
    # Indexing
    outputs = tf.gather(tf.reshape(outputs, [-1, n_hidden[-1]]), index)

    # Linear activation, using outputs computed above
    return outputs #tf.matmul(outputs, weights['out']) + biases['out']

def save(saver, sess, logdir):
   '''Save weights.

   Args:
     saver: TensorFlow Saver object.
     sess: TensorFlow session.
     logdir: path to the snapshots directory.
   '''
   model_name = 'model.ckpt'
   checkpoint_path = os.path.join(logdir, model_name)

   if not os.path.exists(logdir):
      os.makedirs(logdir)
   saver.save(sess, checkpoint_path)
   print('The checkpoint has been created.')

def load(saver, sess, ckpt_path):
    '''Load trained weights.

    Args:
      saver: TensorFlow Saver object.
      sess: TensorFlow session.
      ckpt_path: path to checkpoint file with parameters.
    '''
    saver.restore(sess, ckpt_path)
    print("Restored model parameters from {}".format(ckpt_path))

def report_softmax(gt, preds, n_classes, ignore_label=None):
    """
    Return accuracies and confusion matrix on multi-class predictions.
    If ignore_label is given, print also accuracy on all labels,
    except for it.

    Args:
      gt: vector of ground truth labels.
      preds: array of predictions of size (len(gt), n_classes).
      n_classes: number of classes.
      ignore_label: if given, print also accuracy on all labels, except for it.
    """
    df = pd.DataFrame(columns=['all_labels'] + range(n_classes))
    df.loc['recall', 'all_labels'] = np.mean(preds.argmax(axis=1) == gt)
    for i in range(n_classes):
        df.loc['recall', i] = np.mean(preds[gt == i].argmax(axis=1) == gt[gt == i]) # recall
        df.loc['precision', i] = np.mean(preds.argmax(axis=1)[preds.argmax(axis=1) == i]
                                == gt[preds.argmax(axis=1) == i]) # precision
        df.loc['fscore', i] = (2. * (df.loc['recall', i] * df.loc['precision', i]) /
                    (df.loc['recall', i] + df.loc['precision', i])) # f1-score
    df_cm = pd.DataFrame(confusion_matrix(gt, preds.argmax(axis=1)))
    if ignore_label is not None:
        df.loc['recall', 'no_' + str(ignore_label)] = (np.mean(preds[gt != ignore_label]
                                                .argmax(axis=1) ==
                                                    gt[gt != ignore_label]))
    return df, df_cm
