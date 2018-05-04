import abc

import tensorflow as tf
from xgboost import XGBClassifier, plot_importance
import xgboost as xgb_lib
import numpy as np

from collections import Counter

import cPickle as pickle

from .utils import Dataset, DataGenerator, dynamicRNN, batch_normalisation
from .utils import BalancedDataset, lrelu, save, load


class Model(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def train(self, X_train, y_train):
        """Training model"""
        return

    @abc.abstractmethod
    def test(self, X_test, y_test=None):
        """Testing model"""
        return

    @abc.abstractmethod
    def save_model(self, path):
        """Save model parameters and configuration."""
        return

    @abc.abstractmethod
    def restore_model(self, path):
        """Restore model parameters and configuration."""
        return


class XGBoostModel(Model):
    def __init__(self, config):
        self.config = config
        self.xgb = XGBClassifier(**self.config)

    def train(self, X_train, y_train):
        _ = self.xgb.fit(X_train, y_train)

    def test(self, X_test):
        return self.xgb.predict_proba(X_test)

    def save_model(self, path, model_prefix):
        with open(path + '/' + model_prefix +'.xgb', 'wb') as fp:
            pickle.dump(self.xgb.booster(), fp)
        with open(path + '/' + model_prefix + '.config', 'wb') as fp:
            pickle.dump(self.config, fp)

    def restore_model(self, path):
        self.xgb._Booster = pickle.load(open(path, 'rb'))

slim = tf.contrib.slim

class FCModel(Model):
    """Fully-connected Neural Network."""
    def __init__(self, n_input, n_output, weights, lr, seed):
        self.graph = tf.Graph()
        self.sess = None

        act = lrelu
        n_hidden = [8] * 6
        weight_decay = 5e-4
        pos_weight = 1.1

        with self.graph.as_default():
            tf.set_random_seed(seed)
            self.input_ph = tf.placeholder(shape=[None, n_input], dtype=tf.float32)
            self.label_ph = tf.placeholder(shape=[None, ], dtype=tf.int32)
            self.is_training_ph = tf.placeholder(shape=[], dtype=tf.bool)
            weights = tf.constant(weights, dtype=tf.float32)

            with slim.arg_scope([slim.fully_connected],
                                activation_fn=act,
                                weights_initializer=tf.truncated_normal_initializer(0.0, 0.01),
                                weights_regularizer=slim.l2_regularizer(weight_decay)):
                net = self.input_ph
                for idx, n_h in enumerate(n_hidden):
                    net = slim.fully_connected(net, n_h, scope='fc' + str(idx))
                    net = batch_normalisation(net, 'bn' + str(idx), is_training=self.is_training_ph)
                logits = slim.fully_connected(net,
                                              n_output,
                                              activation_fn=None)
                self.probs = tf.nn.softmax(logits, dim=-1)

            y_label = tf.one_hot(indices=self.label_ph, depth=n_output)
            ws = tf.squeeze(tf.matmul(y_label, weights), 1)
            loss = tf.losses.softmax_cross_entropy(logits=logits,
                                                   onehot_labels=y_label,
                                                   weights=ws)
            self.reduced_loss = tf.reduce_mean(loss) + tf.add_n(tf.losses.get_regularization_losses())

            opt = tf.train.AdamOptimizer(learning_rate=lr)
            self.train_op = opt.minimize(self.reduced_loss)

    def train(self, X_train, y_train, nb_iters=0, n_iters=25000,
              batch_size=64, print_every=500):
        self.sess = tf.InteractiveSession(graph=self.graph)
        self.sess.run(tf.global_variables_initializer())
        self.graph.finalize()
        if n_iters > 0:
            train_dataset = Dataset(X_train, y_train, batch_size=batch_size)
            for i in range(n_iters):
                X_batch, y_batch = train_dataset.next_batch()
                feed_dict = {self.input_ph: X_batch,
                             self.label_ph: y_batch,
                             self.is_training_ph: True}
                _, cost = self.sess.run([self.train_op, self.reduced_loss],
                                         feed_dict=feed_dict)
                if (i > 0) and (i % print_every == 0):
                    print("Loss after %d iter. : %.4f" %
                                (i, cost))
            del train_dataset
        if nb_iters > 0:
            train_dataset = BalancedDataset(X_train, y_train,
                                            batch_size=batch_size)
            for i in range(nb_iters):
                X_batch, y_batch = train_dataset.next_batch()
                feed_dict = {self.input_ph: X_batch,
                             self.label_ph: y_batch,
                             self.is_training_ph: True}
                _, cost = self.sess.run([self.train_op, self.reduced_loss],
                                         feed_dict=feed_dict)
                if (i > 0) and (i % print_every == 0):
                    print("Loss after %d iter. : %.4f" %
                                (i, cost))
            del train_dataset

    def test(self, X_test):
        feed_dict = {self.input_ph: X_test,
                      self.is_training_ph: False}
        return self.sess.run(self.probs, feed_dict=feed_dict)

    def save_model(self, path):
        pass

    def restore_model(self, path):
        pass

class RNNModel(Model):
    """Recurrent Neural Network."""
    def __init__(self, n_input, n_classes,
                 learning_rate = 5e-3, weight_decay=3e-3,
                 n_hidden = [128], n_dist=256, seq_max_len=20, seed=42):
        self.graph = tf.Graph()
        self.sess = None
        act = tf.nn.relu
        with self.graph.as_default():
            tf.set_random_seed(seed)

            self.seq_max_len = seq_max_len

            # tf Graph input
            self.x = tf.placeholder("float", [None, self.seq_max_len, n_input])
            self.y = tf.placeholder(tf.int32, [None,])

            self.is_training_ph = tf.placeholder(shape=[], dtype=tf.bool)
            # A placeholder for indicating each sequence length
            self.seqlen = tf.placeholder(tf.int32, [None])
            pred_rnn = dynamicRNN(self.x, self.seqlen, self.seq_max_len, n_hidden)

            with slim.arg_scope([slim.fully_connected],
                                activation_fn=act,
                                weights_initializer=tf.truncated_normal_initializer(0.0, 0.01),
                                weights_regularizer=slim.l2_regularizer(weight_decay)):
                dist_rnn = slim.fully_connected(pred_rnn, n_dist)
                dist = batch_normalisation(dist_rnn,
                                           'bn_dist', is_training=self.is_training_ph, activation_fn=None)
                logits = slim.fully_connected(dist, n_classes, activation_fn=None)
            self.probs = tf.nn.softmax(logits, dim=-1)
            correct_preds = tf.equal(tf.cast(tf.argmax(self.probs, axis=1), tf.int32), self.y)
            wout_last_indices = tf.squeeze(tf.where(tf.not_equal(self.y, n_classes - 1)), 1)
            self.accuracy = tf.reduce_mean(tf.cast(correct_preds, tf.float32))
            self.accuracy_wout_last = tf.reduce_mean(tf.gather(tf.cast(correct_preds, tf.float32), wout_last_indices))

            loss = tf.losses.sparse_softmax_cross_entropy(logits=logits, labels=self.y)#, weights=ws)
            self.reduced_loss = tf.reduce_mean(loss) + tf.add_n(tf.losses.get_regularization_losses())
            opt = tf.train.AdamOptimizer(learning_rate=learning_rate)
            self.train_op = opt.minimize(self.reduced_loss)
            self.saver = tf.train.Saver(var_list=tf.global_variables(), max_to_keep=10)
            self.sess = tf.InteractiveSession(graph=self.graph)
            self.sess.run(tf.global_variables_initializer())
            self.graph.finalize()

    def train(self, X_train, y_train, n_iters=25000, batch_size=64, print_every=500):
        trainset = DataGenerator(max_seq_len=self.seq_max_len, data=X_train, y=y_train)
        for i in range(n_iters):
            batch_x, batch_y, batch_seqlen = trainset.next(batch_size)
            _, cost, acc, acc_wout_last = self.sess.run([self.train_op, self.reduced_loss,
                                                         self.accuracy, self.accuracy_wout_last],
                                    feed_dict={self.x: batch_x, self.y: batch_y,
                                               self.seqlen: batch_seqlen,
                                               self.is_training_ph : True})
            if (i > 0) and (i % print_every == 0):
                print("Loss / acc. / acc. wout last after %d iter. : %.4f / %.4f / %.4f" % (i, cost, acc, acc_wout_last))

    def test(self, X_test):
        testset = DataGenerator(max_seq_len=self.seq_max_len, data=X_test, y=None)
        feed_dict = { self.x : testset.data, self.seqlen : testset.seqlen,
                      self.is_training_ph : False }
        return self.sess.run(self.probs, feed_dict=feed_dict)

    def save_model(self, path):
        save(self.saver, self.sess, path)

    def restore_model(self, path):
        load(self.saver, self.sess, path)
