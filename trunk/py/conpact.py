# First attempt to model conceptual pacts via bayesian pragmatics

import numpy as np
from scipy.special import betaln

# Constants used in word vectors
WORD_TRUE = 1
WORD_FALSE = -1
WORD_UNSEEN = 0
WORD_CONSTANTS = np.asarray([WORD_TRUE, WORD_FALSE, WORD_UNSEEN])

class Params(object):
    def __init__(self, adjectives=8, objs=10,
                 # Pseudo-observations in l0
                 pseudo_weight=1,
                 prior_prob_range=(0, 1),
                 softmax_weight=1,
                 # measured in nats-per-word
                 word_cost=1):
        self.adjectives = adjectives
        self.objs = objs
        self.pseudo_weight = pseudo_weight
        self.prior_prob_range = prior_prob_range
        self.softmax_weight = softmax_weight
        self.word_cost = word_cost

        shape = (WORD_CONSTANTS.shape[0] ** self.adjectives, self.adjectives)
        self.all_words_vectors = np.empty(shape, dtype=int)
        for i, index in enumerate(np.ndindex((3,) * self.adjectives)):
            self.all_words_vectors[i, :] = WORD_CONSTANTS[np.asarray(index)]
        self.all_words_trues = (self.all_words_vectors == WORD_TRUE)
        self.all_words_falses = (self.all_words_vectors == WORD_FALSE)
        self.all_words_unseen = (self.all_words_vectors == WORD_UNSEEN)

        self.all_words_lengths = np.sum(~self.all_words_unseen, axis=1)
        self.all_words_costs = (self.word_cost * self.all_words_lengths)

    def sample_l0(self, r):
        # We allocate self.pseudo_weight pseudocount mass uniformly at random
        # between true and false pseudo-events.
        prob = r.uniform(self.prior_prob_range[0],
                         self.prior_prob_range[1],
                         size=(self.objs, self.adjectives))
        alpha = prob * self.pseudo_weight
        beta = (1 - prob) * self.pseudo_weight
        return (alpha, beta)

def S0(params, l0, history):
    # Get the current estimated lexicon
    l0_alpha, l0_beta = l0
    lic_alpha = l0_alpha.copy()
    lic_beta = l0_beta.copy()
    # For each observed word in the history, increment the appropriate
    # pseudocount:
    for words, obj in history:
        lic_alpha[obj, words == WORD_FALSE] += 1
        lic_beta[obj, words == WORD_TRUE] += 1
    # Now for each possible object, compute the probability that it generates
    # each possible words vector, given the appropriate (varying!) mask, plus
    # l0, history, and objects space:
    logP_words_given_objs_masks = np.zeros((params.all_words_vectors.shape[0],
                                            params.objs))
    for obj_i in xrange(params.objs):
        # get a view onto the relevant column with a short name
        col = logP_words_given_objs_masks[:, obj_i]
        for word_i in xrange(params.adjectives):
            # if word true, density is B(alpha, 1 + beta)/B(alpha, beta)
            # if word false, density is B(1 + alpha, beta)/B(alpha, beta)
            # else, density is 1
            alpha = lic_alpha[obj_i, word_i]
            beta = lic_beta[obj_i, word_i]
            denom = betaln(alpha, beta)
            col[params.all_words_trues[:, word_i]] += (betaln(alpha, beta + 1)
                                                       - denom)
            col[params.all_words_falses[:, word_i]] += (betaln(alpha + 1, beta)
                                                        - denom)
    return logP_words_given_objs_masks

def L(params, logP_S):
    # logP_S is P(words|obj, ...) as a (word vector, object) matrix
    # to convert to P(obj|words, ...), we normalize each column.
    # XX NB: this assumes a uniform prior over words
    logZ = np.logaddexp.reduce(logP_S, axis=1)
    return logP_S - logZ[:, np.newaxis]

def S(params, logP_L):
    # logP_L is P(obj|words, ...) as a (word vecotr, object) matrix
    # For the speaker:
    #   P(words|object) = 1/Z * exp(SOFTMAX_WEIGHT * U)
    #     U = log P_L0(object | words) - cost(words)
    U = logP_L - params.all_words_costs[:, np.newaxis]
    U *= params.softmax_weight
    logZ = np.logaddexp.reduce(U, axis=0)
    U -= logZ[np.newaxis, :]
    return U

def run(params, seed, iters, s_depth=4, l_depth=3):
    r = np.random.RandomState(seed)
    l0 = params.sample_l0(r)
    S_exp_lengths = []
    #S4_exp_lengths = []
    P_L_corrects = []
    history = []
    assert s_depth % 2 == 0
    assert l_depth % 2 == 1
    for i in xrange(iters):
        dists = []
        for i in xrange(max(s_depth, l_depth) + 1):
            if i == 0:
                dist = S0(params, l0, history)
            elif i % 2 == 0:
                dist = S(params, dists[-1])
            else:
                dist = L(params, dists[-1])
            dists.append(dist)
        S_words_dists = dists[s_depth]
        S_words_dist_raw = np.exp(S_words_dists)
        # assumes uniform dist over objs
        S_exp_length = np.mean(np.sum(params.all_words_lengths[:, np.newaxis]
                                      * S_words_dist_raw, axis=0))
        S_exp_lengths.append(S_exp_length)
        # P(L3_obj = obj) = Sum_S2_word P(L3_obj = obj | S2_word) P(S2_word | obj)
        L_objs_dists = dists[l_depth]
        # assumes uniform dist over objs
        P_L_correct = np.mean(np.sum(np.exp(L_objs_dists + S_words_dists), axis=0))
        P_L_corrects.append(P_L_correct)
        obj = r.randint(params.objs)
        words_i = r.multinomial(1, S_words_dist_raw[:, obj]).nonzero()[0].item()
        words = params.all_words_vectors[words_i, :]
        history.append((words, obj))
    return (S_exp_lengths, P_L_corrects, history)
