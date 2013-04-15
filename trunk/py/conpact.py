# First attempt to model conceptual pacts via bayesian pragmatics

# We imagine a scenario in which a speaker describes an object using a
# sequence of adjectives. The literal speaker uses a variant of dm-wurwur,
# i.e.:
#  - picks an object in the world to describe, uniformly.
#  - decides how many adjectives to use. This is always observed, so we could
#    put a prior over it but there would be no point.
#  - samples that many times from a multinomial P(word|object). This
#    distribution is called the "lexicon", and is assumed to vary with the
#    context. For now we just put an *informative* dirichlet prior on it
#    (notionally representing previous experience with the language, which may
#    or may not be useful in this new context), and assume this prior is
#    observed.
# In addition, we assume that a history of previous (utterance, object) pairs
# are fully observed, and these condition the literal speaker/listener's
# actions (via updating the dirichlet-multinomial lexicon).
#
# Then on top of this, we have the standard probabilistic-game-theoretic
# recursion, with S0 defined as above, Ln doing rational bayesian inference on
# S(n-1), and Sn sampling utterances from a length-penalized softmax of
# L(n-1). (NB: this code uses the convention of disjoint indexes for speakers
# and listeners; odd indices are listeners and even indices are speakers.)

# Inference for now is by enumeration. We use some tricks.
#
# It's *substantially* more efficient to collapse together utterances
# that are equivalent modulo word order (since word order is not
# informative in our model). E.g. for utterances of length 10 on 10
# adjectives, we have:
#   possible utterances including order:   10 ** 10 == 10000000000
#   possible utterances ignoring order: 10 multichoose 10 == 92378
# So we represent utterances as bags-of-words, not lists-of-words.
#
# In general, for each distinct bag-of-words, we must also count how
# many corresponding distinct list-of-words map to it. If the entries
# in each bag were unique (i.e., it were a set), then this would just
# be the number of permutations on k, k!, where k = the number of
# entries. However, if there are duplicate entries in the bag, then
# this will overcount them. For example, here are some of the
# list-of-words that map to the bag {0, 1, 2}:
#   [0, 1, 2]
#   [1, 0, 2]
#   [2, 1, 0]
# But the only list that maps to the bag {0, 0, 0} is
#   [0, 0, 0]
# If item i has r_i duplicates in the multiset, then it produces
# overcounting by a factor of r_i!. So the total multiplicity of each
# multiset is
#   k! / product_i(r_i!)
#
# But, in fact, this doesn't even matter for our model!
#
# For S0 we compute P(bag_i | obj_j) for all i, j. But we don't use
# the raw numbers, because L1 immediately renormalizes to compute
# P(obj_j | bag_i). This means that all we actually care about are the
# ratios
#   P(bag_i | obj_j) / P(bag_i | obj_k)
# and any transformation which preserves these is fine.
#
# Ignoring the length prior is such a transformation, b/c
#   P(bag_i | obj_j) = P(bag_i | obj_j, len(bag_i)) P(len(bag_i))
# and notice that if we plug this into the above ratio, the
# P(len(bag_i)) terms cancel out. So in fact all we have to compute
# for S0 is
#   P(bag_i | obj_j, len(bag_i))
# and this way we don't have to think about length priors, yay.
#
# Ignoring the multiple relationships between utterance vectors and
# utterance bags is also ok, because:
#   P(bag_i | obj_j)
#     = sum_k P(bag_i | vec_k) P(vec_k | obj_j)
# But P(bag_i | vec_k) is either 0 or 1. And for all the items where
# it is 1, P(vec_k | obj_j) takes the same value. If n_i is the count
# of how many vectors map to bag_i (i.e, sum_k P(bag_i | vec_k)), then
# the above expression is just
#     = n_i P(vec_i | obj_j)
# (in an abuse of notation, we write vec_i to indicate an arbitrary
# representative vector which corresponds to bag_i). And this means
# that
#   P(bag_i | obj_j) / P(bag_i | obj_k)
#     = (n_i P(vec_i | obj_j)) / (n_i P(vec_i | obj_k))
#     = P(vec_i | obj_j) / P(vec_i | obj_k)
#
# So what we actually compute for S0 is the matrix of values
#   P(vec_i | obj_j, len(vec_i))

import sys
from itertools import combinations_with_replacement
import numpy as np
from scipy.special import gammaln
import pandas

################################################################
# The model
################################################################

class Model(object):
    def __init__(self,
                 # Memory usage hint: our probability distribution matrices
                 # will contain this many entries:
                 #    multichoose((adjectives + 1),
                 #                max_utterance_length)
                 #    * objects
                 adjectives=8, objects=10, max_utterance_length=6,
                 # Softmax parameter for the pragmatic speaker:
                 softmax_weight=1,
                 # Word cost for the pragmatic speaker (measured in
                 # nats-per-word):
                 word_cost=1):
        self.adjectives = adjectives
        self.objects = objects
        self.max_utterance_length = max_utterance_length
        self.softmax_weight = softmax_weight
        self.word_cost = word_cost

        # We allow utterances of any length up to max_utterance_length. How
        # many are there? Each arbitrary-length utterance can be thought of as
        # a utterance with exactly max_utterance_length words, where we add an
        # additional "null" word. Except that we disallow the null utterance
        # by fiat.
        total_utts = -1 + multichoose(self.adjectives + 1,
                                      self.max_utterance_length)
        # Each row of this matrix represents one utterance as a
        # bag-of-words. The bags are represented in count form, e.g. if we
        # have 5 possible adjectives, then the utterance {0, 0, 3} is stored
        # as:
        #   [2, 0, 0, 1, 0]
        assert self.max_utterance_length <= 255
        utts = np.zeros((total_utts, self.adjectives), dtype=np.uint8)
        words_with_null = [None] + range(self.adjectives)
        utt_iter = combinations_with_replacement(words_with_null,
                                                 self.max_utterance_length)
        # skip the null utterance:
        skipped = utt_iter.next()
        assert skipped == (None,) * self.max_utterance_length
        # and for all the rest, fill in the corresponding bag:
        for i, utt in enumerate(utt_iter):
            for word in utt:
                if word is not None:
                    utts[i, word] += 1
        assert i + 1 == utts.shape[0]

        self.utts = utts
        self.utt_lengths = np.sum(utts, axis=1)

        # Compute multiplicities of each utterance. This is only for the use
        # of .real_S0(); it's not needed at all for the regular model.
        vec_fact = np.vectorize(factorial)
        permutations = vec_fact(self.utt_lengths)
        equiv_permutations = np.prod(vec_fact(self.utts), axis=1)
        assert np.all(permutations % equiv_permutations) == 0
        self.multiplicities = permutations // equiv_permutations

    def flat_lexicon_prior(self, alpha):
        return alpha * np.ones((self.adjectives, self.objects))

    def lexicon(self, lexicon_prior, history):
        # Add pseudocounts to the lexicon based on discourse history
        lexicon = np.copy(lexicon_prior)
        for utt_idx, obj_idx in history:
            lexicon[:, obj_idx] += self.utts[utt_idx]
        return lexicon

    def S0(self, lexicon_prior, history):
        # Compute the DM lexicon, conditioning on prior and history
        lexicon = self.lexicon(lexicon_prior, history)
        # For each possible object, compute the probability that it
        # generates each possible utterance (ignoring word order),
        # conditioning on:
        #   the utterance length
        #   the lexicon
        #   the space of possible objects and adjectives
        # This is the same as the probability that we sample the given
        # utterance from a dirichlet-multinomial distribution with parameters
        # given in 'lexicon'.
        logP_utt_given_objects = np.empty((self.utts.shape[0], self.objects))
        for obj_i in xrange(self.objects):
            # shorthand view onto the relevant part of the big matrix
            logP_utt = logP_utt_given_objects[:, obj_i]
            # The dirichlet parameter vector
            alpha = lexicon[:, obj_i]
            # https://en.wikipedia.org/wiki/Dirichlet-multinomial_distribution#Joint_distribution
            # Joint density for N samples from a dirichlet-multinomial
            # distribution with parameters alpha[i], and n_k
            # repeats of each outcome is
            #   Z * prod_k Gamma(n_k + alpha[k])
            # where:
            #   Z = Gamma(A) / Gamma(N + A) * prod_k 1/Gamma(alpha[k])
            #   A = sum_k(alpha[k])
            #   N = sum_k(n_k)
            # Notice the n_k values are exactly the entries in the count-form
            # bag-of-words we have stored in self.utts, and N is the length of
            # the utterance.
            #
            # This is slightly clever -- calling gammaln separately
            # for every utterance would be a waste of time, because there are
            # only a few different values that N + A can take on. So instead
            # we compute all those values once, and then just index this array
            # by the actual N:
            A = np.sum(alpha)
            possible_N = np.arange(self.max_utterance_length + 1)
            possible_logZ = (gammaln(A)
                             - gammaln(possible_N + A)
                             - np.sum(gammaln(alpha)))
            logP_utt[:] = possible_logZ[self.utt_lengths]
            logP_utt[:] += np.sum(gammaln(self.utts + alpha), axis=1)

        return logP_utt_given_objects

    def normalize_S0(self, S0, length_prior):
        """Compute the actual S0 distribution over utterances; useful for
        simulating experiments with pragmatics "turned off". See the comment
        at the top for why this is different -- here we compute:
          P(utt bag | object)
        while S0() computes
          P(utt vector | object, length)

        'length_prior' should be a vector with max_utterance_length entries,
        which just gives the prior probability of an utterance with each given
        length.
        """
        length_prior = np.asarray(length_prior)
        assert np.allclose(np.sum(length_prior), 1)
        assert length_prior.shape == (self.max_utterance_length,)
        logP = np.copy(S0)
        logP += np.log(self.multiplicities)[:, np.newaxis]
        logP += np.log(length_prior)[self.utt_lengths - 1][:, np.newaxis]
        assert np.allclose(np.logaddexp.reduce(logP, axis=0), 0)
        return logP

    def L(self, logP_S):
        # logP_S is P(utt|obj, ...) as a (word vector, object) matrix
        # to convert to P(obj|utt, ...), we normalize each column.
        # XX NB: this assumes a uniform prior over words
        logZ = np.logaddexp.reduce(logP_S, axis=1)
        return logP_S - logZ[:, np.newaxis]

    def S(self, logP_L):
        # logP_L is P(object|utt, ...) as a (word vector, object) matrix
        # For the speaker:
        #   P(utt|object) = 1/Z * exp(SOFTMAX_WEIGHT * U)
        # where
        #   U = log P_L(object | utt) - cost(utt)
        U = logP_L - (self.word_cost * self.utt_lengths)[:, np.newaxis]
        U *= self.softmax_weight
        logZ = np.logaddexp.reduce(U, axis=0)
        U -= logZ[np.newaxis, :]
        return U

    # Internal utility method: compute the given Sn or Ln distribution, and
    # save requested intermediate results. (If you want, say, S4 and L3, then
    # computing them in one pass is much faster than computing them
    # separately.)
    def _dist_at_depth(self, depth, lexicon_prior, history,
                       depths_to_save):
        assert depth >= 0
        if depth == 0:
            dist = self.S0(lexicon_prior, history)
            saved = []
        else:
            dist, saved = self._dist_at_depth(depth - 1,
                                              lexicon_prior, history,
                                              depths_to_save)
            if depth % 2 == 1:
                dist = self.L(dist)
            else:
                assert depth % 2 == 0
                dist = self.S(dist)
        if depth in depths_to_save:
            saved.append(dist)
        return dist, saved

    def Ln(self, n, lexicon_prior, history):
        "Compute an arbitrary listener distribution"
        assert n % 2 == 1
        return self._dist_at_depth(n, lexicon_prior, history, [])[0]

    def Sn(self, n, lexicon_prior, history):
        "Compute an arbitrary speaker distribution"
        assert n % 2 == 0
        return self._dist_at_depth(n, lexicon_prior, history, [])[0]

    def dists_at_depths(self, depths, lexicon_prior, history):
        "Compute an arbitrary set of listener and/or speaker distributions."
        max_depth = max(depths)
        # _dist_at_depth always returns dists sorted by depth, so e.g. L1
        # comes before S2, even if depths_to_save=[2, 1]. And duplicates are
        # only saved once. So we need to mess around a bit to return a list
        # which is parallel to our 'depths' argument.
        saved_depths = sorted(set(depths))
        depth_to_position = {}
        for i, depth in enumerate(saved_depths):
            depth_to_position[depth] = i
        saved_dists = self._dist_at_depth(max_depth, lexicon_prior, history,
                                          saved_depths)[1]
        return [saved_dists[depth_to_position[d]] for d in depths]

def test_Model_basics():
    m = Model(adjectives=2, objects=4, max_utterance_length=3)
    assert m.adjectives == 2
    assert m.objects == 4
    assert m.max_utterance_length == 3
    assert m.utts.shape == (9, 2)
    assert np.array_equal(m.utts,
                          [[1, 0],
                           [0, 1],
                           [2, 0],
                           [1, 1],
                           [0, 2],
                           [3, 0],
                           [2, 1],
                           [1, 2],
                           [0, 3]])
    assert np.array_equal(m.utt_lengths, [1, 1, 2, 2, 2, 3, 3, 3, 3])
    assert np.array_equal(m.multiplicities, [1, 1, 1, 2, 1, 1, 3, 3, 1])

    prior = m.flat_lexicon_prior(7)
    assert np.array_equal(prior,
                          [[7, 7, 7, 7],
                           [7, 7, 7, 7]])

    # Two utterances:
    #   utt 0 = bag [1, 0] -> object 0
    #   utt 6 = bag [2, 1] -> object 2
    assert np.array_equal(m.lexicon(prior, [(0, 0), (6, 2)]),
                          [[8, 7, 9, 7],
                           [7, 7, 8, 7]])

def test_Model_S0():
    m = Model(adjectives=2, objects=2, max_utterance_length=2)

    prior = np.asarray([[0, 2],
                        [3, 4]])
    history = [(0, 0)]
    lexicon = m.lexicon(prior, history)
    assert np.array_equal(lexicon, [[1, 2], [3, 4]])
    S0 = m.S0(prior, history)
    S0_raw = np.exp(S0)
    # 5 possible utterances
    A0 = np.sum(lexicon[:, 0])
    A1 = np.sum(lexicon[:, 1])
    from scipy.special import gamma
    prodalpha0 = np.prod(gamma(lexicon[:, 0]))
    prodalpha1 = np.prod(gamma(lexicon[:, 1]))
    expected_raw = np.array([
        # utterance: <0>
        [gamma(A0) / gamma(A0 + 1)
           * gamma(1 + lexicon[0, 0])
           * gamma(0 + lexicon[1, 0])
           / prodalpha0,
         gamma(A1) / gamma(A1 + 1)
           * gamma(1 + lexicon[0, 1])
           * gamma(0 + lexicon[1, 1])
           / prodalpha1],
        # utterance: <1>
        [gamma(A0) / gamma(A0 + 1)
           * gamma(0 + lexicon[0, 0])
           * gamma(1 + lexicon[1, 0])
           / prodalpha0,
         gamma(A1) / gamma(A1 + 1)
           * gamma(0 + lexicon[0, 1])
           * gamma(1 + lexicon[1, 1])
           / prodalpha1],
        # utterance: <0 0>
        [gamma(A0) / gamma(A0 + 2)
           * gamma(2 + lexicon[0, 0])
           * gamma(0 + lexicon[1, 0])
           / prodalpha0,
         gamma(A1) / gamma(A1 + 2)
           * gamma(2 + lexicon[0, 1])
           * gamma(0 + lexicon[1, 1])
           / prodalpha1],
        # utterance: <0 1>
        [gamma(A0) / gamma(A0 + 2)
           * gamma(1 + lexicon[0, 0])
           * gamma(1 + lexicon[1, 0])
           / prodalpha0,
         gamma(A1) / gamma(A1 + 2)
           * gamma(1 + lexicon[0, 1])
           * gamma(1 + lexicon[1, 1])
           / prodalpha1],
        # utterance: <1 1>
        [gamma(A0) / gamma(A0 + 2)
           * gamma(0 + lexicon[0, 0])
           * gamma(2 + lexicon[1, 0])
           / prodalpha0,
         gamma(A1) / gamma(A1 + 2)
           * gamma(0 + lexicon[0, 1])
           * gamma(2 + lexicon[1, 1])
           / prodalpha1],
        ])
    assert np.allclose(S0_raw, expected_raw)

    # Distributions should be normalized (given length and object and the fact
    # that our <0 1> bag needs to be double-counted, because it also
    # represents the utterance <1 0>)
    assert np.all(m.multiplicities[:2] == 1)
    assert np.allclose(S0_raw[0, 0] + S0_raw[1, 0], 1)
    assert np.allclose(S0_raw[0, 1] + S0_raw[1, 1], 1)
    assert np.array_equal(m.multiplicities[2:], [1, 2, 1])
    assert np.allclose(S0_raw[2, 0] + 2 * S0_raw[3, 0] + S0_raw[4, 0], 1)
    assert np.allclose(S0_raw[2, 1] + 2 * S0_raw[3, 1] + S0_raw[4, 1], 1)

    real_S0_raw = np.exp(m.normalize_S0(S0, [0.7, 0.3]))
    assert np.allclose(np.sum(real_S0_raw, axis=0), 1)
    assert np.allclose(real_S0_raw[:, 0] / real_S0_raw[:, 1],
                       S0_raw[:, 0] / S0_raw[:, 1])

def test_Model_L():
    m = Model(adjectives=2, objects=2, max_utterance_length=2)
    before_L = np.array([[0.4, 0.2],
                         [0.6, 0.8],
                         [0.1, 0.7],
                         [0.4, 0.1],
                         [0.5, 0.2]])
    after_L_expected = np.array([[0.4 / 0.6, 0.2 / 0.6],
                                 [0.6/ 1.4, 0.8 / 1.4],
                                 [0.1 / 0.8, 0.7 / 0.8],
                                 [0.4 / 0.5, 0.1 / 0.5],
                                 [0.5 / 0.7, 0.2 / 0.7]])
    assert np.allclose(np.sum(after_L_expected, axis=1), 1)
    assert np.allclose(np.exp(m.L(np.log(before_L))), after_L_expected)

def test_Model_S():
    m = Model(adjectives=2, objects=2, max_utterance_length=2,
              softmax_weight=3, word_cost=2)
    P_L = np.array([[0.1, 0.9],  # length 1
                    [0.2, 0.8],  # length 1
                    [0.3, 0.7],  # length 2
                    [0.4, 0.6],  # length 2
                    [0.5, 0.5]]) # length 2
    P_S_unnorm = np.array([[0.1 ** 3 / np.exp(3 * 2 * 1),
                            0.9 ** 3 / np.exp(3 * 2 * 1)],
                           [0.2 ** 3 / np.exp(3 * 2 * 1),
                            0.8 ** 3 / np.exp(3 * 2 * 1)],
                           [0.3 ** 3 / np.exp(3 * 2 * 2),
                            0.7 ** 3 / np.exp(3 * 2 * 2)],
                           [0.4 ** 3 / np.exp(3 * 2 * 2),
                            0.6 ** 3 / np.exp(3 * 2 * 2)],
                           [0.5 ** 3 / np.exp(3 * 2 * 2),
                            0.5 ** 3 / np.exp(3 * 2 * 2)],
                           ])
    P_S = P_S_unnorm / np.sum(P_S_unnorm, axis=0)
    assert np.allclose(np.sum(P_S[:, 0]), 1)
    assert np.allclose(np.sum(P_S[:, 1]), 1)

    assert np.allclose(np.exp(m.S(np.log(P_L))), P_S)

def test_Model_iterates():
    m = Model(adjectives=2, objects=3, max_utterance_length=4,
              softmax_weight=3, word_cost=2)
    r = np.random.RandomState(0)
    lexicon_prior = r.rand(2, 3)
    history = [(2, 2), (6, 1), (0, 1)]
    S0 = m.S0(lexicon_prior, history)
    L1 = m.L(S0)
    S2 = m.S(L1)
    L3 = m.L(S2)
    S4 = m.S(L3)
    assert np.allclose(m.Ln(1, lexicon_prior, history), L1)
    assert np.allclose(m.Ln(3, lexicon_prior, history), L3)
    assert np.allclose(m.Sn(0, lexicon_prior, history), S0)
    assert np.allclose(m.Sn(2, lexicon_prior, history), S2)
    assert np.allclose(m.Sn(4, lexicon_prior, history), S4)

    dists = m.dists_at_depths([2, 4, 3, 0, 3], lexicon_prior, history)
    for dist_got, dist_expected in zip(dists, [S2, S4, L3, S0, L3]):
        assert np.allclose(dist_got, dist_expected)

def sample_objects(seed, model, count):
    return np.random.RandomState(seed).randint(model.objects, size=(count,))

def test_sample_objects():
    m = Model(objects=4)
    objs = sample_objects(0, m, 1000)
    assert objs.shape == (1000,)
    assert set(objs) == set([0, 1, 2, 3])

def sample_naming_task(seed, model, lexicon_prior, objects, s_depth,
                       S0_length_prior=None, trace_funcs=[]):
    """Sample one path through a simple naming task, in which S has to name
    each object in 'objects' in sequence."""
    assert s_depth % 2 == 0
    r = np.random.RandomState(seed)
    history = []
    trace_values = [[] for f in trace_funcs]
    for obj in objects:
        if len(history) % 10 == 0:
            sys.stdout.write(".")
        # Arbitrary, somewhat quirky convention: when sampling utterances from
        # S(n), we provide trace functions with all distributions out to
        # L(n+1).
        dists = model.dists_at_depths(range(s_depth + 2),
                                      lexicon_prior, history)
        if S0_length_prior is not None:
            dists[0] = model.normalize_S0(dists[0], S0_length_prior)
        # Must extract *after* the call to normalize_S0, because if s_depth=0
        # then we need S0 normalized in order to sample from it.
        speaker_dist = dists[-2]
        utt_dist_raw = np.exp(speaker_dist[:, obj])
        utt = r.multinomial(1, utt_dist_raw).nonzero()[0].item()
        for trace_func, trace_value in zip(trace_funcs, trace_values):
            trace_value.append(trace_func(model, lexicon_prior, history,
                                          utt, obj, dists))
        history.append((utt, obj))
    return (history,) + tuple(trace_values)

def trace_P_correct(model, lexicon_prior, history, utt, obj, dists):
    # For a naming task performed by Sn, what is the probability that
    # L<n+1> (i.e., the optimal listener for Sn) will understand?
    # P(L_obj = obj)
    #   = sum_S_utt P(L_obj = obj | S_utt) P(S_utt | obj = obj)
    # (This assumes a uniform distribution over objects)
    S_dist, L_dist = dists[-2:]
    P_L_correct = np.mean(np.sum(np.exp(L_dist + S_dist), axis=0))
    return P_L_correct

def trace_utt_len_dist(model, lexicon_prior, history, utt, obj, dists):
    S_dist = dists[-2]
    # This is like an R tapply(); we compute the total probability mass
    # assigned to utterances of each length, by the speaker
    #   P(utt) = P(utt | obj) * P(obj)
    # Assuming uniform dist over objects,
    #     = mean_obj P(utt | obj)
    S_dist_raw = np.exp(S_dist)
    # Marginalize over objects
    S_utt_dist = np.mean(S_dist_raw, axis=1)
    # Sum the probability mass over all same-length utterances
    S_length_dist = (pandas.Series(S_utt_dist)
                       .groupby(model.utt_lengths)
                         .aggregate(np.sum))
    return S_length_dist

def trace_utt_exp_len(model, lexicon_prior, history, utt, obj, dists):
    S_length_dist = trace_utt_len_dist(model, lexicon_prior, history,
                                       utt, obj, dists)
    return np.sum(S_length_dist.index * S_length_dist)

################################################################
# Some basic combinatoric functions
################################################################

# https://en.wikipedia.org/wiki/Falling_factorial_power
# n * (n - 1) * ... * (n - (k - 1))
def falling_factorial(n, k):
    assert 0 <= k <= n
    total = 1
    for i in xrange(k):
        total *= (n - i)
    return total

def test_falling_factorial():
    assert falling_factorial(4, 0) == 1
    assert falling_factorial(4, 1) == 4
    assert falling_factorial(4, 2) == (4 * 3)
    assert falling_factorial(4, 3) == (4 * 3 * 2)
    assert falling_factorial(4, 4) == (4 * 3 * 2 * 1)

def factorial(n):
    return falling_factorial(n, n)

def test_factorial():
    assert factorial(0) == 1
    assert factorial(1) == 1
    assert factorial(2) == 2
    assert factorial(3) == 6
    assert factorial(4) == 24

def choose(n, k):
    return falling_factorial(n, k) // factorial(k)

def test_choose():
    assert choose(0, 0) == 1
    for n in xrange(5):
        for k in xrange(n):
            # the Pascal's triangle recurrence
            assert choose(n + 1, k + 1) == choose(n, k) + choose(n, k + 1)

def multichoose(n, k):
    return choose(n + k - 1, k)

def test_multichoose():
    for n in xrange(4):
        for k in xrange(2 * n):
            multisets = list(combinations_with_replacement(xrange(n), k))
            assert len(multisets) == multichoose(n, k)

################################################################
# If this file is run as a script, run tests
################################################################

if __name__ == "__main__":
    import nose
    nose.runmodule()
