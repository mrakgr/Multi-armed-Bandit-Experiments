# Multi-armed Bandit Experiments

The V4 of Spiral is quite usable and the RNNs are there waiting to be used. But in fact, any sort of poker is in fact too complex to start off with. Logistic issues of feeding it data aside, just how should I even train them? In both poker and in the bandit problem, the sequences are actually quite short and the rewards are immediate. I do not really need LSTMs, recurrent highway nets, HORNNs, multiplicative integration or layer norm here.

Here I need to do something really basic - prove that recurrent neural nets are capable of metalearning. At the very least, I am thinking that it should be able to take advantage of one very simple pattern - a bandit problem with zero variance, but with shifting distributions after a certain number of iterations. In other words it needs to be capable of quick contextual shifts. I'll add sequence patterns after that.

If I could managed something as trivial seeming as that, then I do not think poker will be a huge challenge after that.

In terms of programming, how to ideally integrate these nets with the rest of the program is far from a solved issue as well. I also need to find a good architecture for reinforcement learning (though I have an idea regarding that) and figure out the form inputs and outputs should take.