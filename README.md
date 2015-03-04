# Debtstack
Finance visualization tool


## Overview

Debtstack is my attempt to provide interesting answers to the question of "What am I currently paying off on my credit card?". It started life as a series of spreadsheets and I decided to further automate it and use it as an excuse to do some F# programming.


## Concept

The typical way to look at a credit card account is to see it where you are paying off purchases in the order that they are made: first in, first out. In data structures terms we refer to this as a queue. You can just take your last few statements and start adding up transactions until you meet your current balance and you've got a queue view into what you are currently paying off. To me, however, it is not an "interesting" view. The view tends to obscure the real reasons you may be in debt and tends to give focus more to recent trivial purchases.

The related data structure to the queue is referred to as a stack. In a stack items are dealt with first in, last out. It seemed clear to me, given the time value of money, that the stack was a more interesting way to look at an account. After having captured this sort of few for a couple years now in spreadsheet form, I'm pretty confident that it does provide the basis for a much more interesting view of one's debt. It gives more "weight" to big purchases and, in my opinion, a better idea of how "behind" you are in paying things off.


## More Documentation

The following blog posts:

* [Debtstack 2.0](http://blog.worldmaker.net/2014/may/20/debtstack-20/)
* [Introducing Debtstack](http://blog.worldmaker.net/2012/apr/29/introducing-debtstack/)
