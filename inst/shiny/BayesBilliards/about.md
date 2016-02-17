Author: Jason Bryer, Ph.D. (jason@bryer.org)  
Website: [bryer.org](http://jason.bryer.org)

### Problem Statement

Consider a pool table with length one. An 8-ball is thrown such that the likelihood of its stopping point is uniform across the entire table. The location of the 8-ball is recorded, but not known to the observer. Subsequent balls are thrown and all that is reported is whether the ball stopped to the left or right of the 8-ball. Given only this information, what is the position of the 8-ball?

### Strategy

For the initial iteration, we have one observation of left or right. We assume a uniform *prior* distribution of length *k*. For each *k*, we sample `unif(0,1)` and record whether that value is less than (i.e. left) or greater than (i.e. right) of *k*. The values from *k* that match the original observation remain and constitutes our *posterior* distribution.

For subsequent iterations, we simply use our *posterior* distribution from the prior iteration as our *prior* distribution for the current sampling.

### Usage

* **Confidence Range** - The width of the confidence interval around the mean.
* **Number of Samples** - The length of the prior distributions.
* **Show Pool Table** - Show a figure with the 8-ball along with the location of subsequent ball throws.
* **Distribution Plot Type** - For the proir and posterior distribution plots use density or histogram plot types.
* **Next Ball** - Throw another ball and record whether it fell to the left or right of the 8-ball.
* **Start Over** - Throw a new 8-ball and start the process over.

### References

Downey, A. (2015). Bayesian Billiards. Retrieved from http://allendowney.blogspot.com/2015/06/bayesian-billiards.html

Eddy, S.R. (2004). What is Bayesian statistics? *Nature Biotechnology 22*. Retrieved from http://www.nature.com/nbt/journal/v22/n9/full/nbt0904-1177.html
