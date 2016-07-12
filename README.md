# Next-Word Predictor Development via Amazon ElasticMapReduce (EMR)

The [notebook](https://github.com/bricof/word_predictor/blob/master/word_predictor_EMR.ipynb) takes as input a pointer to a S3 bucket containing a set of text files, and outputs a set of JSON files, each of which contains a set of prior strings (e.g. 1, 2, 3 or 4 prior words separated by spaces) as keys, pointing to arrays of the most common occurances for the next word in the sequence. The JSON files can be used in next-word prediction applications, as demonstrated in [this blog post](http://briancoffey.ca/blogpost6.html).

