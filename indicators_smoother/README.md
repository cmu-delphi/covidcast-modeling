# Indicators Smoother Refactor

Authors: Dmitry Shemetov

## Problem Introduction

We need smoothing to reduce the noise in the data. Our existing smoothers had a few issues:

- they weren't unified in a utility
- they were computationally costly
- they produced jarring phenomena in sparse data (e.g. sawtooth patterns and jumping)
- in some cases, they were not filters that were only based on past data

## Solution Outline

I implemented a smoother called the Savitzky-Golay filter, which generalizes the existing methods in a utility. It works by fitting a polynomial to a local window in the data weighing more recent data more heavily. This generalization is fast because the regression takes place on a fixed-length, regularly-spaced grid and gives more flexibility to the smoothing.

## Results

We currently have a faster implementation of our main existing smoother. We are still exploring the optimal parameter settings for the smoother in different contexts. Initial results suggests that using the Savitzky-Golay filter with polynomial degree 0 yields a noise-resistant smoother in the sparse regime.

- For a demonstration of the new smoother's behavior on sparse data, see [this notebook](COVIDcast%20Smoothing.ipynb).
- For mathematical descriptions of the existing smoothers, see [this notebook](smoothing_methods_math.ipynb).
