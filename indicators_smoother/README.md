# Indicators Smoother Refactor

Authors: Dmitry Shemetov

## Problem Introduction

We need smoothing to reduce the noise in the data. Our existing smoothers had a few issues:

- they weren't unified in a utility
- they were computationally costly
- they produced jarring phenomena in sparse data (e.g. sawtooth patterns and jumping)

## Solution Outline

I implemented a smoother called the Savitzky-Golay filter, which generalizes the existing methods. It works by fitting a polynomial to a local window in the data and that fit is adjusted to weight more recent data more heavily. The implementation is also fast because the regression takes place on a same-sized, regularly-spaced grid.

## Results

We currently have a faster implementation of our main existing smoother. We are still exploring the optimal parameter settings for the smoother in different contexts.

- For a demonstration of the new smoother's behavior on sparse data, see [this notebook](COVIDcast%20Smoothing.ipynb).
- For mathematical descriptions of the existing smoothers, see [this notebook](smoothing_methods_math.md).
