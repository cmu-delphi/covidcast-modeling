# Smoothing Methods Documentation

This file is meant to document the different smoothers we have available. We provide a high-level summary and a mathematical description of each method.

- [Smoothing Methods Documentation](#smoothing-methods-documentation)
  - [Gaussian-weighted local linear regression](#gaussian-weighted-local-linear-regression)
    - [Summary](#summary)
    - [Mathematical Details](#mathematical-details)
    - [Implementation](#implementation)
  - [Savitzky-Golay filter](#savitzky-golay-filter)
    - [Summary](#summary-1)
    - [Mathematical Details](#mathematical-details-1)
    - [Implementation](#implementation-1)
  - [Moving window average](#moving-window-average)

## Gaussian-weighted local linear regression

### Summary

From [the existing documentation](https://github.com/cmu-delphi/delphi-epidata/blob/main/docs/api/covidcast-signals/ght.md#smoothing):
> For each date, we fit a local linear regression, using a Gaussian kernel, with only data on or before that date. (This is equivalent to using a negative half normal distribution as the kernel.) The bandwidth is chosen such that most of the kernel weight is placed on the preceding seven days. The estimate for the data is the local linear regression's prediction for that date. It is implemented in `left_gauss_linear`.

### Mathematical Details

Suppose $y_1, y_2, \dots, y_n$ is a time series we want to smooth, $\hat y_n$ is the smoothed value at time $n$, and $\mathbf y_k$ denotes a vector of the values from 1 to $k$, both ends inclusive. Suppose that the independent variables are simply the time index $x_i = i$. We model $y$ as follows
$$\mathbf y_k = \mathbf X_k \mathbf \beta_k + \mathbf \epsilon_k,$$
where $\mathbf \beta_k \in \mathbb R^k$ is a vector of coefficients, $\mathbf \epsilon \in \mathbb R^k$ is a vector of noise, and
$$\mathbf X_k = \begin{bmatrix} 1 & 1 \\
1 & 2 \\
\vdots & \vdots \\
1 & k\end{bmatrix}.$$
The minimization problem solved is
$$\hat \beta = \arg \min_{\mathbf \beta} \|\mathbf y - \mathbf X \mathbf \beta\|_2^2,$$
which has classical OLS solution
$$\mathbf {\hat \beta} = (\mathbf X^T \mathbf X)^{-1} (\mathbf X^T \mathbf y).$$
The modified OLS solution for a local regression with weight matrix $\mathbf W$ is
$$\mathbf {\hat \beta} = (\mathbf X^T \mathbf W \mathbf X)^{-1} (\mathbf X^T \mathbf W \mathbf y),$$
where we use the negative half-normal kernel for the weight matrix $\mathbf W$, which is a diagonal matrix with entries given by $\mathbf W_{ii} = K_\sigma(k-i)$. Here we have
$$K_\sigma(x) = \frac{\sqrt 2}{\sigma \sqrt{\pi}} \exp \left(- \frac{x^2}{2 \sigma^2} \right).$$
Interpreting "most kernel weight placed on preceding seven days" to mean that 95% of the weight is on those days, we solve $\int_0^7 K_\sigma(x) dx = .95$ for $\sigma$ and obtain $\sigma \approx 3.5$.

To obtain the smoothed $\hat y_n$, this method forecasts with the regression from the previous $n-1$ data points, i.e.
$$\hat y_n = \langle \mathbf {\hat \beta_{n-1}}, (1, n) \rangle.$$

One issue with our current implementation of the method is that it requires a large, weighted OLS solve for every data point.

### Implementation

The implementation is essentially identical to the mathematics.

The boundaries are handled by using the raw data (i.e. for the first 2 data points).

The largest cost in this computation is the matrix multiplication and inversion in $(\mathbf X^T \mathbf W \mathbf X)^{-1}$. Since the window size is constantly changing to be the whole past, this has to be recomputed for every point. One of the ways to improve this is to fix a finite window for the fit and precompute this large calculation (because of the Gaussian weights, the data points beyond a certain window length have negligeble effects on the fit anyway). This is done in `causal_savgol`.

## Savitzky-Golay filter

### Summary

The local linear regression above is one type of [kernel smoother](https://en.wikipedia.org/wiki/Kernel_smoother). Many kernel smoothers can be thought of as a local function fitting. For instance, a window average corresponds to a constant line fit, local regression corresponds to a linear function fit, and the Savitsky-Golay family contains the rest of the polynomial fits.

The [Savitzky-Golay method](https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter) is a smoothing method popularized in analytical chemistry because it preserves the heights and widths of the data. The method works by fitting a polynomial of specified degree to a window in the data. The Savitzky-Golay method is thus a generalization of local linear regression. Because the method fits a local polynomial, we say that the method preserves local moments (up to the degree of the polynomial fit). We implement this method in `causal_savgol`.

### Mathematical Details

(We follow the derivation in *Numerical Recipes* by Press et. al., pg. 768.) 

Suppose that we want to fit the polynomial $a_0 + a_1 i + \dots + a_M i^M$ to the values $(f_{-n_L},\dots, f_{n_R})$. The design matrix for the fitting problem is
$$A_{ij} = i^j, \quad i=-n_L, \dots, n_R, \quad j=0,\dots,M$$
and the normal equations for the vector of $a_j$'s in terms of the vector $f_i$'s
$$a = (\mathbf A^T \cdot \mathbf A)^{-1} \cdot (\mathbf A^T \cdot \mathbf f).$$
The specific forms of the involved matrices are
$$(\mathbf A^T \cdot \mathbf A)_{ij} = \sum_{k=-n_L}^{n_R} A_{ki}A_{kj} = \sum_{k=-n_L}^{n_R} k^{i+j},$$
$$(\mathbf A^T \cdot \mathbf f)_j = \sum_{k=-n_L}^{n_R} A_{kj}f_k = \sum_{k=-n_L}^{n_R} k^j f_k.$$
There are now two crucial observations that allow for a computationally efficient method. First, we are interested in $\hat f_0$, the value of the fitted polynomial at $i=0$, which is the coefficient $a_0$. Therefore, we only need a single row of the inverted matrix, which can be done with an LU decomposition and a single backsolve. Second, the coefficient $a_0$ is linearly dependent on the input data $\mathbf f$, so it can be expressed as a weighted sum
$$\hat f_0 = a_0 = \sum_{k=-{n_L}}^{n_R} c_k f_k$$
where $c_k$ is the component of $a_0$ when $\mathbf f = \mathbf e_k$, i.e.
$$c_k = \left[ (\mathbf A^T \cdot \mathbf A)^{-1} \cdot (\mathbf A^T \cdot   \mathbf e_k) \right]_0 = \sum_{m=0}^M \left[ (\mathbf A^T \cdot \mathbf A)^{-1} \right]_{0m} k^m.$$

Note that weighting the regression in this method corresponds naturally to adding the weight matrix
$$c_k = \left[ (\mathbf A^T \cdot \mathbf W \cdot \mathbf A)^{-1} \cdot (\mathbf A^T \cdot \mathbf W  \cdot \mathbf e_k) \right]_0$$

### Implementation

The gist of the implementation is to calculate the coefficients $c_k$, with a Gaussian weighting and a fixed window length, and then convolve these coefficients with the data. Thus, the matrix inversion and multiplication is done once and the rest is multiplication and addition.

The left boundary is handled by reducing the window size. For the smallest window sizes (i.e. the first 2 data points), the raw data is used.

nan values are handled by fitting a polynomial of the given window length to the immediate past and extrapolating. nans on the boundaries are handled by reducing the window size.

Note that the SG filter subsumes many standard smoothing methods: $M=0$ corresponds to windowed average and $M=1$ corresponds to local linear regression.

(Although scipy does implement the `savgol_filter` [method](https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.savgol_filter.html), it is not causal (i.e. uses information from the future). We implement our own, taking inspiration from the scipy implementation.)

## Moving window average

This is the standard smoother that averages a window of the past (say, two weeks worth of values) to estimate the present day. It is implemented, as a reference, in `moving_window_smoother`.

Given $y_1, \dots, y_n$, the smoothed value at $\hat y_n$ for a window of length $k < n$ is
$$\hat y_n = \frac 1 k \sum_{i=n-k+1}^n y_i.$$