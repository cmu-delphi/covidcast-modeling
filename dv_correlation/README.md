Ryan Aug 25th at 9:19 AM

> I just want to update you on something I found happening with the DV signal correlations at the county level, when I refactored/updated our correlation notebook yesterday.  You can see that the DV signal’s correlations sliced by time, having been degrading since April.  Right now it’s quite bad, the worst of our signals.
> I think James is going to make an issue of this on the covidcast-indicators repo, but I wanted to start the discussion here just with two thoughts as to why this could be happening.  (Let’s keep this to be just about the DV signal; general discussion about maintaining some kind of dashboard for our signals is here and here).

> Thought 1: Has the DV sample size dropping over time, and we haven’t noticed it?  This seems possible since we’re not even reporting this sample size publicly, and it would require us to look at it internally.  Assuming we erect a dashboard for the DV signal, like the basic dashboard I made for the FB signal, we could plot something like the relative sample size to what it was on the first day.

> Thought 2: Is this a reflection of a change in medical-seeking behavior, that is happening asynchronously across different counties?  Meaning, for example, some people may be going to the doctor’s office for more trivial reasons in some counties, but still only for serious reasons in other counties, and these progressions over time aren’t compatible across counties?  Intuitively, this seems entirely plausible, and this would mess up comparisons of our signal for a fixed time over space.  And there’s some empirical evidence for this: you can see that the correlations sliced by time are quite bad (time series plot), but the correlations sliced by county are still OK (histograms plot).

> If the second explanation is really what’s happening, then it’ll also be messing up any global forecasting model that tries to use the DV signal, since as time progresses, it’ll start to mean different things in different counties.  cc @Addison @Vishnu Shankar @Rob Tibshirani.

> And, if the second explanation is really what’s happening, then we might have to choose the denominator in the DV signal in a more “stationary” way.  That is, take the ratio of CLI-visits to something that is meaningful and stationary over time, or at least, it has the same nonstationary trend across all counties.

Aaron Rumack  2 days ago

> Re Thought 1: From the raw drops, it doesn’t appear that the sample size has been dropping over time. The denominator throughout July is about the same as it was throughout January.

Maria  2 days ago

> For what it's worth, here are the denominators at the state level for the latest drop. Like Aaron said, mainly steady -- new england actually looks to be increasing. We do see a peak in visits around May, but since June it looks somewhat flat. The decline at the end can be attributed to backfill. AK is somewhat anomalous. The only other concern would be data issues over at HSP, which is not out of question, but not possible to "fix" at our end.

![Denominator](denom.pdf)

Addison  2 days ago

> @Maria comparing to the correlations sliced by time in Ryan's notebook (attached), one could argue that the point where the DV correlation "diverges" from the other signals is in mid-June (i.e., DV correlation is steady-ish, whereas the other signals see a big jump in correlation).  This time period (mid-June) which is also when the denominator drops a lot in many states.  So this may give so credence to "Thought 1"?

![correlations](corr_multi.png)