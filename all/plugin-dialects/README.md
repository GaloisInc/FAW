# Dialects Wizard Plugin

FAW plugin for partitioning the corpus into dialects.

## Usage

Click `Find new dialects` to attempt to partition the corpus. Targeting
particular features narrows down the set of files to partition.

### Settings

#### Target feature search

Use this to find features to target or exclude.

#### Target

Select a set of features (or inverted features) to target their conjunction.
The set of files matching all targeted features is partitioned instead of the
entire corpus.

Files outside the target count against the outlier limit or are ignored; adjust
`Dialect out-of-target requirement` to change this behavior.

#### Excluded features

Excluded features cannot be used as dialects. This also excludes features that
are attributable to the excluded features, according to the
`Attributable risk threshold` (see below).

When a particular split is known to be uninteresting, and it's cluttering the
results, excluding it can be useful.

#### Attributable risk threshold for exclusion

This value is used to exclude features similar to the excluded features
(if any), and to disallow similar dialects between partitions (if enabled).

A feature is considered attributable to another if its risk attributable to
the other feature exceeds this threshold. This is calculated on target files
only; similarity outside the target is ignored.

Attributable risk, also called risk difference, varies from -1 (complementary)
to 1 (identical). It's not symmetric, and only measures attribution in one
direction. Given the following contingency table, where "Exposed" means exposed
to the feature we're taking as the risk factor, and "feature" refers to the
(dependent) feature we are considering:

|             | Feature | No Feature |
| ----------- | ------- | ---------- |
| Exposed     | A       | B          |
| Not Exposed | C       | D          |

Attributable Risk $= \frac{A}{A+B} - \frac{C}{C+D}$

#### Min dialect size

Number of files (within the target) that a feature must have in order to
potentially be a dialect in a partition.

If `Max files in multiple/no dialects` is greater than the min dialect size,
that value is used as the min dialect size instead.

#### Dialect out-of-target requirement

This setting determines how a feature's out-of-target representation affects
its eligibility to be a dialect. There are three options:

* Homogeneous outside target (default): Avoid selecting features with lots of
  interesting variation outside of the target as dialects. Out-of-target files
  with a feature (or without, whichever is fewer) count against the outlier
  limit if the feature is chosen as a dialect.
* Not represented outside target: Avoid selecting features that are positive for
  files outside the target as dialects. Out-of-target files with a feature
  count against the outlier limit if the feature is chosen as a dialect.
* No restriction: Out-of-target files are completely ignored, and have no
  bearing on dialect selection. This should produce the same behavior as running
  the plugin on a filtered analysis set.

#### Max files in multiple/no dialects (outliers/slop)

Limit on the number of outliers/slop files; that is, files which aren't neatly
sorted into a single dialect. Out-of-target files may also count against this
limit, depending on the `Dialect out-of-target requirement` setting.

#### Max partitions

Halt the search after finding this many partitions. Increasing this setting
increases the runtime of the plugin.

#### Max dialects per partition

Limit each partition to this many dialects. Increasing this setting increases
the runtime of the plugin.

#### Disallow very similar dialects between partitions

If enabled, once a feature is chosen as a dialect in a partition, prevent it
(or features attributable to it, according to the attributable risk threshold)
from being chosen as a dialect in another partition.

Not enabled by default, since usually this in unnecessary. Enabling this setting
decreases the runtime of the plugin.

#### Include dialects based on inverted features

If enabled, dialects can be based on inverted features as well as non-inverted
features. All the same restrictions apply to choosing inverted features as
dialects as to choosing non-inverted features.

Not enabled by default, chiefly due to runtime. Enabling this setting increases
the runtime of the plugin.

#### Highlight file

Provide an exact filename here, and dialects containing it will be highlighted
in the results.

### Results

After running the wizard, results are shown below.

#### Partitions

Each partition is an attempt to roughly split the target into dialects.
Partitions are listed in the order the search algorithm discovers them. The
search algorithm prioritizes similarly-sized dialects, high attributability of
other features to the dialects, and low outlier count; partitions like this
tend to be listed first.

If no partitions were found, this means the features that could partition the
target either do not exist, were excluded, or contributed too much slop. If you
find no partitions often, usually the most effective rules you can relax are:

* `Max files in multiple/no dialects (outliers)`
* `Dialect out-of-target requirement` (if a target is specified)
* `Include dialects based on inverted features`

#### Quality metrics

Miscellaneous partition quality metrics. These metrics are not used in the
search.

#### Dialects

Partitions comprise two or more dialects. Each dialect is based on a single
feature, but features with highly similar distributions on the target are
listed as well (as "Implied Features").

Dialects are numbered so they can be referenced in
`Files in multiple/no dialects`.

#### Implied Features

Features attributable to the dialect's hero feature are listed here. The hero
feature is prioritized from among these due to the following factors:

* Slop contribution. Some of the "Implied features" may have higher slop than
  the dialect's "hero" due to out-of-target files.
* Attributability of other features to the hero. Some of the "Implied features"
  may be more attributable to the chosen hero than the other way around.
* Size. The partition finding algorithm prefers mid-sized features over very
  large or small features (this is essentially a tiebreaker)

Inverted features are included in "implied features" regardless of if they were
included in the partition search--this doesn't significantly affect plugin
runtime, can be informative, and could help suggest when enabling inverted
features would be helpful.

#### Out-of-target files

If a dialect's hero feature includes files out of the target, they're listed
here.

#### Files in multiple/no dialects

Files in multiple dialects, or in no dialects, are listed here. The numbers in
brackets refer to the dialects a file falls in. For example,
`In 2 dialects: [ 1, 2 ]` means the file is in the first and second dialects.

## Partition search algorithm

Partition search happens in several stages:

1. Deduplicate features
2. Find legal dialect hero features
3. Depth-first search
    1. Prune incompatible heroes
    2. Prune candidates for next step
    3. Prioritize candidates
    4. Confirm valid partition

The following sections explain the logic of each stage, and its motivation.

### 1. Deduplicate features

In corpora examined by the FAW, we expect to commonly find features that appear
on the exact same distribution of files. For example, maybe a parser outputs
redundant status messages when a file has a particular type of error. Generally,
this is not meaningful, and to prevent our statistical analysis from inferring
meaning in such cases, we deduplicate features as a first step.

Features are grouped into equivalence classes by their file distribution
_on the target only_. The class is then assigned a representative feature.

The representative of each equivalence class is the duplicate feature that
would contribute the least slop from the out-of-target requirement. **We assume
that the overlap between out-of-target slop is negligible**, so that we can
ignore the other duplicates in each class, which is a trade-off for simplicity.
Considering overlap between out-of-target slop here would require keeping track
of the representatives of each equivalence class throughout the search,
"collapsing" to the best alternative once other dialects are chosen for the
partition. This is possible, but would complicate the search algorithm
significantly for minimal benefit.

#### Note on inverted features

When inverted features are included in the search, they're concatenated onto the
rest of the features in the pool before deduplication. This is so that inverted
features can be selected as representatives if they would contribute less slop
than non-inverted features that they coincide with. Thus, for the rest of the
search process, for every (deduplicated) feature $A$, there is another feature
with an inverted distribution on the target, but it may not actually be
$\neg A$. We need to keep track of this for later, since $A$ and $\neg A$
shouldn't form a legal partition together (this isn't informative), but $B$ and
$\neg A$ should when $A$ and $B$ have the same file distribution.

### 2. Find legal dialect hero features

From the set of all (deduplicated) features, we select those that could be used
as hero features (representatives) for dialects. A features is valid as a hero
when all of the following are true:

* It is present for at least `min_dialect_size` files.
* It is _not_ present for at least `min_dialect_size` files. This is necessary
  so that there's enough space left for a second legal dialect.
* The feature's inherent slop (from out-of-target files) is less than the max
  slop.
* The feature is not highly attributable to any excluded feature (according to
  the configured attributable risk threshold)

### 3. Depth-first search

The partition search is depth-first and greedy; the first valid partitions
found are kept, up to the maximum number of partitions.

As written, nothing is cached in the search stack. This could be changed to
improve runtime, but it hasn't seemed necessary so far.

### 3.1. Prune incompatible heroes

Given an incomplete partition, we want to select the hero feature for the next
dialect. Starting with the set of all valid dialect heroes, we remove the
following from consideration:

- Features which would push us over the slop limit on their own
- Features that would contribute more slop than coverage
  - This includes features already in partition, so we don't get the same
    feature in the partition more than once
- Features contributing less than the previous feature in the
  partition did, so we always build partitions starting with the largest
  features (and avoid repeating work by blowing up the stack with duplicate
  incomplete partitions)
- Inverses of features already in the partition
  - I.e. if $A$ is in the partition, we'll discard $\neg A$. We don't discard
    $B$ if it has the same file distribution as $\neg A$.

After this pruning, we are left with the set of candidate hero features that
could potentially be added to the incomplete partition later in the search.

Before moving on, we check the distribution of files covered by any of these
candidates. If more files are unable to be covered than we have capacity for
(i.e. if it would cost too much slop), this search branch is terminated. This
makes a very big difference to performance.

### 3.2. Prune candidates for next step

Next, we prune the set of candidates to those suitable to be the _next_
dialect. Features removed at this point may show up later on, but discarding
them now is a significant improvement to performance, preventing the search from
repeating work. We discard:

- Very small features, smaller than one equal part of the remaining files if all
  allowable dialects spent

### 3.3. Prioritize candidates

Given an incomplete partition and the list of candidates to add to it next, we
add an entry to the stack for each. We prefer:

- Features that cover rare files we don't have yet
- Features that contribute less slop
- Features that many other features (including invalid heroes) are
  attributable to
- Features with high entropy based on their size (i.e. mid-sized features)

They are sorted according to this priority:

priority = `binary_entropy * lugsumexp_attr_risk + fraction_of_slop_left + rarity_satisfaction`

where

* `binary_entropy` is the binary/Bernoulli entropy of the feature on the target
  files. This is greatest for features covering half the target.
* `logsumexp_attr_risk` is the attributable risk from each feature to this one,
  aggregated with logsumexp (as a soft maximum) and normalized to between
  0 and 1.
* `fraction_of_slop_left` is the fraction of current remaining slop capacity
  that would be left over after adding this feature to the incomplete partition.
  It's 0 when the feature uses up all of the remaining slop, and 1 if it uses
  none. This term is left out of the calculation if slop is already exhausted.
* `rarity_satisfaction` is a measure of how rare the files covered by the
  feature are. It's the (again, normalized) logsumexp aggregation of
  file rarity, where file rarity is the fraction of *other* features that don't
  have the file, for a feature with it. Formula:
  `(num_candidates - file_coverage_by_candidates) / (num_candidates - 1)`;
  File rarity is 1 when only 1 feature has a file, and 0 when all features
  have it.

All four of these quantities vary between 0 and 1.
`binary_entropy * logsumexp_attr_risk` leads to more interesting partitions,
and `fraction_of_slop_left` and `rarity_satisfaction` lead to features more
likely to end up in a successful partition search. Early in the search, when
there isn't much slop yet, the first term determines feature ranking.

Aside from the above, and the fact that `fraction_of_slop_left` and
`rarity_satisfaction` need to be additive, since either could zero out the
whole term, aggregation of these quantities is not particularly informed.
Weights are all 1.

For performance reasons, `binary_entropy` and `logsumexp_attr_risk` are
precomputed before the search. Pairwise attributable risk is especially slow to
compute.

### 3.4. Confirm valid partition

Once we have a complete partition satisfying the slop criteria, we confirm that
it isn't a duplicate of a partition we already found. If each feature in the new
partition is attributable to a feature in another partition, we can discard the
new partition. Conversely, if each feature in the other partition is
attributable to a feature in the new partition, we can discard the other
partition. If each is attributable to the other (as is usually the case when
one is attributable), we discard the one with less slop.

The exclusion attributable risk threshold is used here to decide if partitions
are sufficiently similar.

## Development

The plugin is a local python package called `dialectsplugin`. As with other
python plugins, the FAW runs it from the local path at runtime without
installing it. Since the package has a `__main__` module, it is run with
`python -m dialectsplugin`.

### Running tests

Running tests requires `pytest`.

```console
$ python -m pytest .
```
