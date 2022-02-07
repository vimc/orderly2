# orderly2

`orderly` has been useful for a number of projects in the centre but it is time to think about improvements based on what we have learnt.

> The focus of the core of orderly/orderly2 should be the ease of sharing and collaborative reuse of persistent artefacts.

This is a little less and a little more ambitious than the goal of `orderly`.

An extensible metadata format that we will use for the data format.  The idea being that there are some extensions that are project-specific and others that are engine-specific.  Core metadata must be provided by all engines for all projects.

## What is the issue that orderly solves well?

We imagined orderly as a reproducible/trustable reporting framework, then morphed it into something else.  It does the reproducible reporting OK, but it's the workflow aspect that is more useful really, and in that it's not even the "DAG of separable bits of work" so much as a "teamwork over a complex piece of work" type thing.

Have a group of researchers working on a shared source tree, able to share outputs of computational work that would be time-consuming to rerun and have high confidence that they are looking at the same thing as each other.

There are two parts to this:

* transport: OrderlyWeb (and to a degree things like the SharePoint version) allow sharing of results
* archive: The representation of orderly tasks on disk, with a few conventions allowing the endpoint of one piece of work to be the beginnings of the next

## Some issues poorly dealt with in `orderly`

### Namespacing and provenance

(see also the section "Sources and trust" below)

We have some missing details in how we track where things have come from:

* where they were computed
* where their canonical location is
* where we fetched them from

This is an issue with doing things like a "partial pull" which we hacked into orderly to prevent downloading complete trees of data.  Git does not really care about most of this, but I think these are all interesting to know about in the context of bits of analysis.

**Computation**: Each compute node can have a URL which we include here.  That could derived from the hostname, or set in a configuration.  In an ideal world this would let us know that something was run on the cluster `mrc78.dide.ic.ac.uk` or on some trusted orderly server `montagu.vaccineimpact.org` or on someones desktop `rich.local`.  This corresponds to a pointer to where the calculation happened, but not where it could necessarily be pulled from.  As in orderly at present we should include information about the environment (package versions etc) but this will necessarily be engine- and application-specific.

**Persistant location**: We already have the concept of relocatable reports with orderly bundles, and we use a more adhoc system with the cluster for the rtm project.  Having a good idea of a canonical location is important because then if we don't download the full tree we know that we can get that tree from its canonical location.  This corresponds to a persistent location for the resource (i.e., a URL!) but not necessarily where it was computed, nor where an individual pulled it from, but where it could be fetched from in future.  This could be implemented if we import an archive into a repository that is willing to share it in future.  We could also perhaps discover this as part of fetching metadata from another archive.  The idea here is that if we pulled part of a tree we'd like to pull metadata that indicates where we might pull the full archives that correspond to other components of that tree.  Any location that has a full tree may be a persistant location, but not all will be.

**Fetched from**: If you pull a task into one repo on your machine, then from there pull it into another, this could be tracked.  Alternatively we could ignore this information and retain only the canonical location.  This could be neither the location that it was computed on, nor the canonical location.

The second and third of these should not be baked into the metadata.

The location should be more than just the hostname as that will not work when we work out how to host more than one OrderlyWeb instance on a single node (in that case we might have `example.com/core` and `example.com/hiv/transmission` as two different locations).  Non OW transport might also be supported here...

### Naming

Related to the above, there are some issues here:

* names can't have a hierarchy (e.g., `data/collate`) even though this is just a limitation of `orderly`, not something fundamental
* when pulling a repository it can't be renamed, even if the names make no sense in the destination repository

On this second point, suppose that we have a data processing repository that people want to use in another repository.  They might call their final data product `final` but we want to use that as (say) `data/core` (their final data is our core input).  When we do the pull, or when we configure the way that their upstream repository interacts with ours, we should be able to remap the name so that we would use our name everywhere.

This becomes a bit easier to think about if we have the namespacing above, but would still require some care to get right.

### Subsets of repositories

Creating a subset of a private repository (source and output) for publication should be easier.  This is just a set of transforms on the metadata largely.  Each engine could provide support for doing a few renaming tricks.

In orderly this is largely restricted to:

* renaming tasks
* rewriting the `orderly.yml` to update dependencies
* dropping some fields in `orderly.yml`
* pruning dependency trees to move dependencies into being resources (this one by far the most complicated)

This will typically be a one-off process, though we've also done this with follow-on exports (e.g., at revision).  We might or might not want to create a persistant link between repos:

* we'd want to in the case where we're importing code as a project evolves
* we'd want not to in the case of exporting a public subset out of a private monorepo

### Hashing

There are some details with hashing that we never dealt with properly in orderly:

* md5 is not a great choice, we probably should be using sha256. Because we want to retain compatibility with orderly we probably need to make hashing configurable on a per-report/task basis
* we never properly cope with hashing of text files on platforms with different line-endings.  This will be an issue with (say) source files that have been under version control as git will transform line endings and normalise the hashes (https://git-scm.com/docs/git-hash-object, see `--no-filters`)

### Multiple languages

`orderly` is 100% R, which was fine for VIMC, but had some issues early on with the COVID response (with some ugly wrappers around python code), and will likely have issues with people doing things like standalone stan.

If we had a common format then it would not matter what the underlying engine used to run the results was, everything could be reused.

### Trust and mutability

There's no very easy way to check if parts of system have been modified - we should consider signing these, perhaps optionally.

It should be easier to validate a single archive, and the whole set, with repair by redownloading broken (e.g., locally modified) files/archives.

### draft vs commit is confusing for users

This is a pretty leaky bit of abstraction that we should sort out.

The problem that we're trying to solve is this:

* there are canonical reports/tasks that we might want to prefer
* there are reports/tasks that are under development that we might want to use locally
* the user needs to make sure that their experimentation does not end up in the main tree

We use the same draft -> report approach on the servers where experimentation is not allowed/does not happen.  There, we run a report in drafts, then move it into place when we "commit".

It's not totally clear what should happen here typically.

Thinking about files on disk, on the server, everything in `archive` is equivalent (i.e., there is a single tree) and that's OK.

On the server, while something is running it does need to be flagged as in staging and not complete.  Then it gets moved into place.

For a user, they need to be able to be able to control at `orderly_run()` the priority to look in.

I think that a "local" directory makes as much sense as "draft" and might be clearer?

### Use of SQLite

This is fine generally, but has some drawbacks:

* it behaves very poorly on a network drive, and is a block to parallel operation (where you can get locking) or just any operation from Linux (even in series)
* it throws baffling errors to users (e.g., FK constraint violation) instead of something actionable
* it's very slow on windows (most orderly tests skipped on CRAN due to this taking 20x longer than on Linux)
* We've ended up sharing this between orderly and OW which is not ideal as we don't really know what is being depended on.

All we wanted from this is some index, but we have a fairly simple structure that can be read fairly quickly on an as-need basis (particularly with caching/serialisation into a native format) without hitting locks or contention.

### No easy way of rerunning

This is only because we've never implemented it, but something to consider a bit.  Because an orderly archive includes almost everything required to run (sources, resources, dependencies etc) it should be possible to rerun it, or open up a session where it could be played about with.  This is particularly useful when trying to work out why something has changed:

* this needs to be done out-of-tree as to avoid accidental modification
* things like DB connections, secrets, environment vars might have changed and will be hard to replicate
* we might want to allow refreshing of _some_ inputs (e.g., same everything but update this one dependency please)
* We might want to rerun locally or in the original place
* Rerunning might be exploratory (let me recreate the computational environment and poke about) or it might result in a report that would be persistant

### Flexible dependencies and artefacts

This has been requested frequently and I've always opposed it.  However, from the point of view of the metadata format it would go away as a thing (we compute what is used and that gets written out explicitly like the flexible resources do).

So for example, we might want to depend on some set of reports matching a pattern (e.g., pull in one per country for some set of countries specified when running the report).  Or we might want to produce some artefacts only when a particular parameter is set.

The reason to avoid this is that then there's too much logic in the tool that is just running the analysis, requiring another level of orchestration to control.  To a degree we suffer here with the query dependencies as these are hard to debug.

This would probably require a little extra metadata to be added by the engine to record what queries we ran, but would not affect the general metadata as we'd always write out what they resolve to.

Use cases:

* depend on some flexible set of upstream task:
  - if a parameter is 'x' pull in task `A` otherwise `B`
  - pull in dependencies from the script itself (`orderly2::orderly_depend("other", "id", "path", ...)`)
  - depend on some matching set of reports (e.g., all reports from date X expanded over country)
  - pull in all files found from a report, matching some path/pattern
* output a flexible set of artefacts
  - Save everything in a directory (e.g., `figures/*.png`) similar to how we do directory resources at present
  - Mark resources in the script itself (so `orderly2::orderly_resource("path.png")`

These would all be implemented in the engine (and the dynamic ones require that there's a pretty strong concept of "running report"), with the final fully resolved information saved out into the final metadata.

## Running on HPC

We don't scale well past one node because the task has to run in one go.  If we had more scope to run up to a point, launch 50 jobs, then finish we'd be able to do all sorts of fun things.

### Integration with other tools

There are any number of [workflow tools](https://github.com/meirwah/awesome-workflow-engines) and [pipeline tools](https://github.com/pditommaso/awesome-pipeline) that exist already, we should make sure that we interact well with these.

For example, a series of reports could be run as a [CWL](https://www.commonwl.org/) script, or an individual task could be a [targets](https://books.ropensci.org/targets/) project.

Many of these have fallen apart over time (e.g, the ironically named [Workflow 4 Ever](http://wf4ever.github.io/ro/) project)

The focus of orderly/orderly2 should be the ease of *sharing and collaborative reuse* of persistent artefacts.

## Future issues to deal with

### Sources and trust

This does not mean "trust" in the "would I let it come in here" meaning but the "do I take this as an authoritative copy" sense.  If you trust a source you trust everything in it, including things it has fetched from.  This builds on ideas in the [orderly patterns vignette](https://www.vaccineimpact.org/orderly/articles/patterns.html).  There's considerable prior work in this space, and terminology we will want to align with [e.g.](https://twitter.com/seldo/status/1486563446099300359/photo/1).

Our previous idea of draft/archive mixes two things

* We want to be able to some *development locally* creating *untrusted* reports that should be excluded in some situations and included in others.
* We also use this as a *staging location* when running reports (in both the trusted and untrusted settings).

To improve this:

* So if we have a process of staging -> archive that's the second part of this
* On run we should be able to filter by trust level (perhaps just untrusted/trusted but if we had multiple trust levels?) and mark the output with the lowest level of trust that we have among sources
* Trust level is configured per instance (so I clone the source repo, say: "here is my upstream source list, with trust levels assigned")

**Example 1 - centralised**

* single canonical source of files (https://orderly.example.org)
* reports are run on the the server (so runner is the same as the source)
* users all agree that we trust this source
* pulling from a source might pull recursively, or might pull just one report
* if we pull just one, then we leave a pointer to the canonical source so that later on it could be easily fetched (the use case here would be filling in a repository or fraction of a repository)

**Example 2 - outsourced computation**

* computationally heavy work run on an HPC system
* imported into a distribution server, and marked as trusted at this point as ingest is restricted
* users download from the server and trust the output

**Example 3 - hierarchical repositories**

* central data server runs reports that are used and trusted by several groups
* secondary server pulls data from the central server (and trusts) it, runs downstream reports that use it
* there are multiple of these secondary servers, and none of them necessarily trust each other

## Migration plan

Come up with a metadata format and some simple tooling to manipulate them, then reimplement orderly on top of that.  This will be done at first in the package `outpack` and then we'll expand that to work with another language and/or engine.

## Prior work

Since we started orderly in 2017, what other attempts have been made to solve this problem?

* https://recipy.readthedocs.io/en/latest/user_manual.html
* https://sumatra.readthedocs.io/en/latest/
* https://handbook.datalad.org/
* https://mlflow.org/docs/latest/tracking.html
* https://github.com/minerva-ml/steppy
