# Layout

Everything happens within an outpack repository, typically also a git repository.  We'll refer to the "root" which is usually where `.git` would live.

Like with orderly, we will strongly advocate not adding outpack files to git; this will cover both the metadata (`.outpack/`) and the actual packets themselves.  Source files and configuration would be under version control.

## Basic metadata

Our metadata store will be in `.outpack` and no file here is user editable.  The primary thing that this store contains is information about packets that might (or might not) be found within the archive.

Because an id must resolve to a single packet, we keep it all in a single directory `metadata`; implementations will build an index over this for efficient querying (it is not expected that the json files would be scanned through regularly)

```
<root>
  .outpack/
    metadata/
      <id>.json
```

This has the slightly unfortunate property that we need to some string manipulation, but the canonical name for the id is contained within the file (an alternative would be to use the hash as the filename, but that feels more annoying).

## Locations

Every location has a concept of the last time that it was updated, in UTC.  This allows us to build a log.  So if we last queried a remote at `t0` we can ask it for new entries imported since then (even if these were imports of older packets) and it should be able to tell us.

To support this we need to hold some information about when we imported any bit of metadata.

```
<root>
  .outpack/
    location/
      <location-id>/
        <id>.json
```

with this file containing simply the time that we downloaded the data, and the hash of the data.

We can then find from these (by reading every one) when the last time that we pulled data from that location is, and can pass that to the location when querying.

## Signatures

*I'm less sure about this, and don't think we'll implement it for the MVP, but something worth considering, I think*

One thing that would help with trustability is if packets were signed by a system.  Then you could use that to decide if you wanted to include them.  This would be fairly decoupled from locations.

There are many places where a packet might be run:

* On someone's machine, on a messy session with globals and packages installed from anywhere at all
* On a HPC, perhaps where the job is manually distributed across nodes before being finished off, but we're still confident that it represents something reliable
* On a trusted server, triggered by an automatic run

We could sign these immediately after running (would make sense in the second and third case), or on import (so a distributing server may sign packets that they distribute to vouch for them).

To store these we can use

```
<root>
  .outpack
    signatures/
      <pubkey>/
        <id>.json
```

Where this time the json includes the hash algorithm used and the signature content.

Validating the packet would then look like:

* verify that the file `<root>/.outpack/metadata/<id.json>` using the signature `<root>/.outpack/signatures/<pubkey>/<id>.json` and some public key that you have previously distributed
* verify all files in `<root>/archive/<name>/<id>` using the hashes present in the metadata file

## Index

Using these json files directly would be quite inefficient, so it'll make sense to build an index.  We assume that each engine will create its own index and not bother using anything elses.

Ideally an index will be multiprocess safe, and so not suffer corruption if two processes try to update it at once.  In general, index-writing functions should be fairly rare though, even if index-updating functions are common as it is not necessary to write every change back to disk for long running processes.

An example might look like:

```
<root>
  .outpack
    orderly/
      index/
        core.rds   <-- stripped down index of name/id/parameters for query
        full.rds   <-- full index over all queriable metadata?
```

## Archive

We'll also want copies of some packets actually downloaded to work with (otherwise we can't use them on-demand!).  In orderly we put these all under `archive` and I think that's a reasonable first cut.

The structure there should be user-friendly (for searching through) so keeping things as before

```
<root>
  archive/
    <name>/
      <id>/
```

Unlike orderly `<name>` here could contain subdirectories.  If we allow renaming on import (see below) it could be different to the `name` field in the json

There's an open question as to if it makes sense to include the metadata here too.

* **Pros**
  * if we downloaded a zipped copy of the directory we'd have everything
  * it would act as a manifest for any directory
* **Cons**
  * if one copy is edited we need to verify
  * it makes it less obvious who is tracking the source of truth

There's no reason why new trees could not be built, so we might not want to limit to a single `archive` directory.  That could be confusing, but I've definitely cloned orderly repotories elsewhere to build a single consistent archive (e.g., download a copy of data/fits and all its dependencies).

To support this we need some per-user configuration of additional archive trees. To allow this extension in future, we'd just need to make sure that `archive` does not get too heavily hardcoded everywhere

## Local copy

In orderly we have a "`draft`" directory which is very useful.  Originally the idea was that people would commit things from their draft to their archive but that became quite annoying, and so we have `use_draft = "newer"` which is basically "Use my untrusted stuff if it's newer"

In some ways this is a bit like the multiple archive trees idea above.

When an local report is created we put the metadata into `<root>/.outpack/metadata/local/<id>.json`, with the "local" name being special perhaps.  We will need a little maintenance trick to clear out of stale things here as users should be free to delete local packets as they feel free.

# Interactions

## Importing a packet

This is from the perspective of a user downloading into a local copy.  We would have:

* A location id
* A metadata json file
* A zip file or a directory of files

The process would look like:

* Validate that the metadata does not differ from any other copy of that metadata found elsewhere (under a different location)?  If it does differ, what do we want to do?
* If a signature for that metadata is present, validate the contents are correct, store the signature alongside the metadata.
* Put the metadata in the `.outpack/metadata/<location-id>` directory
* Copy the actual packet files into place, e.g. `archive/<name>/<id>`; the `archive` part of this path should be configurable at the level of the outpack project (shared among all users, so under version control, perhaps)

Open questions:

* does the path `archive/<name>/` really need to reflect the `name` field in the metadata?  This feels unnecessary really, and there are use cases where that might not be wanted (see below)
* what is the correct behaviour if the metadata has changed (that is, we find that the metadata from one location differs from another)? How would the user fix this?  What is the mechanism by which this would ever happen (a data migration perhaps?)

### Renaming on import

Thinking here of HIV as an example, but also things like the RTM work where we carve off a paper.  The naming system in one repository need not reflect another.

So if we have a part of our location configuration some rewriting rules we could do things like:

* Import the packet `rtm_collate_data` as simply `data`
* Import the packet `final_output` as `upstream/input`
* Import from several different similar repositories with a prefix (`oli/analysis` vs `rachel/analysis`)

The sorts of rewriting rules that might be useful here could be:

* add a prefix
* one-off-renaming
* text substitution

We should not change the actual metadata.json though so that these would not ever change.  This means that turning on or off renaming rules might be wanted when doing queries etc?

What happens if the user wants to change a rewriting rule after an archive has been downloaded? That would result in things not being found in the right places (we'd never know).  When querying we'll only ever look in the metadata and not in the on-disk copy, so after a rewrite rule change we might silently ignore some directories.

1. Suppose that a user downloads packet A from location `X`, they can then use that by requesting, say, `latest(A)`, and that's fine.  Suppose we have versions `id1` and `id2` downloaded already (e.g., `archive/A/id1`)
2. Then we want to set up a rewrite rule for this location so that we want to refer to things as `data/<name>` and immediately we download `id3` as `archive/data/A/id3`
3. At this point `latest(data/A)` should return `id3`, but the behaviour of `latest(A)` is not well defined
   - if we'd only found `A` packets from our location `X` then we'd "want" to rename these existing packets to move them within `data/A`
   - if we'd found `A` packets also from another location then it would not be well defined, and these should not be findable by any query

This seems suboptimal!  So if we track where we've pulled things in from we could probably do better here.  If we remember where things come from we could have any "repair" process do a rename.

Regardless, the concept of renaming introduces the possibility that one "name" may identify exactly the same packet if things have come through two places, and that's just something that people would have to look out for.

## Sending a packet from a location

As above, but from the perspective of the location.  Which packets are we able to send?  Everything we trust: that will the the union over all trusted locations.  So part of the configuration of a server would include which locations we're happy to forward.

For less smart locations that's a bit harder - something here like the dropbox/sharepoint locations that just distribute files.  Here, I think we're ok in that these locations don't try and distinguish between locations?

In some ways this brings us towards the git "bare" checkout - a copy on dropbox probably contains the metadata at the top level (rather than in dot directories) and as we only interact with it via the import/export commands (rather than the full set of outpack commands)

## Locally running

We'll need the same concept in orderly of a workspace that things are run in; unlike orderly there's no reason why things might not typically be run in the (equivalent of the) `src/` directory, which would lend itself to simpler but less reliable workflows.

If we always have a special location `local` we can store local metadata that has been saved (equivalent of orderly's commit).  We do not save metadata for unsuccessful runs in the same place though.

## Queries

We support this already to a degree but it's been quite ad-hoc and some rationalisation would be good

* which locations contain a particular packet? Alongside queries we could provide a list, or provide the "most trusted" location.
* filter by trust
* filter by parameters more easily
* filter by presence of particular paths (most recent packet with subpath `outputs/afg.csv` for example)
* filter by location in the graph (version of X that was depended on by packet Y)
* filter by locally present (how do we tell this easily?)
* indicate reasons why packets do not satisfy query criteria (this turns up when you get a "no suitable report found" error in orderly atm and it's very confusing)
* filter by dates (min and max, or by elapsed time)

## Sync metadata

This should always be fairly fast; we should always be able to ask a location what they have, and then download/fetch all missing cases.  It might be fastest to get the whole lot at once, or all since some date (we can't assume that ids will increase monotonically if locations can import things from other locations).

## Deleting from the archive

As [requested by Sangeeta](https://github.com/vimc/orderly2/issues/1)

> Cleanly delete archived copies. I often wish to delete reports that I have archived because my code has changed and the archived versions are of no use to me. This was particularly an issue when the outputs were big and I had limited space on my machine. I think I can see why this would not be a good idea wrt reproducibility but one instance where the user should be able to delete them easily if they have been pulled from some remote?

With this approach the user can delete things from the archive at will because there is nothing special about it anymore.  We no longer have the `orderly.sqlite` db that needs to be kept in sync, and we can always refetch things (and know where they could be found).  We should be able to add "repair" functionality pretty easily (see below)

## Repair the archive

If you have a local tree that has been deleted from (above), it should be easy to "repair" it by looking for all dependencies up the tree and pulling them from a trusted location.

One could use this to repair individually downloaded directories too, comparing them against the metadata manifest.

See above about notes on renaming; something may be needed here too

## Removing location

Would we ever want to do this?  We could delete a location, and all its metadata, so that it no longer turned up in queries.  We'd still potentially have copies of packets from this in our tree, and at that point we'd want to copy the metadata over to make them local, (or orphaned, which might be better as one could then delete those later).

# Summary

```
<root>/
  .outpack/
    location/
      <location-id>/
        <id>.json
    metadata/
      <id>.json
  archive/
    <name>/
      <id>/
  local/
    <name>/
      <id>/
```
