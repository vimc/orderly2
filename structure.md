# Layout

Everything happens within an outpack repository, typically also a git repository.  We'll refer to the "root" which is usually where `.git` would live.

## Basic metadata

Our metadata store will be in `.outpack` and no file here is user editable.  The primary thing that this store contains is information about packets that might (or might not) be found within the archive.

```
.outpack/
  metadata/
    <location-id>/
       <id>/
         metadata.json
```

This means that the packet name can be anything really (including containing non-path-safe characters) but it will make some operations quite tedious, for example:

* what is the full set of packet names?
* what is the most recent copy of packet name X?

The first of these requires reading through the entire set of json files, and the latter does so in the worst case (packet name does not exist, or is found in the earliest packet).  That suggests that we need some local index over these.  This could be done per session or something shared, depending on concurrency issues.

It's not clear at all if we should put things as

```
       <id>/
         metadata.json
```

or

```
       <id>.json
```

The former allows more easy addition of associated files (e.g., some partial processing to grab a minimal subset of metadata for an index, or a signature file etc), but has a weird redundancy `metadata/<location-id>/<id>/metadata.json`

The latter is simpler and less redundant but requires a bit of path manipulation (e.g., finding all ids requires that we string remove `.json$`) and might be less future-proof.

In any case, we end up with one id per packet per location.

An alternative location, which might be a bit easier to work with:

```
.outpack/
  metadata/
    <id>/
      metadata.json
  locations/
    <location-id>/
      contents/
        <X>
```

Where would decide on `<X>` to be one of

* empty file with an id
* a symlink to the `metadata/<id>` directory

This gets a bit weird if we have signing, because who is doing the signing?  If we sign by creator only then we don't really have a problem but distributing keys is quite dull.  If we sign by location (so a location imports a packet from somewhere else and signs that metadata in their local storage), then we should track signatures by location.

Some filesytems don't respond that well to having a billion files dumped into one folder (ext2/ext3 are bad at this, among others).  We could shard the files a bit (as git does) with the added complication of having to loop over a set of directories to get a final list of ids.

## Location metadata

# Workflow

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

As above, but from the perspective of the location.  Which packets are we able to send?  Everything we trust: that will the the union over all trusted locations.

## Locally running

We'll need the same concept in orderly of a workspace that things are run in; unlike orderly there's no reason why things might not typically be run in the (equivalent of the) `src/` directory, which would lend itself to simpler but less reliable workflows.

If we always have a special location `local` we can store local metadata that has been saved (equivalent of orderly's commit).  We do not save metadata for unsuccessful runs in the same place though.

## Server versions

# Interactions

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

## Create a portable package

* We can simply zip a directory
* We can zip a directory and limit to files that are included in the manifest
