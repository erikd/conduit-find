find-conduit is essentially a souped version of GNU find for Haskell, using a
DSL to provide both ease of us, and extensive flexbility.

In its simplest form, let's compare some uses of find to find-conduit.  Bear
in mind that the result of the find function is a conduit, so you're expected
to either sink it to a list, or operate on the file paths as they are yielded.

## Basic comparison with GNU find

A typical find command:

    find src -name '*.hs' -type f -print
    
Would in find-conduit be:

    find "src" (glob "*.hs" <> regular) $$ mapM_C (liftIO . print)

The `glob` predicate matches the file basename against the globbing pattern,
while the `regular` predicate matches plain files.

A more complicated example:

    find . -size +100M -perm 644 -mtime 1
    
Now in find-conduit:

    let megs = 1024 * 1024
        days = 86400
    now <- liftIO getCurrentTime
    find "." ( fileSize (> 100*megs) 
            <> hasMode 0o644
            <> lastModified (> addUTCTime now (-(1*days)))
             )
             
Appending predicates like this expressing an "and" relationship.  Use `<|>` to
express "or".  You can also negate any predicate:

    find "." (not_ (hasMode 0o644))
    
By default, predicates, whether matching or not, will allow recursion into
directories.  In order to express that matching predicate should disallow
recursion, use `prune`:

    find "." (prune (depth (> 2)))
    
This is the same as using `-maxdepth 2` in find.

    find "." (prune (filename_ (== "dist")))
    
This is the same as:

    find . \( -name dist -prune \) -o -print
    
## Performance

find-conduit strives to make file-finding a well performing operation.  To
this end, a composed Predicate will only call stat once per entry being
considered; and if you prune a directory, it is not traversed at all.

By default, `find` calls stat for every file before it applies the predicate,
in order to ensure that only one such call is needed.  Sometimes, however, you
know just from the FilePath that you don't want to consider a certain file, or
you want to prune a directory tree.

To support these types of optimized queries, a variant of find is provided
called `findWithPreFilter`.  This takes two predicates: one that is applied to
only the FilePath, before stat (or lstat) is called; and one that is applied
to the full file information after the stat.

## Final notes

Predicates form a Category and an Arrow, so you can use Arrow-style
composition rather than Monoids if you wish.  They also form an Applicative, a
Monad and a MonadPlus.

In the Monad, the value bound over is whatever the predicate chooses to return
(most Predicates return the same FilePath they examined, however, making the
Monad less value).  Here's an example Monad:

    start <- liftIO getCurrentTime
    find "." $ do
        -- The Predicate Monad is a short-circuiting monad, meaning we stop as
        -- soon as it can be determined that the user is not interested in a
        -- given file.  To access the current file, simply bind the result
        -- value from any Predicate.  To change the file being matched against,
        -- for whatever reason, use 'consider'.
        glob "*.hs"

        -- If the find takes longer than 5 minutes, abort.  We could have
        -- used 'timeout', but this is for illustration.
        end <- liftIO getCurrentTime
        if diffUTCTIme end start > 300
            then ignoreAll
            else matchAll                -- matchAll is "id" in this Category
