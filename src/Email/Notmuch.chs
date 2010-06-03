{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}
{-
- Haskell bindings for notmuch mail indexing library
- Copyright © 2010 Ali Polatel
-
- This program is free software: you can redistribute it and/or modify
- it under the terms of the GNU General Public License as published by
- the Free Software Foundation, either version 3 of the License, or
- (at your option) any later version.
-
- This program is distributed in the hope that it will be useful,
- but WITHOUT ANY WARRANTY; without even the implied warranty of
- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
- GNU General Public License for more details.
-
- You should have received a copy of the GNU General Public License
- along with this program.  If not, see http://www.gnu.org/licenses/ .
-
- Author: Ali Polatel <alip@exherbo.org>
-}

--{{{ Notes
-- The functions below are missing from this binding but present in the notmuch library:
-- - notmuch_database_find_message()
--      Reason: Doesn't distinguish between error and not found
-- - notmuch_query_count_messages()
--      Reason: Doesn't distinguish between error and count zero
--}}}
--{{{ Exports
module Email.Notmuch (
    CommaSeparatedString,         -- = String
    Count,                        -- = Integer
    Header,                       -- = String
    Subject,                      -- = String
    Tag,                          -- = String
    Time,                         -- = Integer
    Version,                      -- = Integer

    Status(..),
    statusStr,                    -- :: Status -> IO String

    DatabaseError,
    MaildirError,
    MemoryError,
    XapianError,

    Database,                     -- = Ptr NotmuchDatabase
    DatabaseOpenMode(..),         -- = ModeReadOnly | ModeReadWrite
    databaseCreate,               -- :: FilePath -> IO Database
    databaseOpen,                 -- :: FilePath -> DatabaseOpenMode -> IO Database
    databaseClose,                -- :: Database -> IO ()
    databasePath,                 -- :: Database -> IO FilePath
    databaseVersion,              -- :: Database -> IO Version
    databaseNeedsUpgrade,         -- :: Database -> IO Bool
    databaseUpgrade,              -- :: Database -> IO Status
    databaseGetDirectory,         -- :: Database -> FilePath -> IO Directory
    databaseAddMessage,           -- :: Database -> FilePath -> IO (Status, Message)
    databaseRemoveMessage,        -- :: Database -> FilePath -> IO Status
    databaseGetAllTags,           -- :: Database -> IO Tags

    Query,                        -- = Ptr NotmuchQuery
    QueryString,                  -- = String
    QuerySort(..),
    queryCreate,                  -- :: Database -> QueryString -> IO Query
    querySetSort,                 -- :: Query -> QuerySort -> ()
    querySearchThreads,           -- :: Query -> IO Threads
    querySearchMessages,          -- :: Query -> IO Messages
    queryDestroy,                 -- :: Query -> IO ()

    Threads,                      -- = Ptr NotmuchThreads
    threadsValid,                 -- :: Threads -> IO Bool
    threadsGet,                   -- :: Threads -> IO Thread
    threadsMoveToNext,            -- :: Threads -> IO ()
    threadsDestroy,               -- :: Threads -> IO ()

    Thread,                       -- = Ptr NotmuchThread
    ThreadId,                     -- = String
    threadId,                     -- :: Thread -> IO ThreadId
    threadTotalMessages,          -- :: Thread -> IO Count
    threadTopLevelMessages,       -- :: Thread -> IO Messages
    threadMatchedMessages,        -- :: Thread -> IO Count
    threadAuthors,                -- :: Thread -> IO CommaSeparatedString
    threadSubject,                -- :: Thread -> IO String
    threadOldestDate,             -- :: Thread -> IO Time
    threadNewestDate,             -- :: Thread -> IO Time
    threadTags,                   -- :: Thread -> IO Tags
    threadDestroy,                -- :: Thread -> IO ()

    Messages,                     -- = Ptr NotmuchMessages
    messagesValid,                -- :: Messages -> IO Bool
    messagesGet,                  -- :: Messages -> IO Message
    messagesMoveToNext,           -- :: Messages -> IO ()
    messagesCollectTags,          -- :: Messages -> IO Tags
    messagesDestroy,              -- :: Messages -> IO ()

    Message,                      -- = Ptr NotmuchMessage
    MessageFlag(..),
    MessageId,                    -- = String
    messageId,                    -- :: Message -> IO MessageId
    messageThreadId,              -- :: Message -> IO ThreadId
    messageReplies,               -- :: Message -> IO Messages
    messageFileName,              -- :: Message -> IO FilePath
    messageGetFlag,               -- :: Message -> MessageFlag -> IO Bool
    messageSetFlag,               -- :: Message -> MessageFlag -> Bool -> IO ()
    messageDate,                  -- :: Message -> IO Time
    messageHeader,                -- :: Message -> Header -> IO String
    messageTags,                  -- :: Message -> IO Tags
    messageAddTag,                -- :: Message -> Tag -> IO Status
    messageRemoveTag,             -- :: Message -> Tag -> IO Status
    messageRemoveAllTags,         -- :: Message -> IO Status
    messageFreeze,                -- :: Message -> IO Status
    messageThaw,                  -- :: Message -> IO Status
    messageDestroy,               -- :: Message -> IO ()

    Tags,                         -- = Ptr NotmuchTags
    tagsValid,                    -- :: Tags -> IO Bool
    tagsGet,                      -- :: Tags -> IO Tag
    tagsMoveToNext,               -- :: Tags -> IO ()
    tagsDestroy,                  -- :: Tags -> IO ()

    Directory,                    -- = Ptr NotmuchDirectory
    directorySetMtime,            -- :: Directory -> Time -> IO Status
    directoryGetMtime,            -- :: Directory -> IO Time
    directoryGetChildFiles,       -- :: Directory -> IO FileNames
    directoryGetChildDirectories, -- :: Directory -> IO FileNames
    directoryDestroy,             -- :: Database -> IO ()

    FileNames,                    -- = Ptr NotmuchFileNames
    filenamesValid,               -- :: FileNames -> IO Bool
    filenamesGet,                 -- :: FileNames -> IO FilePath
    filenamesMoveToNext,          -- :: FileNames -> IO ()
    filenamesDestroy              -- :: FileNames -> IO ()
    ) where
--}}}
--{{{ Imports
import Control.Exception (Exception, throw)
import Data.Typeable

import Foreign
import Foreign.C.Types  (CChar, CDouble, CInt, CUInt, CLong)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr      (nullPtr, nullFunPtr)
--}}}
--{{{ Types
#include <notmuch.h>

{#context lib="notmuch" prefix="notmuch"#}

{#enum notmuch_status_t as Status {underscoreToCase}
    with prefix = "NOTMUCH_STATUS" deriving (Eq,Show)#}
{#enum notmuch_database_mode_t as DatabaseOpenMode {underscoreToCase}
    with prefix = "NOTMUCH_DATABASE" deriving (Eq,Show)#}
{#enum notmuch_sort_t as QuerySort {underscoreToCase}
    with prefix = "NOTMUCH_SORT" deriving (Eq,Show)#}
{#enum notmuch_message_flag_t as MessageFlag {underscoreToCase}
    with prefix = "NOTMUCH_MESSAGE" deriving (Eq, Show)#}

{#pointer * notmuch_database_t as NotmuchDatabase foreign newtype#}
{#pointer * notmuch_query_t as NotmuchQuery foreign newtype#}
{#pointer * notmuch_threads_t as NotmuchThreads foreign newtype#}
{#pointer * notmuch_thread_t as NotmuchThread foreign newtype#}
{#pointer * notmuch_messages_t as NotmuchMessages foreign newtype#}
{#pointer * notmuch_message_t as NotmuchMessage foreign newtype#}
{#pointer * notmuch_tags_t as NotmuchTags foreign newtype#}
{#pointer * notmuch_directory_t as NotmuchDirectory foreign newtype#}
{#pointer * notmuch_filenames_t as NotmuchFileNames foreign newtype#}
type CommaSeparatedString   = String
type Count                  = Integer
type Header                 = String
type Subject                = String
type Tag                    = String
type Time                   = Integer
type Version                = Integer
type QueryString            = String
type ThreadId               = String
type MessageId              = String
type Database               = Ptr NotmuchDatabase
type Query                  = Ptr NotmuchQuery
type Threads                = Ptr NotmuchThreads
type Thread                 = Ptr NotmuchThread
type Messages               = Ptr NotmuchMessages
type Message                = Ptr NotmuchMessage
type Tags                   = Ptr NotmuchTags
type Directory              = Ptr NotmuchDirectory
type FileNames              = Ptr NotmuchFileNames
data DatabaseError          = DatabaseError deriving(Show,Typeable)
data MaildirError           = MaildirError  deriving(Show,Typeable)
data MemoryError            = MemoryError   deriving(Show,Typeable)
data XapianError            = XapianError   deriving(Show,Typeable)
instance Exception DatabaseError
instance Exception MaildirError
instance Exception MemoryError
instance Exception XapianError
--}}}
--{{{ Local Functions
cFromEnum :: (Enum a, Integral b) => a -> b
cFromEnum = fromIntegral . fromEnum
--}}}
--{{{ Status
-- |Get a string representation of the 'Status' value.
statusStr :: Status -> IO String
statusStr = peekCString . {#call pure unsafe status_to_string#} . cFromEnum
--}}}
--{{{ Database
{-|
    Create a new, empty notmuch database located at 'FilePath'.

    The path should be a top-level directory to a collection of plain-text
    email messages (one message per file). This call will create a new
    \".notmuch\" directory within 'FilePath' where notmuch will store its data.

    After a successful call to 'databaseCreate', the returned database will be
    open so the caller should call 'databaseClose' when finished with
    it.

    The database will not yet have any data in it ('databaseCreate' itself is a
    very cheap function). Messages contained within 'FilePath' can be added to
    the database by calling 'databaseAddMessage'.

    In case of any failure, this function throws 'DatabaseError', (after
    printing an error message on stderr).
-}
databaseCreate :: FilePath -> IO Database
databaseCreate p = withCString p {#call unsafe database_create#} >>=
                   (\db -> if db == nullPtr then throw DatabaseError else return db)

{-|
    Open an existing notmuch database located at 'FilePath'.

    The database should have been created at some time in the past, (not
    necessarily by this process), by calling 'databaseCreate' with 'FilePath'.
    By default the database should be opened for reading only. In order to
    write to the database you need to pass the 'ModeReadWrite' mode.

    An existing notmuch database can be identified by the presence of a
    directory named \".notmuch\" below 'FilePath'.

    The caller should call 'databaseClose' when finished with this database.

    In case of any failure, this function throws 'DatabaseError', (after
    printing an error message on stderr).
-}
databaseOpen :: FilePath -> DatabaseOpenMode -> IO Database
databaseOpen p m = withCString p (flip {#call unsafe database_open#} (cFromEnum m)) >>=
                   (\db -> if db == nullPtr then throw DatabaseError else return db)

{-|
    Close the given notmuch database, freeing all associated resources.

    See: 'databaseOpen'
-}
databaseClose :: Database -> IO ()
databaseClose = {#call unsafe database_close#}

-- |Return the database path of the given database.
databasePath :: Database -> IO FilePath
databasePath d = peekCString =<< {#call unsafe database_get_path#} d

-- |Return the database format version of the given database.
databaseVersion :: Database -> IO Version
databaseVersion d = do
                    v <- {#call unsafe database_get_version#} d
                    return $ fromIntegral v

{-|
    Does this database need to be upgraded before writing to it?

    If this function returns 'True' then no functions that modify the database
    ('databaseAddMessage', 'messageAddTag', 'directorySetMtime', etc.) will
    work unless the function 'databaseUpgrade' is called successfully first.
-}
databaseNeedsUpgrade :: Database -> IO Bool
databaseNeedsUpgrade d = do
                         b <- {#call unsafe database_needs_upgrade#} d
                         return $ b /= 0

{-|
    Upgrade the current database.

    After opening a database in read-write mode, the client should check if an
    upgrade is needed ('databaseNeedsUpgrade') and if so, upgrade with this
    function before making any modifications.
-}
databaseUpgrade :: Database -> IO Status
databaseUpgrade d = do
                    s <- {#call unsafe database_upgrade#} d nullFunPtr nullPtr
                    return $ toEnum $ fromIntegral s

{-|
    Retrieve a directory object from the database for 'FilePath'.

    Here, 'FilePath' should be a path relative to the path of 'Database'
    (see 'databasePath'), or else should be an absolute path
    with initial components that match the path of 'Database'.

    Throws 'XapianError' if a Xapian exception occurs.
-}
databaseGetDirectory :: Database -> FilePath -> IO Directory
databaseGetDirectory d p = withCString p ({#call unsafe database_get_directory#} d) >>=
                           (\dir -> if dir == nullPtr then throw XapianError else return dir)

{-|
    Add a new message to the given notmuch database.

    Here, 'FilePath' should be a path relative to the path of 'Database'
    (see 'databasePath'), or else should be an absolute filename with initial
    components that match the path of 'Database'.

    The file should be a single mail message (not a multi-message mbox) that is
    expected to remain at its current location, (since the notmuch database
    will reference the filename, and will not copy the entire contents of the
    file.

    On successful return 'Message' will be initialized to a message type that
    can be used for things such as adding tags to the just-added message. The
    user should call 'messageDestroy' when done with the message. On any
    failure 'Message' will be set to 'nullPtr'.
-}
databaseAddMessage :: Database -> FilePath -> IO (Status, Message)
databaseAddMessage d p = withCString p (\p' -> alloca $ \ptr -> do
                         s <- {#call unsafe database_add_message#} d p' ptr
                         m <- peek ptr
                         return (toEnum $ fromIntegral s, m))

{-|
    Remove a message from the given notmuch database.

    Note that only this particular filename association is removed from the
    database. If the same message (as determined by the message ID) is still
    available via other filenames, then the message will persist in the
    database for those filenames. When the last filename is removed for a
    particular message, the database content for that message will be entirely
    removed.
-}
databaseRemoveMessage :: Database -> FilePath -> IO Status
databaseRemoveMessage d p = do
                            s <- withCString p ({#call database_remove_message#} d)
                            return $ toEnum $ fromIntegral s

{-|
    Return a list of all tags found in the database.

    This function creates a list of all tags found in the database. The
    resulting list contains all tags from all messages found in the database.

    On error this function throws 'XapianError'.
-}
databaseGetAllTags :: Database -> IO Tags
databaseGetAllTags d = {#call unsafe database_get_all_tags#} d >>=
                       (\ts -> if ts == nullPtr then throw XapianError else return ts)
--}}}
--{{{ Query
{-|
    Create a new query for 'Database'.

    See 'querySetSort' for controlling the order of results.

    See 'querySearchMessages' and querySearchThreads to actually execute the
    query.

    User should call queryDestroy when finished with this query.

    Will throw 'MemoryError' if insufficient memory is available.
-}
queryCreate :: Database -> QueryString -> IO Query
queryCreate d s = withCString s ({#call unsafe query_create#} d) >>=
                  (\q -> if q == nullPtr then throw MemoryError else return q)

-- |Specify the sorting desired for this query.
querySetSort :: Query -> QuerySort -> IO ()
querySetSort q s = {#call unsafe query_set_sort#} q (cFromEnum s)

{-|
    Execute a query for threads, returning a 'Threads' type which can be used
    to iterate over the results. The returned threads object is owned by the
    query and as such, will only be valid until 'queryDestroy'.
-}
querySearchThreads :: Query -> IO Threads
querySearchThreads = {#call unsafe query_search_threads#}

{-|
    Execute a query for messages, returning a 'Messages' type which can be used
    to iterate over the results. The returned messages object is owned by the
    query and as such, will only be valid until 'queryDestroy'.
-}
querySearchMessages :: Query -> IO Messages
querySearchMessages = {#call unsafe query_search_messages#}

{-|
    Destroy a notmuch_query_t along with any associated resources.

    This will in turn destroy any 'Threads' and 'Messages' generated by this
    query, (and in turn any 'Thread' and 'Message' objects generated from those
    results, etc.), if such objects haven't already been destroyed.
-}
queryDestroy :: Query -> IO ()
queryDestroy = {#call unsafe query_destroy#}
--}}}
--{{{ Threads
{-|
    Is the given 'threads' iterator pointing at a valid thread?

    When this function returns 'True', 'threadsGet' will return a valid
    'Thread'. Whereas when this function returns 'False', 'threadsGet' will
    return 'nullPtr'.
-}
threadsValid :: Threads -> IO Bool
threadsValid ts = do
                  b <- {#call unsafe threads_valid#} ts
                  return $ b /= 0

{-|
    Get the current thread from 'Threads' as a 'Thread'.

    Note: The returned thread belongs to 'Threads' and has a lifetime
    identical to it (and the 'Query' to which it belongs).

    If an out-of-memory situation occurs, this function throws 'MemoryError'.
-}
threadsGet :: Threads -> IO Thread
threadsGet ts = {#call unsafe threads_get#} ts >>=
                (\t -> if t == nullPtr then throw MemoryError else return t)

{-|
    Move the 'Threads' iterator to the next thread.

    If 'Threads' is already pointing at the last thread then the iterator will
    be moved to a point just beyond that last thread,
    (where 'threadsValid' will return 'False').
-}
threadsMoveToNext :: Threads -> IO ()
threadsMoveToNext = {#call unsafe threads_move_to_next#}

{-|
    Destroy a 'Threads' type.

    It's not strictly necessary to call this function. All memory from the
    'Threads' will be reclaimed when the containing 'Query' object is destroyed.
-}
threadsDestroy :: Threads -> IO ()
threadsDestroy = {#call unsafe threads_destroy#}
--}}}
--{{{ Thread
-- |Get the thread ID of 'Thread'.
threadId :: Thread -> IO ThreadId
threadId t = peekCString =<< {#call unsafe thread_get_thread_id#} t

{-|
    Get the total number of messages in 'Thread'.

    This count consists of all messages in the database belonging to
    this thread. Contrast with 'threadMatchedMessages'.
-}
threadTotalMessages :: Thread -> IO Count
threadTotalMessages t = do
                        s <- {#call unsafe thread_get_total_messages#} t
                        return $ fromIntegral s

{-|
   Get a 'Messages' iterator for the top-level messages in 'Thread'.

    This iterator will not necessarily iterate over all of the messages in the
    thread. It will only iterate over the messages in the thread which are not
    replies to other messages in the thread.

    To iterate over all messages in the thread, the caller will need to iterate
    over the result of 'messageReplies' for each top-level message (and do that
    recursively for the resulting messages, etc.).
-}
threadTopLevelMessages :: Thread -> IO Messages
threadTopLevelMessages = {#call unsafe thread_get_toplevel_messages#}

{-|
    Get the number of messages in 'Thread' that matched the search.

    This count includes only the messages in this thread that were matched by
    the search from which the thread was created. Contrast with 'threadTotalMessages'.
-}
threadMatchedMessages :: Thread -> IO Count
threadMatchedMessages t = do
                          c <- {#call unsafe thread_get_matched_messages#} t
                          return $ fromIntegral c

{-|
    Get the authors of 'Thread'

    The returned string is a comma-separated list of the names of the authors
    of mail messages in the query results that belong to this thread.
-}
threadAuthors :: Thread -> IO CommaSeparatedString
threadAuthors t = (\auth -> if auth == nullPtr then return "" else peekCString auth) =<<
                  {#call unsafe thread_get_authors#} t

{-|
    Get the subject of 'Thread'

    The subject is taken from the first message (according to the query
    order---see 'querySetSort') in the query results that belongs to this
    thread.
-}
threadSubject :: Thread -> IO Subject
threadSubject t = (\subj -> if subj == nullPtr then return "" else peekCString subj) =<<
                  {#call unsafe thread_get_subject#} t

-- |Get the date of the oldest message in 'Thread' as a 'Time' value.
threadOldestDate :: Thread -> IO Time
threadOldestDate t = do
                     d <- {#call unsafe thread_get_oldest_date#} t
                     return $ fromIntegral d

-- |Get the date of the newest message in 'Thread' as a 'Time' value.
threadNewestDate :: Thread -> IO Time
threadNewestDate t = do
                     d <- {#call unsafe thread_get_newest_date#} t
                     return $ fromIntegral d

{-|
    Get the tags for 'Thread', returning a 'Tags' which can be used to iterate
    over all tags.

    Note: In the Notmuch database, tags are stored on individual messages, not
    on threads. So the tags returned here will be all tags of the messages
    which matched the search and which belong to this thread.

    The 'Tags' is owned by the thread and as such, will only be valid for as
    long as the thread is valid, (for example, until 'threadDestroy' or until
    the query from which it derived is destroyed).
-}
threadTags :: Thread -> IO Tags
threadTags = {#call unsafe thread_get_tags#}

-- |Destroy a 'Thread'.
threadDestroy :: Thread -> IO ()
threadDestroy = {#call unsafe thread_destroy#}
--}}}
--{{{ Messages
-- |Is the given 'Messages' iterator pointing at a valid message.
messagesValid :: Messages -> IO Bool
messagesValid ms = do
                   v <- {#call unsafe messages_valid#} ms
                   return $ v /= 0

{-|
    Get the current message from 'Messages' as a 'Message'.

    Note: The returned message belongs to 'Messages' and has a lifetime
    identical to it (and the query to which it belongs).

    If an out-of-memory situation occurs, this function throws 'MemoryError'.
-}
messagesGet :: Messages -> IO Message
messagesGet ms = {#call unsafe messages_get#} ms >>=
                 (\m -> if m == nullPtr then throw MemoryError else return m)

-- |Move the 'Messages' iterator to the next message.
messagesMoveToNext :: Messages -> IO ()
messagesMoveToNext = {#call unsafe messages_move_to_next#}

{-|
    Return a list of tags from all messages.

    The resulting list is guaranteed not to contain duplicated tags.

    WARNING: You can no longer iterate over messages after calling this
    function, because the iterator will point at the end of the list.
    We do not have a function to reset the iterator yet and the only
    way how you can iterate over the list again is to recreate the
    message list.

    The function throws 'MemoryError' on error.
-}
messagesCollectTags :: Messages -> IO Tags
messagesCollectTags ms = {#call unsafe messages_collect_tags#} ms >>=
                         (\ts -> if ts == nullPtr then throw MemoryError else return ts)

-- |Destroy the 'Messages'
messagesDestroy :: Messages -> IO ()
messagesDestroy = {#call unsafe messages_destroy#}
--}}}
--{{{ Message
-- |Get the message ID of 'Message'.
messageId :: Message -> IO MessageId
messageId m = peekCString =<< {#call unsafe message_get_message_id#} m

-- |Get the thread ID of 'Message'.
messageThreadId :: Message -> IO ThreadId
messageThreadId m = peekCString =<< {#call unsafe message_get_thread_id#} m

{-|
    Get a 'Messages' iterator for all of the replies to 'Message'.

    Note: This call only makes sense if 'Message' was ultimately obtained from
    a 'Thread', (such as by coming directly from the result of calling
    'threadTopLevelMessages' or by any number of subsequent calls to
    'messageGetReplies').

    If 'Message' was obtained through some non-thread means, (such as by a call
    to 'querySearchMessages'), then this function will return 'nullPtr'.

    If there are no replies to 'Message', this function will return 'nullPtr'.
    (Note that messagesValid will accept that 'nullPtr' value as legitimate,
    and simply return 'False' for it.)
-}
messageReplies :: Message -> IO Messages
messageReplies = {#call unsafe message_get_replies#}

{-|
    Get a filename for the email corresponding to 'message'.

    The returned filename is an absolute filename, (the initial
    component will match 'databasePath').

    Note: If this message corresponds to multiple files in the mail store,
    (that is, multiple files contain identical message IDs), this function will
    arbitrarily return a single one of those filenames.
-}
messageFileName :: Message -> IO FilePath
messageFileName m = peekCString =<< {#call unsafe message_get_filename#} m

-- |Get a value of a 'MessageFlag' for the email corresponding to 'Message'
messageGetFlag :: Message -> MessageFlag -> IO Bool
messageGetFlag m f = do
    v <- {#call unsafe message_get_flag#} m (cFromEnum f)
    return $ v /= 0

-- |Set a value of a 'MessageFlag' for the email corresponding to 'Message'
messageSetFlag :: Message -> MessageFlag -> Bool -> IO ()
messageSetFlag m f True  = {#call unsafe message_set_flag#} m (cFromEnum f) 1
messageSetFlag m f False = {#call unsafe message_set_flag#} m (cFromEnum f) 0

{-|
    Get the date of 'Message' as a 'Time' value.

    For the original textual representation of the Date header from the message
    call 'messageHeader' with a header value of \"date\".
-}
messageDate :: Message -> IO Time
messageDate m = do
                t <- {#call unsafe message_get_date#} m
                return $ fromIntegral t

{-|
    Get the value of the specified header from 'Message'.

    The value will be read from the actual message file, not from the
    notmuch database. The header name is case insensitive.

    Returns an empty string (\"\") if the message does not contain a
    header line matching 'Header'.

    Throws 'MaildirError' if any error occurs.
-}
messageHeader :: Message -> Header -> IO String
messageHeader m h = withCString h ({#call unsafe message_get_header#} m) >>=
                    (\v -> if v == nullPtr then throw MaildirError else peekCString v)

{-|
    Get the tags for 'Message', returning a 'Tags' which can be used to iterate
    over all tags.

    The 'Tags' is owned by the 'Message' and as such, will only be valid for as
    long as the 'Message' is valid, (which is until the query from which it
    derived is destroyed).
-}
messageTags :: Message -> IO Tags
messageTags = {#call unsafe message_get_tags#}

-- |Add a tag to the given message.
messageAddTag :: Message -> Tag -> IO Status
messageAddTag m t = do
                    s <- withCString t ({#call unsafe message_add_tag#} m)
                    return $ toEnum $ fromIntegral s

-- |Remove a tag from the given message.
messageRemoveTag :: Message -> Tag -> IO Status
messageRemoveTag m t = do
                       s <- withCString t ({#call unsafe message_remove_tag#} m)
                       return $ toEnum $ fromIntegral s

-- |Remove all tags from the given message.
messageRemoveAllTags :: Message -> IO Status
messageRemoveAllTags m = do
                         s <- {#call unsafe message_remove_all_tags#} m
                         return $ toEnum $ fromIntegral s
{-|
    Freeze the current state of 'Message' within the 'Database'.

    This means that changes to the message state, (via 'messageAddTag',
    'messageRemoveTag', and 'messageRemoveAllTags'), will not be committed to
    the database until the message is thawed with 'messageThaw'.

    Multiple calls to freeze/thaw are valid and these calls will
    "stack". That is there must be as many calls to thaw as to freeze
    before a message is actually thawed.
-}
messageFreeze :: Message -> IO Status
messageFreeze m = do
                  s <- {#call unsafe message_freeze#} m
                  return $ toEnum $ fromIntegral s

{-|
    Thaw the current 'Message', synchronizing any changes that may have
    occurred while 'Message' was frozen into the notmuch database.
-}
messageThaw :: Message -> IO Status
messageThaw m = do
                s <- {#call unsafe message_thaw#} m
                return $ toEnum $ fromIntegral s

-- |Destroy a 'Message'
messageDestroy :: Message -> IO ()
messageDestroy = {#call unsafe message_destroy#}
--}}}
--{{{ Tags
-- |Is the given 'Tags' iterator pointing at a valid tag.
tagsValid :: Tags -> IO Bool
tagsValid ts = do
               v <- {#call unsafe tags_valid#} ts
               return $ v /= 0

{-|
    Get the current tag from 'Tags' as a 'Tag'.
-}
tagsGet :: Tags -> IO Tag
tagsGet ts = peekCString =<< {#call unsafe tags_get#} ts

-- |Move the 'Tags' iterator to the next tag.
tagsMoveToNext :: Tags -> IO ()
tagsMoveToNext = {#call unsafe tags_move_to_next#}

-- |Destroy 'Tags'
tagsDestroy :: Tags -> IO ()
tagsDestroy = {#call unsafe tags_destroy#}
--}}}
--{{{ Directory
{-|
    Store an mtime within the database for 'Directory'.

    The intention is for the caller to use the mtime to allow efficient
    identification of new messages to be added to the database. The
    recommended usage is as follows:

        - Read the mtime of a directory from the filesystem

        - Call 'databaseAddMessage' for all mail files in the directory

        - Call 'directorySetMtime' with the mtime read from the
          filesystem.

    Then, when wanting to check for updates to the directory in the
    future, the client can call 'directoryGetMtime' and know
    that it only needs to add files if the mtime of the directory and
    files are newer than the stored timestamp.

    Note: The 'directoryGetMtime' function does not allow the
    caller to distinguish a timestamp of 0 from a non-existent
    timestamp. So don't store a timestamp of 0 unless you are
    comfortable with that.
-}
directorySetMtime :: Directory -> Time -> IO Status
directorySetMtime d t = do
                        s <- {#call unsafe directory_set_mtime#} d (cFromEnum t)
                        return $ toEnum $ fromIntegral s

{-|
    Get the mtime of a directory, (as previously stored with
    'directorySetMtime').

    Returns 0 if no mtime has previously been stored for this directory.
-}
directoryGetMtime :: Directory -> IO Time
directoryGetMtime d = do
                      t <- {#call unsafe directory_get_mtime#} d
                      return $ fromIntegral t

{-|
    Get a 'FileNames' iterator listing all the filenames of messages in the
    database within the given directory.

    The returned filenames will be the basename-entries only
    (not complete paths).
-}
directoryGetChildFiles :: Directory -> IO FileNames
directoryGetChildFiles = {#call unsafe directory_get_child_files#}

{-|
    Get a 'FileNames' iterator listing all the filenames of sub-directories in
    the database within the given directory.

    The returned filenames will be the basename-entries only
    (not complete paths).
-}
directoryGetChildDirectories :: Directory -> IO FileNames
directoryGetChildDirectories = {#call unsafe directory_get_child_directories#}

-- |Destroy a 'Directory'.
directoryDestroy :: Directory -> IO ()
directoryDestroy = {#call unsafe directory_destroy#}
--}}}
--{{{ FileNames
-- |Is the given 'Filenames' iterator pointing at a valid filename.
filenamesValid :: FileNames -> IO Bool
filenamesValid f = do
                   v <- {#call unsafe filenames_valid#} f
                   return $ v /= 0

-- |Get the current filename from 'Filenames' as a 'FilePath'.
filenamesGet :: FileNames -> IO FilePath
filenamesGet f = peekCString =<< {#call unsafe filenames_get#} f

-- |Move the 'Filenames' iterator to the next filename.
filenamesMoveToNext :: FileNames -> IO ()
filenamesMoveToNext = {#call unsafe filenames_move_to_next#}

-- |Destroy 'FileNames'
filenamesDestroy :: FileNames -> IO ()
filenamesDestroy = {#call unsafe filenames_destroy#}
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
