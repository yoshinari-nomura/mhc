################################################################
#
# We assume a CalDAV server as a remote side.
# CalDAV has no ability to provide such information.
# It only provides ETag mechanism, which is defined
# in HTTP protocol (see rfc2616).
#
# So, we have to maintain a ETag cache (replica) on local and
# manage difference between the cache and the remote.
# Using the ETag information:
#
# (1) get uid-etag list via PROPFIND (WebDav) method
#
#     make a set: R_SET = [(r_uid, r_etag)..]
#       r_uid:  unique id of a remote article.
#       r_etag: corresponding etag of r_uid.
#
# (2) get uid-etag list via local cache.
#
#     make a set: L_SET = [(l_uid, l_etag)..]
#
# (3) for each uid: [uid| (uid, etag) <- L_SET + R_SET]
#
#     (A) if (uid, _) is missed in L_SET
#           -> SET_MARK(uid, M)
#
#     (B) if (uid, _) is missed in R_SET
#           -> SET_MARK(uid, D)
#
#     (C) if (uid, _) exists in both R_SET and L_SET
#           if l_etag != r_etag
#             -> SET_MARK(uid, M)
#           if l_etag == r_etag
#             -> SET_MARK(uid, N)
#
module Mhc
  module Sync
    subdir = File.dirname(__FILE__) + "/sync/"
    require subdir + "driver.rb"
    require subdir + "status.rb"
    require subdir + "status_manager.rb"
    require subdir + "strategy.rb"
    require subdir + "syncinfo.rb"
  end # module  Sync
end # module Mhc
