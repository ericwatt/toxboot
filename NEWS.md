# toxboot 0.2.0

* Changed all mongoDB functions to use `mongolite` rather than `rmongodb` now that `rmongodb` has been archived.
* Renamed mongoDB field `boot_type` to `boot_method` so that is is consistently named in all functions and outputs.

# toxboot 0.1.1

* Moved RMySQL, DBI, and rmongodb from Imports to Suggests (#1, #2)

# toxboot 0.1.0

* Initial public release
