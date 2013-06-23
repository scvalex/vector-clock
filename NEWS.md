News
====

v0.2.2
------

Minor release fixing compatibility issues with older versions of GHC.

v0.2.1
------

Minor release fixing a compatibility issue with the latest `hashable`.

v0.2.0
------

Minor release introducing a new vector clock type:

  - add `Data.VectorClock.Approximate`, an implementation of
    approximate vector clocks

v0.1.2
------

Maintenance release for `v0.1.1`:

  - fix `VectorClock.relation`
  - add usage examples
  - more tests


v0.1.1
------

Maintenance release for `v0.1.0`:

  - export `Relation` so that the library is actually usable
  - more tests
  - fix a number of bugs uncovered by the tests

v0.1.0
------

Initial release:

  - naive implementation of vector clock implementation, as described
    in "Fundamentals of Distributed Computing: A Practical Tour of
    Vector Clock Systems" by Roberto Baldoni and Michel Raynal.
