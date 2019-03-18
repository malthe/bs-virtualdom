# Changes

In next release ...

- Fix issue where nested remove transitions would not trigger when
  appearing inside directives other than vnodes.

1.0.4 (2019-03-08)
------------------

- Use item-by-item comparison for tuples and arrays in thunks and
  components.

1.0.3 (2019-02-22)
------------------

- Added optional `onRemove` argument to virtual nodes. If provided,
  this function is responsible for removing the DOM element. The
  function receives the parent and child elements.

- Added optional `onPatch` argument to mount function which gives an
  opportunity to react to DOM changes before a paint event.

1.0.2 (2019-02-20)
------------------

- Avoid reinserting a node into the exact same location which would
  cause an input element to lose focus.

- Added browser events `AnimationEnd`, `AnimationIteration`, and
  `AnimationStart`.

1.0.1 (2019-01-13)
------------------

- Fixed brown bag release.


1.0.0 (2019-01-13)
------------------

- Initial release.
