A plain String or Text is dangerous because it can be arbitrarily combined,
converted, emitted, and updated without consideration of the validity and
applicability of the contents and the usage scenario.

This module provides a Named type wrapper around Text which has two
additional type parameters: one which indicates what the content of the Text
represents, and one which indicates the style of the name.

There are additionally functions and classes which allow conversions and
transformations between names with different styles and types (or disallow them
where appropriate).

The core intent of this library is that the Named is used instead of a raw
String or Text to provide type-level tracking and safety.
