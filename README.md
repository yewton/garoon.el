garoon.el
=========

garoon.el is a client for [Cybozu Garoon](http://products.cybozu.co.jp/garoon/).

Installation
------------

- TBD

Usage
-----

```lisp
(autoload 'grn:get-schedule-events "garoon")
```

Configuration
-------------

```lisp
(customize-group 'garoon)
```

You must customize at least `grn:endpoint-url` and `grn:username`.
