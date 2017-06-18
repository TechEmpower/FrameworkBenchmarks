(ns hello.app
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [hello.core-test]))

(doo-tests 'hello.core-test)


